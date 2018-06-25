###Europepmc
#https://cran.r-project.org/web/packages/europepmc/europepmc.pdf
###Allows you to search for citations between pubmed central and medline only
###SWEEEET
#https://ropensci.org/tutorials/europepmc_tutorial/

## libraries ---------------------------------------------------------------
#Get the most recent version of europepmc from github
# devtools::install_github("ropensci/europepmc", threads = parallel::detectCores())
library(europepmc)
library(tidyverse)
library(humaniformat)
library(memoise)
library(fs)
library(readxl)
library(rlang)
library(furrr)
library(futile.logger)
not_null <- negate(is_null)
`%<>%` <- magrittr::`%<>%`

## setup -------------------------------------------------------------------
# (GM) Set path to data and filenames as "constants" and use CAPS to denote.
DATA_DIR <- path("~/data/")
WORKING_DIR <- path("~/Dropbox/Pubmedsearch/scraper/6.22.2018/") 
# escape sequences for spaces in path
SCRIPT_DIR <- path(WORKING_DIR, "europepmc")
GOBA_FILE <- "GOBA_divorce_study.xlsx"
DOX_FILE <- path(DATA_DIR, "DoximityData-full.csv")

# (GM) cacheing (sp?) via `memoise()` allows you to reuse searches so you 
# aren't bothering the europepmc server for something you've already asked for.
flog.info("Memoising the EPMC search and citaion functions.")
fc <- cache_filesystem(path(DATA_DIR, ".rcache"))          # cache location
epmc_search <- memoise(epmc_search, cache = fc)            # cache results
epmc_citations <- memoise(epmc_citations, cache = fc)      # cache results

# (GM) helper functions so you don't have to retype arguments
# `safely()` is for defensive purposes: if there is an error on any given query
# it will note the error but continue to loop.
get_sorted_search <- partial(epmc_search,
                             limit = 1e3,
                             verbose = T,
                             sort = "cited") %>% safely

get_med_citations <- partial(epmc_citations,
                             # data_src = "med",  # (GM) med is default
                             limit = 1e4,
                             verbose = T) %>% safely

## Read in authors ---------------------------------------------------------
#sheetname <- readxl::excel_sheets("Dropbox/scraper/Version from 6.22.2018/DoximityData-full.csv")
(sheetname <- readxl::excel_sheets(path(DATA_DIR, "GOBA_divorce_study.xlsx")))


data_types <- c("text", "numeric", "text", "text", "text",
                "text", "numeric", "text", "text", "numeric",
                "text", "text", "text", "guess", "numeric",
                "text", "text", "text", "text")
flog.info("Reading in the GOBA file.")
goba <- readxl::read_xlsx(path(DATA_DIR, GOBA_FILE),
                             sheet = sheetname,
                             col_types = data_types)
flog.info("Reading in the DOXIMITY file.")
dox <- data.table::fread(DOX_FILE, nThread = 2, header = T)
dox %<>% mutate_at(vars(ID, NPI), as.character)

doctor_names <- humaniformat::parse_names(goba$GOBA_Full.Name)
names(doctor_names)
goba %<>% bind_cols(doctor_names)
names(goba)

## Reformat names for query ------------------------------------------------
flog.info("Creating query from author names.")
goba %<>%
  # (GM) last + first
  mutate(last_first = str_c(last_name, " ", first_name) %>%
           tolower() %>%                   # (GM) convert to lowercase
           str_replace(., "\\.", "")) %>%  # (GM) remove period
  # (GM) last + first + m (if middle name exists)
  mutate(last_first_m = str_c(last_first, " ", str_sub(middle_name, 1, 1) %|% "") %>%
           tolower() %>%                   # (GM) convert to lowercase
           str_replace(., "\\.", "") %>%   # (GM) remove period
           str_trim()) %>%                 # (GM) remove leading/trailing whitespace
  # (GM) last + first + m + suffix (if suffix exists)
  mutate(last_first_m_suffix = str_c(
      `ifelse`(last_first == last_first_m, last_first, last_first_m),
      " ",
      suffix %|% "") %>%
      tolower() %>%                         # (GM) convert to lowercase
      str_replace(., "\\.", "") %>%         # (GM) remove period
      str_trim()) %>%                       # (GM) remove leading/trailing whitespace
  # (GM) surround name as AUTH:"name"
  mutate(query = str_c('AUTH:\"', last_first, '\"')) %>%
  # (GM) add OR statment if last + first + m exists
  mutate(query = str_c(query,
                       `ifelse`(
                         last_first == last_first_m,
                         "",
                         str_c(' OR AUTH:\"', last_first_m, '\"')
                       ))) %>%
  # (GM) add OR statment if last + first + m + suffix exists
  mutate(query = str_c(
    query,
    `ifelse`(
      last_first_m_suffix == last_first_m,
      "",
      str_c(' OR AUTH:\"', last_first_m_suffix, '\"')
    )
  ) %>% 
    str_trim() %>%     # (GM) remove trailing/leading whitespace
    str_squish()) %>%  # (GM) remove consecutive whitespace
  # (GM) create last + fm name
  mutate(last_fm = str_c(last_name, " ", str_sub(first_name, 1, 1), 
                         str_sub(middle_name, 1, 1) %|% "") %>% tolower()) %>% 
  # (GM) add OR statement for last + fm name
  mutate(query = str_c(query, ' OR AUTH:\"', last_fm, '\"')) %>% 
  # (GM) add keywords, affiliations, and language to query
  mutate(query = str_c(
    "(", query,
    ") AND (KW:female OR KW:obstetrics OR KW:gynecology) AND (LANG:eng) AND (AFF:obstetrics OR AFF:gynecology OR AFF:gynecologic OR AFF:family OR AFF:maternal OR AFF:Female OR AFF:Pelvic OR AFF:Reproductive)"  
  )) %>% 
  # (GM) publication date range to query if graduation date exists
  mutate(query = `ifelse`(
    !is.na(`MERGED medical school graduation year End_Year`),
    str_c( query,
           " AND (PUB_YEAR:[",
           as.character(`MERGED medical school graduation year End_Year`),
           " TO ",
           as.character(lubridate::year(Sys.Date())),
           "])"),
    query) %>% str_trim() %>% str_squish())

## (GM) get results
## NB: This step is time intensive!
flog.warn("Asynchronously retrieving query results.")
plan(multiprocess)
goba %<>%
  nest(query) %>%
  mutate(
    search = data %>%
      future_map(get_sorted_search, .progress = TRUE) %>%
      map("result") %>%
      map_if(not_null, ~ select(.x, everything())) %>%
      set_names(full_name)) 

## (GM) NULL query results
goba %>% filter(map_lgl(search, is_null)) 

## (GM) populated query results
flog.info("Getting citation counts.")
citations <- goba %>%
  select(full_name, `NPI Number`, search) %>%
  filter(map_lgl(search, not_null)) %>%
  unnest() %>%
  right_join(goba %>% select(full_name, `NPI Number`)) %>% 
  count(full_name, sort = T)  # (GM) total citation count by author
  # arrange(full_name, desc(citedByCount)) %>% View()  # (GM) by title by author
  # summarise(cite_cnt = sum(citedByCount, na.rm = T))  # (GM) all citations

## h-index -----------------------------------------------------------------
flog.info("Getting H-index.")
h_index <- goba %>%
  select(full_name, `NPI Number`, search) %>%
  filter(map_lgl(search, not_null)) %>%
  unnest() %>%
  right_join(goba %>% select(full_name, `NPI Number`)) %>% 
  arrange(full_name, desc(citedByCount)) %>% 
  group_by(full_name) %>% 
  mutate(citationIndex = row_number()) %>%
  filter(citationIndex <= citedByCount) %>% 
  summarise(h_index = max(citationIndex)) %>%
  arrange(desc(h_index))

## m-quotient --------------------------------------------------------------
flog.info("Getting M-quotient.")
m_quotient <- goba %>% 
  select(full_name, `NPI Number`, search) %>%
  filter(map_lgl(search, not_null)) %>%
  unnest() %>%
  right_join(goba %>% select(full_name, `NPI Number`)) %>% 
  group_by(full_name) %>% 
  mutate(pubYear = lubridate::year(firstPublicationDate)) %>%
  summarise(pubEra = max(pubYear, na.rm = T) - min(pubYear, na.rm = T)) %>% 
  inner_join(h_index) %>%
  mutate(m_quotient = h_index / pubEra) %>% 
  select(-h_index, -pubEra)

## g-index -----------------------------------------------------------------
flog.info("Getting G-index.")
g_index <- 
  goba %>%
  select(full_name, `NPI Number`, search) %>%
  filter(map_lgl(search, not_null)) %>%
  unnest() %>%
  right_join(goba %>% select(full_name, `NPI Number`)) %>% 
  arrange(full_name, desc(citedByCount)) %>% 
  group_by(full_name) %>% 
  mutate(citationIndex = row_number(),
         square = citationIndex^2,
         sums = cumsum(citedByCount)) %>%
  filter(square < sums) %>% 
  summarise(g_index = max(citationIndex)) %>%
  arrange(desc(g_index))


# combine -----------------------------------------------------------------
flog.info("Combining datasets and bibliometric indices.")
combined <- goba %>%
  left_join({
    list(citations, g_index, h_index, m_quotient) %>%
      reduce(inner_join)
  }) %>% left_join(dox, by = c("NPI Number" = "NPI"))

## (GM) write compressed results
flog.info("Writing combined data to %s", path(DATA_DIR, "combined.rds.bz"))
write_rds(combined, path(DATA_DIR, "combined.rds.bz"), compress = "bz")

# (GM) NB: The data is currently aggregated at the author level.
# To access a lower level of data aggregation use `unnest()` on the 
# `search` list-column after filtering out cases where `search` is null:
dim(combined )
combined %>% 
  filter(map_lgl(search, not_null)) %>%
  unnest(search) %>%
  select(id:bookid) %>% 
  names

## (GM) write to csv
flog.info("Writing combined data to %s", path(DATA_DIR, "combined_unnested.csv"))
combined %>% 
  filter(map_lgl(search, not_null)) %>%
  unnest(search) %>%
  write_csv(path(DATA_DIR, "combined_unnested.csv"))