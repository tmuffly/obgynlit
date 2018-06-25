## utilities ---------------------------------------------------------------
library(tidyverse)
library(fs)
library(futile.logger)
not_null <- negate(is_null)
`%<>%` <- magrittr::`%<>%`

## read data ---------------------------------------------------------------
#DATA_DIR <- path("/media", "garrett", "ExtraDrive1", "data", "europepmc")
#setwd("~/Dropbox/Pubmedsearch/Scraper/Version from 6.22.2018")

DATA_DIR <- path("~/Dropbox/Pubmedsearch/Scraper/Version from 6.22.2018")

# flog.info("Reading in %s", path(DATA_DIR, "combined.rds.bz"))
# combined <- read_rds(path(DATA_DIR, "combined.rds.bz"))

flog.info("Reading in %s", path(DATA_DIR, "combined_unnested.csv"))
combined <- data.table::fread(path(DATA_DIR, "combined_unnested.csv"),
                              header = T,
                              nThread = 4)

dim(combined)
str(combined %>% ungroup %>% select(id:bookid))

combined %<>% rename(pubCount = n)

## I-10 index --------------------------------------------------------------
flog.info("Getting I-10 index.")
combined %<>%
  group_by(full_name, `NPI Number`) %>%
  mutate(I10_index = sum(citedByCount >= 10))

## hc-index ----------------------------------------------------------------
flog.info("Getting HC-index.")
combined %<>%
  group_by(full_name, `NPI Number`) %>%
  mutate(hc_index = citedByCount * 4 / (2018L - pubYear))

## hi-norm -----------------------------------------------------------------
# get author count for hi-norm
flog.info("Getting author count.")
combined %<>%
  group_by(full_name, `NPI Number`) %>%
  mutate(authorCount = str_count(authorString, ",") + 1L)

# get max pubYear
# get min pubYear
flog.info("Getting min and max publication years.")
combined %<>%
  group_by(full_name, `NPI Number`) %>%
  mutate(maxPubYear = max(pubYear),
         minPubYear = min(pubYear))

# hi-norm
flog.info("Getting HI-norm.")
combined %<>%
  group_by(full_name, `NPI Number`) %>%
  mutate(hi_norm = citedByCount / authorCount / (maxPubYear - minPubYear))

## hi-annual ---------------------------------------------------------------
flog.info("Writing results to %s", path(DATA_DIR, "data.feather"))
combined %>%
  ungroup %>%
  group_by(full_name, `NPI Number`, pmid) %>%
  select(minPubYear, maxPubYear, pubYear, pmid, citedByCount) %>%
  feather::write_feather(path(DATA_DIR, "data.feather"))

## create h_indices via python
flog.warn("Setting up H-indices data structure via python.")
# system("~/path/to/my/python h_indices.py")
system("~/anaconda3/envs/myenv/bin/python h_indices.py")

flog.info("Reading in python results from %s", path(DATA_DIR, "h_indices.feather"))
h_indices <- feather::read_feather(path(DATA_DIR, "h_indices.feather"))
years <- seq.int(1943L, 2018L)
h_indices$year <- years
flog.info("Creating H-indices.")
h_indices <- tidyr::gather(h_indices, "author", "h_index", -year)
hi_annual <- h_indices %>%
  group_by(author) %>%
  fill(h_index) %>%
  mutate(lag_h_index = lag(h_index),
         h_index_diff = h_index - lag_h_index) %>%
  summarise(hi_annual = mean(h_index_diff, na.rm = TRUE))

flog.info("Getting Hi-annual.")
combined %<>%
  ungroup() %>%
  left_join(hi_annual, by = c("full_name" = "author"))
 
## total citations ------------------------------------------------------------
flog.info("Getting total citations.")
combined %<>%
  group_by(full_name, `NPI Number`) %>%
  mutate(citeCount = sum(citedByCount, na.rm = TRUE))

## max citations ------------------------------------------------------------
flog.info("Getting max citations.")
combined %<>%
  group_by(full_name, `NPI Number`) %>%
  mutate(maxCite = max(citedByCount, na.rm = TRUE))

## first publication ----------------------------------------------------------
flog.info("Getting first publication.")
combined %<>%
  group_by(full_name, `NPI Number`) %>%
  mutate(firstPub = min(pubYear, na.rm = TRUE))

## write results --------------------------------------------------------------
flog.info("Writing results to %s", path(DATA_DIR, "combined.fst"))
combined %>% fst::write_fst(path(DATA_DIR, "combined.fst"), compress = 100)
