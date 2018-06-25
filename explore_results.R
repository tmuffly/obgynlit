## utilities ---------------------------------------------------------------
library(tidyverse)
library(fs)
library(futile.logger)
library(fst)

## read data ---------------------------------------------------------------
#DATA_DIR <- path("/Dropbox")
#DATA_DIR <- path("/Dropbox/scraper/Version from 6.22.2018/")
setwd("~/Dropbox/Pubmedsearch/Scraper/Version from 6.22.2018")

flog.info("Reading results from %s", path(DATA_DIR, "combined.fst"))
combined <- fst::read.fst("combined.fst")

## sample author --------------------------------------------------------------
flog.info("Viewing sample author with NPI == '1154435063'.")
combined %>%
  ungroup %>%
  filter(`NPI Number` == '1689603763') %>%
  select(full_name, pmid, title,
         citedByCount, maxCite, pubCount, citeCount, authorCount, firstPub,
         pubYear, minPubYear, maxPubYear, 
         h_index, I10_index,  hi_norm, hc_index, hi_annual, m_quotient, g_index) %>%
  View("Sample author")

