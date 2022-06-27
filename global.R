library(tidyverse)
library(blscrapeR)
library(xts)



AllIowaURL <- "https://download.bls.gov/pub/time.series/la/la.data.22.Iowa"


ICRIowaData <- read_table(AllIowaURL) %>% 
  filter(grepl("CN19011|CN19031|CN19095|CN19103|CN19105|CN19113|CN19183", series_id) &
           period != "M13") %>%
  mutate(county = case_when(
    grepl("19011", series_id) ~ "Benton",
    grepl("19031", series_id) ~ "Cedar",
    grepl("19095", series_id) ~ "Iowa",
    grepl("19103", series_id) ~ "Johnson",
    grepl("19105", series_id) ~ "Jones",
    grepl("19113", series_id) ~ "Linn",
    grepl("19183", series_id) ~ "Washington"
  )) %>% 
  mutate(metric_name = case_when(
    grepl("6$", series_id) ~ "Labor Force",
    grepl("5$", series_id) ~ "Employment",
    grepl("4$", series_id) ~ "Unemployment"
  )) %>% 
  dateCast()

LF_ICRIowa <- ICRIowaData %>% 
  filter(grepl("6$", series_id)) %>%
  select(-footnote_codes, -period, -year)

Emp_ICRIowa <- ICRIowaData %>% 
  filter(grepl("5$", series_id)) %>%
  select(-footnote_codes, -period, -year)

Unemp_ICRIowa <- ICRIowaData %>% 
  filter(grepl("4$", series_id)) %>%
  select(-footnote_codes, -period, -year)

mostRecent <- last(ICRIowaData$date, n=1, keep = FALSE)
leastRecent <- first(ICRIowaData$date, n=1, keep = FALSE)


ICRIowaXTS <- xts(ICRIowaData,
                  order.by = as.Date(ICRIowaData$date))
