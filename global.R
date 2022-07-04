library(tidyverse)
library(blscrapeR)
library(xts)
library(rvest)
library(tigris)
options(tigris_use_cache = TRUE)


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
  mutate(FIPS = str_sub(series_id, start = 6, end = 10)
  ) %>% 
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

# https://community.rstudio.com/t/r-shiny-make-a-reactive-map-of-state-by-county/63224
IAcounties <- tigris::counties(state = "IA", 
                               keep_zipped_shapefile = TRUE,
                               refresh = FALSE)
ICRcounties <- IAcounties %>% 
  filter(NAME %in% c("Benton", "Linn", "Jones", "Washington", "Johnson", "Iowa", "Cedar"))


LAUSSchedule <- read_html("https://www.bls.gov/lau/")
# reprogram with table from https://www.bls.gov/schedule/news_release/metro.htm
longerLAUSSchedule <- read_html("https://www.bls.gov/schedule/news_release/metro.htm")

nextReleaseSchedule <- LAUSSchedule %>% 
  html_element(css = "#bodytext > div.highlight-box-green > ul") %>% 
  html_text() %>% 
  str_replace_all("[\n]|[\r]", " ")

fullReleaseSchedule <- longerLAUSSchedule %>% 
  html_element(css = "#bodytext > table") %>% 
  html_table() %>%
  mutate(
    `Reference Month` = lubridate::my(`Reference Month`),
    `Release Date` = lubridate::mdy(`Release Date`)
    ) %>% 
  filter(`Release Date` >= lubridate::today() - 30 & `Release Date` < lubridate::today() + 60) %>% 
  mutate(
    `Reference Month` = strftime(`Reference Month`, format = "%b %Y"),
    `Release Date` = strftime(`Release Date`, format = "%m/%d/%Y")
  )

