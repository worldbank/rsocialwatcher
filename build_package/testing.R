# Test Facebook Parameters

# Setup ------------------------------------------------------------------------
## Load packages
library(rSocialWatcher)
library(dplyr)
library(stringr)
library(jsonlite)
library(httr)
library(sf)

## Load keys
api_keys <- read.csv("~/Dropbox/World Bank/Webscraping/Files for Server/api_keys.csv",
                     stringsAsFactors = F)

api_keys <- api_keys %>%
  dplyr::filter(Service == "facebook_marketing_ad",
                Details == "robmarty3@gmail.com")

TOKEN <- api_keys %>% 
  dplyr::filter(Account == "token") %>% 
  pull(Key)

CREATION_ACT <- api_keys %>% 
  dplyr::filter(Account == "creation_act") %>% 
  pull(Key) %>% 
  str_replace_all("ACT_", "") 

VERSION <- api_keys %>% 
  dplyr::filter(Account == "version") %>% 
  pull(Key)

# Develop ----------------------------------------------------------------------
country_df <- get_fb_parameter_ids(type    = "country",
                                   version = VERSION, 
                                   token   = TOKEN,
                                   add_location_coords = T)
                                   
loc_sf <- get_location_coords(location_unit_type = "countries",
                                  location_keys = country_df$key[1:3],
                                  version = VERSION,
                                  token = TOKEN)

# Test parameter types ---------------------------------------------------------
## Get IDs ####
get_fb_parameter_ids(type = "interests", version = VERSION, token = TOKEN)
get_fb_parameter_ids(type = "behaviors", version = VERSION, token = TOKEN)
get_fb_parameter_ids(type = "education_majors", q = "Computer", version = VERSION, token = TOKEN)
get_fb_parameter_ids(type = "education_schools", q = "Washington", version = VERSION, token = TOKEN)
get_fb_parameter_ids(type = "education_statuses", version = VERSION, token = TOKEN)
get_fb_parameter_ids(type = "family_statuses", version = VERSION, token = TOKEN)
get_fb_parameter_ids(type = "income", version = VERSION, token = TOKEN)
get_fb_parameter_ids(type = "industries", version = VERSION, token = TOKEN)
get_fb_parameter_ids(type = "work_positions", q = "Data", version = VERSION, token = TOKEN)
get_fb_parameter_ids(type = "work_employers", q = "World Bank", version = VERSION, token = TOKEN)
get_fb_parameter_ids(type = "relationship_statuses", version = VERSION, token = TOKEN)
get_fb_parameter_ids(type = "life_events", version = VERSION, token = TOKEN)

## All #### 
query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

## Interests #### 
id <- get_fb_parameter_ids(type = "interests", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  interests          = id,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

## Behaviors ####
id <- get_fb_parameter_ids(type = "behaviors", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  behaviors          = id,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

## Education Majors ####
id <- get_fb_parameter_ids(type = "education_majors", q = "Computer", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  education_majors   = id,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

## Education Schools ####
id <- get_fb_parameter_ids(type = "education_schools", q = "Washington", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  education_schools  = id,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

## Education Statuses ####
id <- get_fb_parameter_ids(type = "education_statuses", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  education_statuses = id,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

## Family Statuses ####
id <- get_fb_parameter_ids(type = "family_statuses", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  family_statuses    = id,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

## Income ####
id <- get_fb_parameter_ids(type = "income", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  income             = id,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

## Industries ####
id <- get_fb_parameter_ids(type = "industries", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  industries         = id,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

## Work Positions ####
id <- get_fb_parameter_ids(type = "work_positions", q = "Data", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  work_positions     = id,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

## Work Employers ####
id <- get_fb_parameter_ids(type = "work_employers", q = "World Bank", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  work_employers     = id,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

## Relationship Statuses ####
id <- get_fb_parameter_ids(type = "relationship_statuses", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type    = "countries",
  location_keys         = "US",
  relationship_statuses = id,
  version               = VERSION, 
  creation_act          = CREATION_ACT, 
  token                 = TOKEN)

## Live Events ####
id <- get_fb_parameter_ids(type = "life_events", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type    = "countries",
  location_keys         = "US",
  life_events           = id,
  version               = VERSION, 
  creation_act          = CREATION_ACT, 
  token                 = TOKEN)

# Test location types ----------------------------------------------------------

# Test exclusion ---------------------------------------------------------------
id_interest <- get_fb_parameter_ids(type = "interests", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)
id_behavior <- get_fb_parameter_ids(type = "behaviors", version = VERSION, token = TOKEN) %>% pull(id) %>% head(1)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  interests          = id_interest,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  behaviors          = id_behavior,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)

query_fb_marketing_api(
  location_unit_type = "countries",
  location_keys      = "US",
  interests          = id_interest,
  excl_behaviors     = id_behavior,
  version            = VERSION, 
  creation_act       = CREATION_ACT, 
  token              = TOKEN)
