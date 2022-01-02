# Load data

source("https://raw.githubusercontent.com/ramarty/rSocialWatcher/main/R/main.R")

# Load keys --------------------------------------------------------------------
api_keys <- read.csv("~/Dropbox/World Bank/Webscraping/Files for Server/api_keys.csv",
                     stringsAsFactors = F)

api_keys <- api_keys %>%
  dplyr::filter(Service == "facebook_marketing_ad",
                Details == "robmarty3@gmail.com")

TOKEN        <- api_keys %>% dplyr::filter(Account == "token")        %>% pull(Key)
CREATION_ACT <- api_keys %>% dplyr::filter(Account == "creation_act") %>% pull(Key)
VERSION      <- api_keys %>% dplyr::filter(Account == "version")      %>% pull(Key)

# Test -------------------------------------------------------------------------
fb_df <- query_fb_marketing_api(
  location_type = "country",
  country_iso2  = "KE",
  behavior      = c(6003966451572, 6120699721983),
  interest      = 6003985771306,
  version       = VERSION,
  creation_act  = CREATION_ACT,
  token         = TOKEN,
  sleep_time    = 0.1)

behavior      = c(6003966451572, 6120699721983)
interest      = 6003985771306

location_type = "country"
country_iso2 = "KE"
latitude = NULL
longitude = NULL
radius = NULL
radius_unit = NULL
education_statuses = NULL
user_os = NULL
wireless_carrier = NULL
behavior = NULL
interest = NULL
gender = c(1,2)
age_min = 18
age_max = 65
sleep_time = 20
show_result = T
version       = VERSION
creation_act  = CREATION_ACT
token         = TOKEN
