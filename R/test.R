# Load data

if(F){
  
  roxygen2::roxygenise("~/Documents/Github/rSocialWatcher")
  
  remove.packages("rSocialWatcher")
  devtools::install_github("ramarty/rSocialWatcher")
  
  ## Setup
  library(rSocialWatcher)
  library(tidyverse)
  
  ## Load keys
  api_keys <- read.csv("~/Dropbox/World Bank/Webscraping/Files for Server/api_keys.csv",
                       stringsAsFactors = F)

  
  api_keys <- api_keys %>%
    dplyr::filter(Service == "facebook_marketing_ad",
                  Details == "robmarty3@gmail.com")
  
  TOKEN        <- api_keys %>% dplyr::filter(Account == "token")        %>% pull(Key)
  CREATION_ACT <- api_keys %>% dplyr::filter(Account == "creation_act") %>% pull(Key) %>% str_replace_all("ACT_", "")
  VERSION      <- api_keys %>% dplyr::filter(Account == "version")      %>% pull(Key)
  
  # Test: get_fb_parameters ------------------------------------------------------
  ## Parameters
  demog_df <- get_fb_parameter_ids(type = "demographics",
                                   version = VERSION,
                                   token = TOKEN)
  
  interests_df <- get_fb_parameter_ids(type = "interests",
                                       version = VERSION,
                                       token = TOKEN)
  
  behaviors_df <- get_fb_parameter_ids(type = "behaviors",
                                       version = VERSION,
                                       token = TOKEN)
  
  ## Geolocation keys
  country_group_df <- get_fb_parameter_ids(type = "country_group",
                                        version = VERSION,
                                        token = TOKEN)
  
  country_df <- get_fb_parameter_ids(type = "country",
                                  version = VERSION,
                                  token = TOKEN)
  
  states_df <- get_fb_parameter_ids(type = "region",
                                 version = VERSION,
                                 token = TOKEN,
                                 country_code = "US")
  
  # All cities that start with "New" in New York
  city_df <- get_fb_parameter_ids(type = "city",
                               version = VERSION,
                               token = TOKEN,
                               country_code = "US",
                               q="New York City",
                               region_id = 3875)
  
  city_df <- get_fb_parameter_ids(type = "city",
                               version = VERSION,
                               token = TOKEN,
                               country_code = "US",
                               q="Cincinnati")
  
  city_df <- get_fb_parameter_ids(type = "city",
                               version = VERSION,
                               token = TOKEN,
                               country_code = "US",
                               q="Cincinnati",
                               region_id = 3878)
  
  # NYC and all locations within NYC (boroughs, neighborhoods, etc)
  nyc_df <- get_fb_parameter_ids(type = "city",
                              version = VERSION,
                              token = TOKEN,
                              country_code = "US",
                              q="New York City",
                              region_id = 3875,
                              key = 2490299)
  
  nyc_subcity_df <- get_fb_parameter_ids(type = "subcity",
                                      version = VERSION,
                                      token = TOKEN,
                                      country_code = "US",
                                      q="New York City",
                                      region_id = 3875,
                                      key = 2490299)
  
  nyc_neigh_df <- get_fb_parameters(type = "neighborhood",
                                    version = VERSION,
                                    token = TOKEN,
                                    country_code = "US",
                                    q="New York City",
                                    region_id = 3875,
                                    key = 2490299)
  
  zip_df <- get_fb_parameter_ids(type = "zip",
                              version = VERSION,
                              token = TOKEN,
                              country_code = "US",
                              q="1",
                              region_id = 3875)
  
  zip_nyc_df <- get_fb_parameter_ids(type = "zip",
                                  version = VERSION,
                                  token = TOKEN,
                                  country_code = "US",
                                  q="1",
                                  region_id = 3875,
                                  key = 2490299)
  
  # US ONLY
  dma_market <- get_fb_parameter_ids(type = "geo_market",
                                  version = VERSION,
                                  token = TOKEN)
  
  elec_dist_df <- get_fb_parameter_ids(type = "electoral_district",
                                    version = VERSION,
                                    token = TOKEN,
                                    q = 'Ohio')


  
  
  
  out_df <- GET(
    paste0("https://graph.facebook.com/",VERSION,"/search"),
    query=list(
      type='adlocale',
      access_token=TOKEN,
      limit=2000
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  # Examples -------------------------------------------------------------------
  library(WDI)
  
  wdi_df <- WDI(
    country = "all",
    indicator = c("SE.TER.ENRR", "SE.ADT.1524.LT.ZS", "SE.SEC.ENRR"),
    start = 2019,
    end = 2019,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )
  
  wdi_df <- wdi_df %>%
    dplyr::filter(!is.na(SE.TER.ENRR))
  
  country_df <- get_fb_parameters(type = "country",
                                  version = VERSION,
                                  token = TOKEN)
  country_df <- country_df[country_df$country_code %in% wdi_df$iso2c,]
  
  countries_list <- as.list(country_df$country_code[1:30])
  
  fb_df <- query_fb_marketing_api(
    location_type = "countries",
    country_code  = countries_list,
    education_statuses = list(NULL, c(2,5,6,7,8,9,10,11)),
    version       = VERSION,
    creation_act  = CREATION_ACT,
    token         = TOKEN,
    sleep_time    = 0.5,
    add_query=T)
  
  ## Proportion high school grad
  fb_wide_df <- fb_df %>%
    mutate(educ_level = ifelse(is.na(education_statuses), "all", "educ_ter")) %>%
    pivot_wider(id_cols = country_code, names_from = educ_level, values_from = estimate_mau_upper_bound) %>%
    mutate(prop_hs = (educ_ter / all)*100) %>%
    rename(iso2c = country_code) %>%
    left_join(wdi_df, by = "iso2c")
  
  fb_wide_df
  
  fb_wide_df %>%
    ggplot() +
    geom_point(aes(x = prop_hs,
                   y = SE.TER.ENRR))
  
  fb_wide_df %>%
    ggplot() +
    geom_point(aes(x = prop_hs,
                   y = SE.SEC.ENRR))
  
  
  
  head(fb_df)
  
  # Test: query_fb_marketing_api -------------------------------------------------
  states_df
  fb_df <- query_fb_marketing_api(
    location_type = "cities",
    location_key  = 2733673,
    radius = 30,
    radius_unit = "kilometer",
    version       = VERSION,
    creation_act  = CREATION_ACT,
    token         = TOKEN,
    sleep_time    = 0.1,
    add_query=T)
  
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
  version      = VERSION
  creation_act = CREATION_ACT
  token        = TOKEN
  
  # Test making parameter dataframe ==============================================
  ## (1) Can enter this into another function, and it loops over. 
  ## (2) Function should give warning if required parameters are not entered
  ## --- See warnings from other script. Make those into separate functions
  ##     to use in both scripts?
  ## (3) **TODO Think about how this works with looping over locations
  
  # TODO: (1) sleep after location; (2) sleep after parameter --> auto calculate
  # TODO: (1) Allow looping over keys! Then auto-figure out timing needed!
  # TODO: Double check requests per hour to calculate this.
  # TODO: Just have this be the one, main function? Yes! Then makes sleeping easier,
  #       if want different sleeping after location vs param.
  
  # Parameter inputs -------------------------------------------------------------
  location_type <- "coordinates"
  
  lat_lon <- list(c(-1.286389, 36.817222),
                  c(-6.816111, 39.280278))
  
  radius        = 5
  radius_unit   = "kilometer"
  country_iso2  <- NULL
  
  education_statuses <- NULL
  user_os <- NULL
  wireless_carrier <- NULL
  
  behavior <- list(NULL,
                   6004382299972,
                   6002714895372,
                   c(6004382299972, 6002714895372))
  
  interest <- list(c(6003349442621, 6003020834693))
  
  gender <- c(1,2)
  
  age_min <- 18
  age_max <- 65
  
  # Checks -----------------------------------------------------------------------
  if(length(location_type) != 1) stop("'location_type' must be a vector of length one, either 'coordinates' or 'country'; only one option allowed")
  
  # Convert param inputs to list -------------------------------------------------
  convert_to_list <- function(x){
    # Converts to list if not a list
    if(!is.list(x)) x <- list(x)
    return(x)
  }
  
  lat_lon            <- lat_lon %>% convert_to_list()
  radius             <- radius %>% convert_to_list()
  radius_unit        <- radius_unit %>% convert_to_list()
  country_iso2       <- country_iso2 %>% convert_to_list()
  education_statuses <- education_statuses %>% convert_to_list()
  user_os            <- user_os %>% convert_to_list()
  wireless_carrier   <- wireless_carrier %>% convert_to_list()
  behavior           <- behavior %>% convert_to_list()
  interest           <- interest %>% convert_to_list()
  gender             <- gender %>% convert_to_list()
  age_min            <- age_min %>% convert_to_list()
  age_max            <- age_max %>% convert_to_list()
  
  # Length parameter inputs to same length ---------------------------------------
  n_param_combn <- length(lat_lon) * 
    length(radius) *
    length(radius_unit) *
    length(country_iso2) *
    length(education_statuses) *
    length(user_os) *
    length(wireless_carrier) *
    length(behavior) *
    length(interest) *
    length(gender) *
    length(age_min) *
    length(age_max)
  
  lat_lon            <- rep(lat_lon, length = n_param_combn)
  radius             <- rep(radius, length = n_param_combn)
  radius_unit        <- rep(radius_unit, length = n_param_combn)
  education_statuses <- rep(education_statuses, length = n_param_combn)
  user_os            <- rep(user_os, length = n_param_combn)
  wireless_carrier   <- rep(wireless_carrier, length = n_param_combn)
  behavior           <- rep(behavior, length = n_param_combn)
  interest           <- rep(interest, length = n_param_combn)
  gender             <- rep(gender, length = n_param_combn)
  age_min            <- rep(age_min, length = n_param_combn)
  age_max            <- rep(age_max, length = n_param_combn)
  
  # Length parameter inputs to same length ---------------------------------------
  out_df <- mapply(query_fb_marketing_api_1call,
                   lat_lon = lat_lon,
                   radius = radius,
                   radius_unit = radius_unit,
                   education_statuses = education_statuses,
                   user_os = user_os,
                   wireless_carrier = wireless_carrier,
                   behavior = behavior,
                   interest = interest,
                   gender = gender,
                   age_min = age_min,
                   age_max = age_max,
                   MoreArgs = list(location_type = location_type,
                                   sleep_time = 3,
                                   show_result = T,
                                   version = version,
                                   creation_act = creation_act,
                                   token = token),
                   SIMPLIFY = F
  ) %>% 
    bind_rows()
  
}


