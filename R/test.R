# Load data



if(F){
  
  #source("https://raw.githubusercontent.com/ramarty/rSocialWatcher/main/R/main.R")
  
  remove.packages("rSocialWatcher")
  
  roxygen2::roxygenise("~/Documents/Github/rSocialWatcher")
  
  devtools::install_github("ramarty/rSocialWatcher")
  library(rSocialWatcher)
  
  ?query_fb_marketing_api
  
  # Load keys --------------------------------------------------------------------
  api_keys <- read.csv("~/Dropbox/World Bank/Webscraping/Files for Server/api_keys.csv",
                       stringsAsFactors = F)
  
  api_keys <- api_keys %>%
    dplyr::filter(Service == "facebook_marketing_ad",
                  Details == "robmarty3@gmail.com_v2")
  
  TOKEN        <- api_keys %>% dplyr::filter(Account == "token")        %>% pull(Key)
  CREATION_ACT <- api_keys %>% dplyr::filter(Account == "creation_act") %>% pull(Key) %>% str_replace_all("ACT_", "")
  VERSION      <- api_keys %>% dplyr::filter(Account == "version")      %>% pull(Key)
  
  # Test: get_fb_parameters ------------------------------------------------------
  ## Geolocation keys
  country_df <- get_fb_parameters(type = "country",
                                  version = VERSION,
                                  token = TOKEN)
  
  states_df <- get_fb_parameters(type = "region",
                                 version = VERSION,
                                 token = TOKEN,
                                 country_code = "US")
  
  # All cities that start with "New" in New York
  city_df <- get_fb_parameters(type = "city",
                               version = VERSION,
                               token = TOKEN,
                               country_code = "US",
                               q="New York City",
                               region_id = 3875)
  
  # NYC and all locations within NYC (boroughs, neighborhoods, etc)
  nyc_df <- get_fb_parameters(type = "city",
                              version = VERSION,
                              token = TOKEN,
                              country_code = "US",
                              q="New York City",
                              region_id = 3875,
                              key = 2490299)
  
  nyc_subcity_df <- get_fb_parameters(type = "subcity",
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
  
  ## Parameters
  demog_df <- get_fb_parameters(type = "demographics",
                                version = VERSION,
                                token = TOKEN)
  
  interests_df <- get_fb_parameters(type = "interests",
                                    version = VERSION,
                                    token = TOKEN)
  
  behaviors_df <- get_fb_parameters(type = "behaviors",
                                    version = VERSION,
                                    token = TOKEN)
  
  
  
  out_df <- GET(
    paste0("https://graph.facebook.com/",VERSION,"/search"),
    query=list(
      type='adlocale',
      access_token=TOKEN,
      limit=2000
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  # Test: query_fb_marketing_api -------------------------------------------------
  fb_df <- query_fb_marketing_api(
    location_type = "country",
    country_iso2  = "KE",
    behavior      = c(6003966451572, 6120699721983),
    interest      = 6003349442621,
    family_statuses = c(6002714398372),
    version       = VERSION,
    creation_act  = CREATION_ACT,
    token         = TOKEN,
    sleep_time    = 0.1)
  
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


