# Load data

# TODO
# 1. location types -- all plural or singular 
# 2. And condition

if(F){
  
  roxygen2::roxygenise("~/Documents/Github/rSocialWatcher")
  
  remove.packages("rSocialWatcher")
  devtools::install_github("ramarty/rSocialWatcher")
  
  ## Setup
  library(rSocialWatcher)
  library(tidyverse)
  library(dplyr)
  library(lubridate)
  library(jsonlite)
  library(httr)
  library(stringr)
  library(splitstackshape) 
  
  ## Load keys
  api_keys <- read.csv("~/Dropbox/World Bank/Webscraping/Files for Server/api_keys.csv",
                       stringsAsFactors = F)
  
  
  api_keys <- api_keys %>%
    dplyr::filter(Service == "facebook_marketing_ad",
                  Details == "robmarty3@gmail.com")
  
  TOKEN        <- api_keys %>% dplyr::filter(Account == "token")        %>% pull(Key)
  CREATION_ACT <- api_keys %>% dplyr::filter(Account == "creation_act") %>% pull(Key) %>% str_replace_all("ACT_", "")
  VERSION      <- api_keys %>% dplyr::filter(Account == "version")      %>% pull(Key)
  
  # Test -----------------------------------------------------------------------
  version <- VERSION
  token   <- TOKEN
  
  #### connections [NOPE]
  #### friends_of_connections [NOPE]
  #### custom_audiences [NOPE]
  #### interests [ID_FUN]
  GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=list(
      type='adTargetingCategory',
      class="interests",
      access_token=token,
      limit=2000
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  #### behaviors [ID_FUN]
  GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=list(
      type='adTargetingCategory',
      class="behaviors",
      access_token=token,
      limit=2000
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  #### college_years 
  # Array of integers for college graduation year
  
  #### education_majors [??]
  q <- "phys"
  GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=list(
      type='adeducationmajor',
      q=q,
      access_token=token
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  #### education_schools
  q <- "William"
  GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=list(
      type='adeducationschool',
      q=q,
      access_token=token
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  #### education_statuses [ID_FUN - Manual df]
  
  #### family_statuses [ID_FUN]
  GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=list(
      type='adTargetingCategory',
      class="family_statuses",
      access_token=token,
      limit=2000
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  #### home_value [NOPE]

  #### interested_in [NOPE]

  #### income [ID_FUN]
  GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=list(
      type='adTargetingCategory',
      class="income",
      access_token=token,
      limit=2000
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  #### industries [ID_FUN]
  GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=list(
      type='adTargetingCategory',
      class="industries",
      access_token=token,
      limit=2000
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  #### life_events [ID_FUN]
  GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=list(
      type='adTargetingCategory',
      class="life_events",
      access_token=token,
      limit=2000
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  #### user_adclusters

  #### work_positions
  q <- "Research Analyst"
  GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=list(
      type="adworkposition",
      q=q,
      access_token=token
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  #### work_employers
  q <- "University"
  GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=list(
      type="adworkemployer",
      q=q,
      access_token=token
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  
  
  
  
  
  # https://developers.facebook.com/docs/marketing-api/reference/ad-account/targetingsearch/
  map_param <- function(...){
    # Function will create a separate query for each item. 
    # Creates a list, where the first element in the list is "map_param", where the
    # function then interprets each element of the list as a separate query.
    # Complex queries can still be made:
    # map_param
    
    l <- as.list(c(...))
    l <- as.list(c("map_param", l))
    return(l)
  }
  
  group <- function(...){
    list(...)
  }
  map_param(NA,1,2,3,4:5,group(list(1,2)))
  
  map_param(1,2,3,group(1:2))
  
  
  
  group
  
  
  map_param(c(1,2))
  
  map_param(1,2,3,NULL)
  
  
  a <- map_param(1,2)
  
  map_param(list(1,2,NULL))
  
  
  map_param(list(1,list(1,2)))
  
  map_param(list(NULL, 1, NULL))
  
  
  df_out <- query_fb_marketing_api(location_unit_type = "countries",
                                   location_keys = map_param("US", "FR"),
                                   interest = c(6002839660079,
                                                6002884511422),
                                   # flex_target = list(
                                   #   list("interest" = c(6002839660079)),
                                   #   list("interest" = c(6002884511422))
                                   # ),
                                   version = VERSION, 
                                   creation_act = CREATION_ACT, 
                                   token = TOKEN,
                                   add_query = T,
                                   add_query_hide_credentials = F)
  
  # c(): OR
  # list(): AND
  
  list(c(1,2), 3)
  
  

  
  a <- map_param(list(1:2,list(1,2)))
  
  map_param(list(1,2))
  map_param(list(list(1,2)))
  
  
  l <- as.list(1:2)
  
  as.list(c("mq",l))
  
  map_query(1:2)
  
  map_df(l = c("US", "FR"))
  
  a <- list(c(1,2), 3)
  a
  
  
  df_out
  
  
  qu <- list()
  
  list(or = (1,2),
       and = 4:5)
  
  cn <- c("US", "FR")
  queries(cn)
  
  list(cn)
  
  
  
  
  list()
  
  
  list()
  
  
  c(or=c(1,2),
    and=c(3,4))
  
  list(c(1,2),
       1)
  
  interests_df <- get_fb_parameter_ids(type = "interests",
                                       version = VERSION,
                                       token = TOKEN)
  
  head(interests_df)
  

  
  list(NULL,
       list(list("interest" = c(6002839660079)),
            list("interest" = c(6002884511422)))
  )
  
  
  data.frame(a = c(list(1,2)))
  
  list()
  
  
  
  
  
  
  data.frame(flex_target = list(list("interest" = c(6002839660079)),
                                list("interest" = c(6002884511422))))
  
  interest = list(list(6002839660079,
                       6002884511422))
  
  
  list(c(1,2),
       c(1))
  
  a1 <- list(
    list("interest" = c(1, 2),
         "behavior" = c(7, 8)),
    list("interest" = c(3)),
    list("interest" = c(4))
  )
  
  a2 <- list(
    # Query 1
    list(
      list("interest" = c(1, 2),
           "behavior" = c(7, 8)),
      list("interest" = c(3)),
      list("interest" = c(4))
    ),
    
    # Query 2
    list(
      list("interest" = c(1, 2),
           "behavior" = c(7, 8)),
      list("interest" = c(3)),
      list("interest" = c(4))
    )
  )
  
  flex_target <- a1
  
  # Prep parameters
  flex_target_copy <- flex_target
  
  
  
  n_embedded_lists <- function(x){
    
    counter <- 0
    is_list <- T
    
    while(is_list){
      x <- x[[1]]
      
      counter <- counter + 1
      is_list <- is.list(x)
    }
    
    return(counter)
  }
  
  n_embedded_lists(a2)
  
  
  b <- a1[[1]]
  is.list(b[[1]])
  
  a2
  
  
  
  
  # Test: get_fb_parameter_ids -------------------------------------------------
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
  
  locales_df <- get_fb_parameter_ids(type = "locales",
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
  
  # Extract data ---------------------------------------------------------------
  ## Coordinate location
  df_out <- query_fb_marketing_api(location_type = "coordinates",
                                   lat_lon = c(38.90, -77.01),
                                   radius = 10,
                                   radius_unit = "kilometer",
                                   version = VERSION, 
                                   creation_act = CREATION_ACT, 
                                   token = TOKEN)
  
  ## Country Group
  country_group_df <- get_fb_parameter_ids(type = "country_group",
                                           version = VERSION,
                                           token = TOKEN)
  
  df_out <- query_fb_marketing_api_1call(location_type = "country_groups",
                                         location_keys = "africa",
                                         version = VERSION, 
                                         creation_act = CREATION_ACT, 
                                         token = TOKEN)
  
  ## Country
  country_df <- get_fb_parameter_ids(type = "country",
                                     version = VERSION,
                                     token = TOKEN)
  
  df_out <- query_fb_marketing_api_1call(location_type = "countries",
                                         location_keys = "US",
                                         version = VERSION, 
                                         creation_act = CREATION_ACT, 
                                         token = TOKEN)
  
  ## Region
  region_df <- get_fb_parameter_ids(type = "region",
                                    version = VERSION,
                                    token = TOKEN,
                                    country_code = "US")
  
  df_out <- query_fb_marketing_api_1call(location_type = "regions",
                                         location_keys = 3866,
                                         version = VERSION, 
                                         creation_act = CREATION_ACT, 
                                         token = TOKEN)
  
  ##
  region_df <- get_fb_parameter_ids(type = "region",
                                    version = VERSION,
                                    token = TOKEN,
                                    country_code = "US")
  
  df_out <- query_fb_marketing_api(location_type = "electoral_districts",
                                   location_keys = "US:OH12",
                                   version = VERSION, 
                                   creation_act = CREATION_ACT, 
                                   token = TOKEN,
                                   add_query = T,
                                   add_query_hide_credentials = F)
  
  # Test -----------------------------------------------------------------------
  df_out <- query_fb_marketing_api(location_unit_type = "countries",
                                   location_keys = "US",
                                   interest = c(6002839660079,
                                                6002884511422),
                                   version = VERSION, 
                                   creation_act = CREATION_ACT, 
                                   token = TOKEN,
                                   add_query = T,
                                   add_query_hide_credentials = F)
  
  interests_df <- get_fb_parameter_ids(type = "interests",
                                       version = VERSION,
                                       token = TOKEN)
  
  df_out$query

  
  
  'exclusions': { 
    'relationship_statuses': [1,3], 
    'life_events': [{'id':6003054185372,'name':'Recently moved'}] 
  }, 
  'flexible_spec': [ 
    { 
      'behaviors': [{'id':6002714895372,'name':'All travelers'}], 
      'interests': [ 
        {'id':6003107902433,'name':'Association football (Soccer)'}, 
        {'id':6003139266461,'name':'Movies'} 
      ] 
    }, 
    { 
      'interests': [{'id':6003020834693,'name':'Music'}], 
      'life_events': [{'id':6002714398172,'name':'Newlywed (1 year)'}] 
    } 
  ]
  
  list("interests" = 1:2,
       "behaviors" = 1:2)

  
  
  
  df_out$query
  
  interests_df %>% 
    head()
  
  Facebook Marketing API - And vs OR Condition
  

  

  
  get_fb_parameter_ids(type    = "city",
                       country_code = "US",
                       q = "LA",
                       version = VERSION,
                       token   = TOKEN) %>%
    head()
  
  
  
  | region             |
    | large_geo_area     |
    | medium_geo_area    |
    | small_geo_area     |
    | metro_area         |
    | city               |
    | subcity            |
    | neighborhood       |
    | subneighborhood    |
    | zip                |
    | geo_market         |
    | electoral_district |
    
    
    
}