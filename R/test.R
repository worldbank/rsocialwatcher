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
  
  ## Load keys
  api_keys <- read.csv("~/Dropbox/World Bank/Webscraping/Files for Server/api_keys.csv",
                       stringsAsFactors = F)
  
  
  api_keys <- api_keys %>%
    dplyr::filter(Service == "facebook_marketing_ad",
                  Details == "robmarty3@gmail.com")
  
  TOKEN        <- api_keys %>% dplyr::filter(Account == "token")        %>% pull(Key)
  CREATION_ACT <- api_keys %>% dplyr::filter(Account == "creation_act") %>% pull(Key) %>% str_replace_all("ACT_", "")
  VERSION      <- api_keys %>% dplyr::filter(Account == "version")      %>% pull(Key)
  
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
  
  df_out$query
  
  interests_df %>% 
    head()
  
  Facebook Marketing API - And vs OR Condition
  
  https://graph.facebook.com/v14.0/<ACT>/delivery_estimate?access_token=<TOKEN>&include_headers=false&method=get&pretty=0&suppress_http_code=1&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1&targeting_spec={'geo_locations':{'countries':['US']},'interests':[{'id':6002839660079},{'id':6002884511422},{'id':6002867432822}]}

  
  
    
  
  https://graph.facebook.com/v14.0/act_10355127/delivery_estimate?access_token=EAAlmckTru5EBABTaZBhK2UjvIMqKZCT6GFvWSvFueC8Fn9M8xTJa4dgWNGjVoXCi7IJv7ruJjRzPRZADMqotokuMHzFqGumvHTQl2F0anOL4chCBsSIauBuZCrgiu7lE1sPTVIWuzyHr2BWXh2semFWMYeyeXGkTFcp200UkZBO6xb0xZC6sez&include_headers=false&method=get&pretty=0&suppress_http_code=1&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1&targeting_spec={'geo_locations':{'countries':['US']},'interests':[{'id':6002839660079},{'id':6002884511422},{'id':6002867432822}]}
  
  https://graph.facebook.com/v14.0/act_10355127/delivery_estimate?access_token=EAAlmckTru5EBABTaZBhK2UjvIMqKZCT6GFvWSvFueC8Fn9M8xTJa4dgWNGjVoXCi7IJv7ruJjRzPRZADMqotokuMHzFqGumvHTQl2F0anOL4chCBsSIauBuZCrgiu7lE1sPTVIWuzyHr2BWXh2semFWMYeyeXGkTFcp200UkZBO6xb0xZC6sez&include_headers=false&method=get&pretty=0&suppress_http_code=1&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1&targeting_spec={'geo_locations':{'countries':['US'],'location_types':['home']},'interests':[{'id':6002839660079}],'interests':[{'id':6002884511422}],'interests':[{'id':6002867432822}],'genders':[1,2],'age_min':18,'age_max':65}
  
  https://graph.facebook.com/v14.0/act_10355127/delivery_estimate?access_token=EAAlmckTru5EBABTaZBhK2UjvIMqKZCT6GFvWSvFueC8Fn9M8xTJa4dgWNGjVoXCi7IJv7ruJjRzPRZADMqotokuMHzFqGumvHTQl2F0anOL4chCBsSIauBuZCrgiu7lE1sPTVIWuzyHr2BWXh2semFWMYeyeXGkTFcp200UkZBO6xb0xZC6sez&include_headers=false&method=get&pretty=0&suppress_http_code=1&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1&targeting_spec={'geo_locations':{'countries':['US'],'location_types':['home']},'interests':[{'id':6002884511422}],'interests':[{'id':6002839660079}],'genders':[1,2],'age_min':18,'age_max':65}
  
  
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