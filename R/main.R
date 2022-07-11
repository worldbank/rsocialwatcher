
# TODO:
# 1. Function for 1 pull
# 2. Wrapper; over multiple locations and multiple parameters
# 3. behavior_not: For behaviors to exlcude (also for interests, etc.)

library(dplyr)
library(lubridate)
library(jsonlite)
library(httr)
library(stringr)
if(F){
  roxygen2::roxygenise("~/Documents/Github/rSocialWatcher")
}

# Helper functions -------------------------------------------------------------
is_null_or_na <- function(x){
  # Return TRUE if x is NULL or NA; FALSE otherwise
  
  out <- FALSE
  
  if(is.null(x)) out <- TRUE
  
  if(!is.null(x)){
    if(TRUE %in% is.na(x)) out <- TRUE
  }
  
  return(out)
}

# Main functions ---------------------------------------------------------------

#' Get Facebook Parameter IDs
#'
#' This function returns dataframes of parameters for behaviors, demographics, 
#' and interests. The dataframes contain ids that can be used in the 
#' query_fb_marketing_api function.
#'
#' @param class Type of data; either "behaviors", "demographics", or "interests"
#' @param version Facebook Marketing API version; for example, "v14.0"
#' @param token Facebook Marketing API token
#' 
#' @return Dataframe with parameter IDs, descriptions, and global audience sizes
#' @export
get_fb_parameters <- function(class,
                              version,
                              token){
  # Get Facebook Parameters
  # ARGS
  # -- class: "demographics", "interests", or "behaviors"
  # -- version: API version. e.g., "v12.0"
  # -- token: Facebook API token
  
  out_df <- GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=list(
      type='adTargetingCategory',
      class=class,
      access_token=token,
      limit=2000
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  return(out_df)
}

query_fb_marketing_api_1call <- function(location_type,
                                         lat_lon = NULL,
                                         radius = NULL,
                                         radius_unit = NULL,
                                         country_iso2 = NULL,
                                         locales = NULL,
                                         behavior = NULL,
                                         interest = NULL,
                                         relationship_statuses = NULL, 
                                         life_events = NULL, 
                                         industries = NULL, 
                                         income = NULL, 
                                         family_statuses = NULL,
                                         education_statuses = NULL,
                                         user_os = NULL,
                                         wireless_carrier = NULL,
                                         gender = c(1,2),
                                         age_min = 18,
                                         age_max = 65,
                                         version, 
                                         creation_act, 
                                         token,
                                         sleep_time = 20,
                                         show_result = T,
                                         add_query = F,
                                         add_query_hide_credentials = T){
  
  # Query Facebook Marketing API
  # ARGs:
  # location_type: "coordinates" or "country"
  
  # --loc_i: Numeric id of which row to use from `coords_df`
  # --coords_df: Dataframe with latitude and longitude variables
  # --parameters_df_i: Dataframe with parameters
  # --version: Facebook marketing API verion
  # --creation_act: Creation act (associated with API key/account)
  # --token: API token/key
  
  # Checks ---------------------------------------------------------------------
  if(is.null(location_type)){
    stop("'location_type' required. Must be either 'coordinates' or 'country'")
  }
  
  if(!(location_type %in% c("coordinates", "country"))){
    stop("'location_type' must be either 'coordinates' or 'country'")
  }
  
  if(location_type == "coordinates"){
    
    if(length(lat_lon) != 2 ) stop("'lat_lon' must be a vector of length 2, with latitude then longitude")
    #if(is.null(latitude))    stop("Must enter numeric value for 'latitude'")
    #if(is.null(longitude))   stop("Must enter numeric value for 'longitude'")
    if(is.null(radius))      stop("Must enter numeric value for 'radius'")
    if(is.null(radius_unit)) stop("Must enter 'kilometer' or 'mile' for 'radius_unit'")
  }
  
  if(location_type == "country"){
    if(is.null(country_iso2)) stop("Must enter value for 'country_iso2'")
  }
  
  # Check internet -------------------------------------------------------------
  # Stall if not connected to internet
  while(!curl::has_internet()){ Sys.sleep(5); print("Looking for internet")}
  
  # Prep parameters ------------------------------------------------------------
  if(is_null_or_na(behavior)){
    behavior_param <- NULL
  } else{
    behavior_param <- paste0("{'id':", behavior, "}") %>% paste(collapse = ",")
  }
  
  if(is_null_or_na(interest)){
    interest_param <- NULL
  } else{
    interest_param <- paste0("{'id':", interest, "}") %>% paste(collapse = ",")
  }
  
  if(is_null_or_na(relationship_statuses)){
    relationship_statuses_param <- NULL
  } else{
    relationship_statuses_param <- paste0("{'id':", relationship_statuses, "}") %>% paste(collapse = ",")
  }
  
  #relationship_statuses life_events industries income family_statuses
  
  if(is_null_or_na(education_statuses)){
    education_statuses_param <- NULL
  } else{
    education_statuses_param <- education_statuses %>% paste(collapse = ",")
  }
  
  if(is_null_or_na(user_os)){
    user_os_param <- NULL
  } else{
    if(length(user_os) > 1) stop("Only accepts vector of length 1 for user_os (right now)")
    user_os_param <- paste0("'", user_os, "'")
  }
  
  if(is_null_or_na(wireless_carrier)){
    wireless_carrier_param <- NULL
  } else{
    if(length(wireless_carrier) > 1) stop("Only accepts vector of length 1 for wireless_carrier (right now)")
    wireless_carrier_param <- paste0("'", wireless_carrier, "'")
  }
  
  gender_param <- gender %>% paste(collapse = ",")
  
  # Make Query -----------------------------------------------------------------
  if(location_type == "coordinates"){
    latitude  <- lat_lon[1]
    longitude <- lat_lon[2]
    
    query_location <- paste0("'geo_locations':{'location_types':['home'],'custom_locations':[{'latitude':",
                             latitude %>% substring(1,7),",",
                             "'longitude':",
                             longitude %>% substring(1,7),",",
                             "'radius':",
                             radius,",",
                             "'distance_unit':'",radius_unit,"'}]},")
  } else if (location_type == "country"){
    query_location <- paste0("'geo_locations':{'countries':['",country_iso2,"']},")
  }
  
  query <- paste0("https://graph.facebook.com/",version,
                  "/act_",creation_act,
                  "/delivery_estimate?access_token=",token,
                  "&include_headers=false&method=get&pretty=0&suppress_http_code=1&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1&targeting_spec={",
                  query_location,
                  ifelse(is.null(behavior_param), "", 
                         paste0("'behaviors':[", behavior_param, "],")), 
                  ifelse(is.null(interest_param), "", 
                         paste0("'interests':[", interest_param, "],")), 
                  ifelse(is.null(relationship_statuses_param), "", 
                         paste0("'interests':[", relationship_statuses_param, "],")), 
                  ifelse(is.null(education_statuses_param), "", 
                         paste0("'education_statuses':[", education_statuses_param, "],")), 
                  ifelse(is.null(user_os_param), "", 
                         paste0("'user_os':[", user_os_param, "],")), 
                  ifelse(is.null(wireless_carrier_param), "", 
                         paste0("'wireless_carrier':[", wireless_carrier_param, "],")), 
                  "'genders':[",gender_param,"],", 
                  "'age_min':",age_min,",",
                  "'age_max':",age_max, 
                  "}")
  
  # Make query and prep dataframe with results and parameter
  query_val_df <- tryCatch({
    
    query_val <- url(query) %>% fromJSON
    
    #### If there is no error
    if(is.null(query_val$error)){
      
      ## Marketing info to dataframe
      query_val_df <- query_val$data
      query_val_df$daily_outcomes_curve <- NULL
      
      ## Add parameter info
      query_val_df$location_type         <- location_type
      query_val_df$behavior              <- behavior              %>% paste(collapse = ",")
      query_val_df$interest              <- interest              %>% paste(collapse = ",")
      query_val_df$relationship_statuses <- relationship_statuses %>% paste(collapse = ",")
      query_val_df$life_events           <- life_events           %>% paste(collapse = ",")
      query_val_df$industries            <- industries            %>% paste(collapse = ",")
      query_val_df$income                <- income                %>% paste(collapse = ",")
      query_val_df$family_statuses       <- family_statuses       %>% paste(collapse = ",")
      query_val_df$education_statuses    <- education_statuses    %>% paste(collapse = ",")
      query_val_df$user_os               <- user_os               %>% paste(collapse = ",")
      query_val_df$wireless_carrier      <- wireless_carrier      %>% paste(collapse = ",")
      query_val_df$gender                <- gender                %>% paste(collapse = ",")
      query_val_df$age_min               <- age_min
      query_val_df$age_max               <- age_max
      
      if(location_type == "coordinates"){
        query_val_df$latitude    <- latitude
        query_val_df$longitude   <- longitude
        query_val_df$radius      <- radius
        query_val_df$radius_unit <- radius_unit
      } else{
        query_val_df$country_iso2 <- country_iso2
      }
      
      ## Add time
      query_val_df$api_call_time_utc <- Sys.time() %>% with_tz(tzone = "UTC")
      
      if(add_query){
        query_val_df$query <- query
        if(add_query_hide_credentials){
          query_val_df$query <- query_val_df$query %>%
            str_replace_all(paste0("act_", creation_act), "act_CREATION_ACT") %>%
            str_replace_all(paste0("access_token=", token), "access_token=TOKEN")
        }
        
      } 
      
      ## Print result and sleep (sleep needed b/c of rate limiting)
      #if(show_result){
      #  print(query_val_df)
      #}
      
      if(show_result){
        print(query_val_df$estimate_mau_upper_bound)
      }
      
      ## Sleep
      Sys.sleep(sleep_time) 
      
      #### If there is an error, print the error and make output null  
    } else{
      print(query_val)
      print("Checking error!")
      
      if(!is.null(query_val$error$code)){
        if((query_val$error$code == 80004)){
          print("too many calls, so sleeping a bit!")
          Sys.sleep(10)
        } 
      }
      
      query_val_df <- ""
      
      # Sometimes lat/lon is not in a valid location. We still create a dataframe
      # for those queries.
      if(query_val$error$error_user_title == "Incorrect Location Format"){
        query_val_df <- data.frame(ERROR = "Incorrect Location Format")
      } else{
        query_val_df <- NULL
      }
      
    }
    
    query_val_df
    
  },error = function(e){
    print("ERROR")
    Sys.sleep(0.1)
    return(NULL)
  })
  
  return(query_val_df)
}

#' Query Facebook Marketing API
#' 
#' @param location_type Either `"coordinates"` (for buffer around single point) or `"country"`
#' ## If location_Type = "coordinates"
#' @param lat_lon Coordinates, c(lat, lon). For example, `c(38.90, -77.01)`
#' @param radius Radius around coordinate
#' @param radius_unit Unit for radius; either `"kilometer"` or `"mile"`
#' ## If location_type = "country" 
#' @param country_iso2 Country ISO2; for example, `"US"`.
#' ## Other location??
#' @param locales Words
#' ## Parameters. These are optional. If nothing specified, then searches for all users.
#' @param behavior Vector of behavior IDs. If multiple, uses `OR` condition; for example, `behavior = c(6002714895372, 6008297697383)` will target users who are either frequent travelers or returned from travels 2 weeks ago. Use `get_fb_parameters(class = "behaviors")` to get dataframe with IDs and descriptions. 
#' @param interest Vector of interest IDs. If multiple, uses `OR` condition; for example, `interest = c(6003349442621, 6003139266461)` will target users who are interested in either entertainment or movies. Use `get_fb_parameters(class = "interests")` to get dataframe with IDs and descriptions. 
#' @param relationship_statuses Vector of relationship status IDs. If multiple, uses `OR` condition; for example, `relationship_statuses = c(3,4)` targets those who are married or engaged. See `relationship_statuses` in the [Advanced Targeting Documentation](https://developers.facebook.com/docs/marketing-api/audiences/reference/advanced-targeting) to see relationship status ID options with descriptions. 
#' @param life_events Vector of life event IDs. If multiple, uses `OR` condition; for example, `life_events = c(6005149512172, 6005149512172)` targets users who recently moved or are in a new job. Use `get_fb_parameters(class = "demographics")` to get dataframe with IDs and descriptions. 
#' @param industries Vector of industries IDs. If multiple, uses `OR` condition; for example, `industries = c(6008888980183, 6008888972183)` targets users who work in sales or legal services. Use `get_fb_parameters(class = "demographics")` to get dataframe with IDs and descriptions. 
#' @param income Vector of income IDs. If multiple, uses `OR` condition; for example, `income = c(6107813553183, 6107813554583)` targets users with a household income in the top 10%-25% or 25%-50% of ZIP codes (US). Use `get_fb_parameters(class = "demographics")` to get dataframe with IDs and descriptions. 
#' @param family_statuses Vector of family status IDs. If multiple, uses `OR` condition; for example, `family_statuses = c(6023080302983, 6023005681983)` targets users who are parents with preteens or parents with teenagers. Use `get_fb_parameters(class = "demographics")` to get dataframe with IDs and descriptions. 
#' @param education_statuses Education status IDs. If multiple, uses `OR` condition; for example, `education_statuses = c(9,10)` will yeild those who report to have either a Master degree or professional degree. See `education_statuses` in the [Advanced Targeting Documentation](https://developers.facebook.com/docs/marketing-api/audiences/reference/advanced-targeting) to see education status options. 
#' @param user_os User operating systems. If multiple, uses `OR` condition; for example `user_os = ['iOS', 'Android']` targets those that use either an iOS or Android OS. See `user_os` in the [Advanced Targeting Documentation](https://developers.facebook.com/docs/marketing-api/audiences/reference/advanced-targeting) for additional details.
#' @param wireless_carrier Wireless carriet. If set to `Wifi`, then targets those connecting via a Wifi network. See `wireless_carrier` in the [Advanced Targeting Documentation](https://developers.facebook.com/docs/marketing-api/audiences/reference/advanced-targeting) for additional details.
#' @param gender Genders to target; 1 targets males and 2 targets females Default is both. See `gender` in the [Basic Targeting Documentation](https://developers.facebook.com/docs/marketing-api/audiences/reference/basic-targeting#demographics).
#' @param age_min Minimum age. Default is 18. See `age_min` in the [Basic Targeting Documentation](https://developers.facebook.com/docs/marketing-api/audiences/reference/basic-targeting#demographics).
#' @param age_max Maximum age. Default is 65. See `age_max` in the [Basic Targeting Documentation](https://developers.facebook.com/docs/marketing-api/audiences/reference/basic-targeting#demographics).
#' ## Credentials
#' @param version API version. e.g., "v14.0"
#' @param creation_act Facebook creation act
#' @param token Facebook API token
#' ## Scraping parameters
#' @param sleep_time words
#' @param show_result words
#' ## Return query text as variable in returned dataframe
#' @param add_query If `TRUE`, add query text as variable in returned dataframe 
#' @param add_query_hide_credentials If `TRUE` (and `add_query` is `TRUE`), hide the `creation_act` and `token` from the query text returned in the dataframe
#' 
#' @return Dataframe that includes (1) daily and monthly active users and (2) parameter values
#' 
#' @seealso [get_fb_parameters()] To get IDs and descriptions for behaviors, demographics, and interests.
#' @export
query_fb_marketing_api <- function(location_type,
                                   lat_lon = NULL,
                                   radius = NULL,
                                   radius_unit = NULL,
                                   country_iso2 = NULL,
                                   locales = NULL,
                                   behavior = NULL,
                                   interest = NULL,
                                   relationship_statuses = NULL, 
                                   life_events = NULL, 
                                   industries = NULL, 
                                   income = NULL, 
                                   family_statuses = NULL,
                                   education_statuses = NULL,
                                   user_os = NULL,
                                   wireless_carrier = NULL,
                                   gender = c(1,2),
                                   age_min = 18,
                                   age_max = 65,
                                   version, 
                                   creation_act, 
                                   token,
                                   sleep_time = 1,
                                   show_result = T,
                                   add_query = F,
                                   add_query_hide_credentials = T){
  
  # Checks -----------------------------------------------------------------------
  if(length(location_type) != 1) stop("'location_type' must be a vector of length one, either 'coordinates' or 'country'; only one option allowed")
  
  # Convert param inputs to list -------------------------------------------------
  convert_to_list <- function(x){
    # Converts to list if not a list
    if(!is.list(x)) x <- list(x)
    return(x)
  }
  
  lat_lon               <- lat_lon               %>% convert_to_list()
  radius                <- radius                %>% convert_to_list()
  radius_unit           <- radius_unit           %>% convert_to_list()
  country_iso2          <- country_iso2          %>% convert_to_list()
  locales               <- locales               %>% convert_to_list()
  behavior              <- behavior              %>% convert_to_list()
  interest              <- interest              %>% convert_to_list()
  relationship_statuses <- relationship_statuses %>% convert_to_list()
  life_events           <- life_events           %>% convert_to_list()
  industries            <- industries            %>% convert_to_list()
  income                <- income                %>% convert_to_list()
  family_statuses       <- family_statuses       %>% convert_to_list()
  education_statuses    <- education_statuses    %>% convert_to_list()
  user_os               <- user_os               %>% convert_to_list()
  wireless_carrier      <- wireless_carrier      %>% convert_to_list()
  gender                <- gender                %>% convert_to_list()
  age_min               <- age_min               %>% convert_to_list()
  age_max               <- age_max               %>% convert_to_list()
  
  # Length parameter inputs to same length ---------------------------------------
  n_param_combn <- length(lat_lon) * 
    length(radius) *
    length(radius_unit) *
    length(country_iso2) *
    length(locales) *
    length(behavior) *
    length(interest) *
    length(relationship_statuses) *
    length(life_events) *
    length(industries) *
    length(income) *
    length(family_statuses) *
    length(education_statuses) *
    length(user_os) *
    length(wireless_carrier) *
    length(gender) *
    length(age_min) *
    length(age_max)
  
  lat_lon               <- rep(lat_lon,               length = n_param_combn)
  radius                <- rep(radius,                length = n_param_combn)
  radius_unit           <- rep(radius_unit,           length = n_param_combn)
  locales               <- rep(locales,               length = n_param_combn)
  behavior              <- rep(behavior,              length = n_param_combn)
  interest              <- rep(interest,              length = n_param_combn)
  relationship_statuses <- rep(relationship_statuses, length = n_param_combn)
  life_events           <- rep(life_events,           length = n_param_combn)
  industries            <- rep(industries,            length = n_param_combn)
  income                <- rep(income,                length = n_param_combn)
  family_statuses       <- rep(family_statuses,       length = n_param_combn)
  education_statuses    <- rep(education_statuses,    length = n_param_combn)
  user_os               <- rep(user_os,               length = n_param_combn)
  wireless_carrier      <- rep(wireless_carrier,      length = n_param_combn)
  gender                <- rep(gender,                length = n_param_combn)
  age_min               <- rep(age_min,               length = n_param_combn)
  age_max               <- rep(age_max,               length = n_param_combn)
  
  # Length parameter inputs to same length -------------------------------------
  out_df <- mapply(query_fb_marketing_api_1call,
                   lat_lon               = lat_lon,
                   radius                = radius,
                   radius_unit           = radius_unit,
                   locales               = locales,
                   behavior              = behavior,
                   interest              = interest,
                   relationship_statuses = relationship_statuses,
                   life_events           = life_events,
                   industries            = industries,
                   income                = income,
                   family_statuses       = family_statuses,
                   education_statuses    = education_statuses,
                   user_os               = user_os,
                   wireless_carrier      = wireless_carrier,
                   gender                = gender,
                   age_min               = age_min,
                   age_max               = age_max,
                   MoreArgs = list(location_type = location_type,
                                   sleep_time    = sleep_time,
                                   show_result   = show_result,
                                   version       = version,
                                   creation_act  = creation_act,
                                   token         = token),
                   SIMPLIFY = F
  ) %>% 
    bind_rows()
  
}

