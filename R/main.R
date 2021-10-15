
is_null_or_na <- function(x){
  # Return TRUE if x is NULL or NA; FALSE otherwise
  
  out <- FALSE
  
  if(is.null(x)) out <- TRUE
  
  if(!is.null(x)){
    if(TRUE %in% is.na(x)) out <- TRUE
  }
  
  return(out)
}

query_fb_marketing_api <- function(location_type,
                                   latitude = NULL,
                                   longitude = NULL,
                                   radius = NULL,
                                   radius_unit = NULL,
                                   country_iso2 = NULL,
                                   education_statuses = NULL,
                                   user_os = NULL,
                                   wireless_carrier = NULL,
                                   behavior = NULL,
                                   interest = NULL,
                                   gender = c(1,2),
                                   age_min = NULL,
                                   age_max = NULL,
                                   version, 
                                   creation_act, 
                                   token,
                                   other_vars_df = NULL,
                                   sleep_time = 20,
                                   show_result = T){
  
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
    if(is.null(latitude)) stop("Must enter numeric value for 'latitude'")
    if(is.null(longitude)) stop("Must enter numeric value for 'longitude'")
    if(is.null(radius)) stop("Must enter numeric value for 'radius'")
    if(is.null(radius_unit)) stop("Must enter numeric value for 'radius_unit'")
  }
  
  if(location_type == "country"){
    if(is.null(country_iso2)) stop("Must enter value for 'country_iso2'")
  }
  
  # Check internet -------------------------------------------------------------
  # Stall if not connected to internet
  while(!curl::has_internet()){ Sys.sleep(30); print("Looking for internet")}
  
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
                  ifelse(is.null(education_statuses_param), "", 
                         paste0("'education_statuses':[", education_statuses_param, "],")), 
                  ifelse(is.null(user_os_param), "", 
                         paste0("'user_os':[", user_os_param, "],")), 
                  ifelse(is.null(wireless_carrier_param), "", 
                         paste0("'wireless_carrier':[", wireless_carrier_param, "],")), 
                  ifelse(is.null(behavior_param), "", 
                         paste0("'behaviors':[", behavior_param, "],")), 
                  ifelse(is.null(interest_param), "", 
                         paste0("'interests':[", interest_param, "],")), 
                  "'genders':[",gender_param,"],", 
                  "'age_min':",age_min,",",
                  "'age_max':",age_max, 
                  "}")
  
  # Make query and prep dataframe with results and parameter
  query_val_df <- tryCatch({
    
    query_val <- url(query) %>% fromJSON
    query_val
    
    #### If there is no error
    if(is.null(query_val$error)){
      
      ## Marketing info to dataframe
      query_val_df <- query_val$data
      query_val_df$daily_outcomes_curve <- NULL
      
      ## Add parameter info
      query_val_df$location_type <- location_type
      query_val_df$latitude <- latitude
      query_val_df$longitude <- longitude
      query_val_df$radius <- radius
      query_val_df$radius_unit <- radius_unit
      query_val_df$country_iso2 <- country_iso2
      query_val_df$education_statuses <- education_statuses_param
      query_val_df$user_os <- user_os_param
      query_val_df$wireless_carrier <- wireless_carrier_param
      query_val_df$behavior <- behavior_param
      query_val_df$interest <- interest_param
      query_val_df$gender <- gender_param
      query_val_df$age_min <- age_min
      query_val_df$age_max <- age_max
      
      ## Add time
      query_val_df$api_call_time_utc <- Sys.time() %>% with_tz(tzone = "UTC")
      
      ## Print result and sleep (sleep needed b/c of rate limiting)
      if(show_result){
        print(paste0(query_val_df$estimate_mau," ", query_val_df$estimate_dau))
      }
      
      ## Sleep
      Sys.sleep(sleep_time) 
      
      #### If there is an error, print the error and make output null  
    } else{
      print(query_val)
      print("Checking error!")
      
      if(!is.null(query_val$error$code)){
        if((query_val$error$code == 80004) & param_i == 33){
          print("too many calls, so sleeping a bit!")
          Sys.sleep(10)
        } 
      }
      
      query_val_df <- ""
      
      # Sometimes lat/lon is not in a valid location. We still create a dataframe
      # for those
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

