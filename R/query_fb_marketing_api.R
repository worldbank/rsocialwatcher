#' @import dplyr
#' @import lubridate
#' @import jsonlite
#' @import httr
#' @import stringr
#' @import splitstackshape

# TODO
# 1. Start documentation (vignette)
# 2. Test ALL parameter inputs (and illustrate) [including -- which are flex_target? relationship_statuses?]
# 3. Singular vs plural? Standardize
# 4. Add parameter names to dataframe
# 5. Make geojson of locations (see if feasible to add as option, so exports as sf?)

library(dplyr)
library(lubridate)
library(jsonlite)
library(httr)
library(stringr)
library(splitstackshape) 

if(F){
  remove.packages("rSocialWatcher")
  devtools::install_github("ramarty/rSocialWatcher")
  
  roxygen2::roxygenise("~/Documents/Github/rSocialWatcher")
}

# TODO:
# 2. And/Or
# 3. More geolocation options; other types (region, etc etc -- implement all)
#   -- Allow searching...
#   --SEE HERE: https://developers.facebook.com/docs/marketing-api/audiences/reference/basic-targeting
# 6. See very bottom: https://developers.facebook.com/docs/marketing-api/audiences/reference/targeting-search#geo
# 8. Add examples
# 9. Function to add names, from IDs (don't do for location?)

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

#' Map Parameters
#' @param ... Vector or list
#' @export
map_param <- function(...){
  # Function will create a separate query for each item. 
  # Creates a list, where the first element in the list is "map_param", where the
  # function then interprets each element of the list as a separate query.
  # Complex queries can still be made:
  # map_param
  
  if(is.list(c(...))){
    l <- list(...)
  } else{
    l <- as.list(c(...))
  }
  
  #l <- as.list(c(...))
  l <- as.list(c("map_param", l))
  return(l)
}

group <- function(...){
  list(c(...))
}

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

make_query_nonflex_params <- function(location_unit_type = NULL,
                                      lat_lon = NULL,
                                      location_types = NULL,
                                      radius = NULL,
                                      radius_unit = NULL,
                                      location_keys = NULL,
                                      gender_param = NULL,
                                      age_min = NULL,
                                      age_max = NULL,
                                      version = NULL,
                                      creation_act = NULL,
                                      token = NULL){
  
  location_types <- paste0("'",location_types,"'") %>% paste(collapse = ",")
  
  #### Build location query
  if(location_unit_type == "coordinates"){
    latitude  <- lat_lon[1]
    longitude <- lat_lon[2]
    
    query_location <- paste0("'geo_locations':{'location_types':[",location_types,"],'custom_locations':[{'latitude':",
                             latitude %>% substring(1,7),",",
                             "'longitude':",
                             longitude %>% substring(1,7),",",
                             "'radius':",
                             radius,",",
                             "'distance_unit':'",radius_unit,"'}]},")
  } else if (location_unit_type %in% c("countries", "country_groups")){
    query_location <- paste0("'geo_locations':{'",location_unit_type,"':[",
                             paste0("'",location_keys,"'") %>% paste(collapse = ","),
                             "],'location_types':[",location_types,"]},")
  } else if (location_unit_type %in% c("regions","electoral_districts","zips","geo_markets")){
    query_location <- paste0("'geo_locations':{'",location_unit_type,"':[",
                             paste0("{'key':'",location_keys,"'}") %>% paste(collapse = ","),
                             "],'location_types':[",location_types,"]},")
  } else if ( (location_unit_type %in% c("cities")) & is.null(radius)){
    query_location <- paste0("'geo_locations':{'",location_unit_type,"':[",
                             paste0("{'key':'",location_keys,"'}") %>% 
                               paste(collapse = ","),
                             "],'location_types':[",location_types,"]},")
  } else if ( (location_unit_type %in% c("cities", "places")) & !is.null(radius)){
    query_location <- paste0("'geo_locations':{'",location_unit_type,"':[",
                             paste0("{'key':'",location_keys,"','radius':",radius,",'distance_unit':'",radius_unit,"'}") %>% 
                               paste(collapse = ","),
                             "],'location_types':[",location_types,"]},")
  }
  
  #### Add non-flexible parameters
  query <- paste0("https://graph.facebook.com/",version,
                  "/act_",creation_act,
                  "/delivery_estimate?access_token=",token,
                  "&include_headers=false&method=get&pretty=0&suppress_http_code=1&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1&targeting_spec={",
                  query_location,
                  "'genders':[",gender_param,"],", 
                  "'age_min':",age_min,",",
                  "'age_max':",age_max)
  
  return(query)
}

prep_param <- function(param,
                       add_id){
  
  if(is_null_or_na(param)){
    param <- NULL
  } else if (add_id == T){
    param <- paste0("{'id':", param, "}") %>% paste(collapse = ",")
  } else if (add_id == F){
    param <- param %>% paste(collapse = ",")
  }
  
  return(param)
}

rm_blank <- function(x){
  x[x != ""]
}

add_comma_if_not_blank <- function(x){
  if(x != ""){
    x <- paste0(x, ",")
  }
  
  return(x)
}

make_flex_spec_or <- function(param,
                              name,
                              add_id){
  
  param_clean <- prep_param(param, add_id = add_id)
  
  out <- ifelse(is.null(param_clean), "", 
                paste0("'",name,"':[", param_clean, "]")) 
  
  return(out)
}

make_flex_spec <- function(param, 
                           name){
  
  if(name == "interests")             add_id <- T
  if(name == "behaviors")             add_id <- T
  if(name == "college_years")         add_id <- T # NA ??
  if(name == "education_majors")      add_id <- T # NA ??
  if(name == "education_schools")     add_id <- T # NA ??
  if(name == "education_statuses")    add_id <- F
  if(name == "family_statuses")       add_id <- T # NA ??
  if(name == "income")                add_id <- T # NA ??
  if(name == "industries")            add_id <- T # NA ??
  if(name == "work_positions")        add_id <- T # NA ??
  if(name == "work_employers")        add_id <- T # NA ??
  if(name == "relationship_statuses") add_id <- F
  
  # life_events_param     <- prep_param(life_events, add_id = T)
  # industries_param      <- prep_param(industries, add_id = T)
  # income_param          <- prep_param(income, add_id = T)
  # family_statuses_param <- prep_param(family_statuses, add_id = T)
  # 
  # relationship_statuses_param <- prep_param(relationship_statuses, add_id = F)
  # education_statuses_param    <- prep_param(education_statuses, add_id = F)
  # locales_param               <- prep_param(locales, add_id = F)
  
  # If not a list, make a list. If not a list, then just a vector -- so don't need
  # apply over multiple entries; just need to apply once
  if(!is.list(param)){
    param <- list(param)
  }
  
  out <- lapply(param, make_flex_spec_or, name, add_id) %>% 
    rm_blank() %>% 
    paste(collapse = ",") %>%
    add_comma_if_not_blank()
  
  return(out)
}

make_iterable <- function(x){
  
  if(!is.null(x)){
    
    if(x[[1]][1] == "map_param"){
      # Already an iterable list; take away the "map_param" identifier
      x <- x[x != "map_param"]
      
    } else{
      # Not an iterable list; put in a list, so just iterate over once
      x <- list(x)
      
    }
    
  } else{
    x <- list(x)
  }
  
  return(x)
}

param_str_or <- function(x){
  
  if(length(x) == 1){
    x_or <- x
  } else{
    x_or <- x %>% paste(collapse = " or ")
  }
  
  return(x_or)
}

param_str_and_or <- function(params){
  
  lapply(params, function(x){
    
    x_or <- param_str_or(x)
    if(length(x) > 1){
      x_or <- paste0("(", x_or, ")")
    }
    
    return(x_or)
  }) %>%
    paste(collapse = " and ")
  
}

param_str <- function(params){
  
  if(is.list(params)){
    out <- param_str_and_or(params)
  } else{
    out <- param_str_or(params)
  }
  
  if(out == ""){
    out <- NULL
  }
  
  return(out)
}

add_name_to_param <- function(params){
  lapply(1:length(params), function(i){
    param_i <- params[i]
    
    paste(names(param_i), param_i[[1]], sep = ":") 
    
  })
}


param_str_flex_target <- function(params){
  
  if(n_embedded_lists(params) == 1){
    param_out <- add_name_to_param(params) %>%
      unlist() %>%
      param_str()
  }
  
  if(n_embedded_lists(params) == 2){
    
    param_out <- lapply(params, function(param_i){
      add_name_to_param(param_i) %>%
        unlist() 
    }) %>%
      param_str()
  }
  
  return(param_out)
}

# Query: 1 API CAll ------------------------------------------------------------
query_fb_marketing_api_1call <- function(location_unit_type,
                                         lat_lon,
                                         radius,
                                         radius_unit,
                                         location_keys,
                                         location_types,
                                         locales,
                                         
                                         #### Specify here or in flex_targeting
                                         interests,
                                         behaviors,
                                         college_years,
                                         education_majors,
                                         education_schools,
                                         education_statuses,
                                         family_statuses,
                                         income,
                                         industries,
                                         work_positions,
                                         work_employers,
                                         
                                         ## Exclude 
                                         excl_interests,
                                         excl_behaviors,
                                         excl_college_years,
                                         excl_education_majors,
                                         excl_education_schools,
                                         excl_education_statuses,
                                         excl_family_statuses,
                                         excl_income,
                                         excl_industries,
                                         excl_work_positions,
                                         excl_work_employers,
                                         
                                         ## Non Flex Targetting Parameters
                                         relationship_statuses, 
                                         life_events, 
                                         user_os,
                                         wireless_carrier,
                                         gender,
                                         age_min,
                                         age_max,
                                         
                                         flex_target,
                                         
                                         ## API Keys/Info
                                         version, 
                                         creation_act, 
                                         token,
                                         
                                         ## Query info
                                         sleep_time,
                                         show_result,
                                         
                                         ## Add to dataframe
                                         add_param_id_name_vars,
                                         add_query,
                                         add_query_hide_credentials){
  
  # Checks ---------------------------------------------------------------------
  
  if(!is.null(lat_lon)){
    if(is.list(lat_lon)){
      stop('\"lat_lon\" cannot be a list')
    }
  }
  
  if(location_unit_type == "coordinates"){
    if(length(lat_lon) != 2 ) stop("'lat_lon' must be a vector of length 2, with latitude then longitude")
    if(is.null(radius))      stop("Must enter numeric value for 'radius'")
    if(is.null(radius_unit)) stop("Must enter 'kilometer' or 'mile' for 'radius_unit'")
  }
  
  if(location_unit_type != "coordinates"){
    if(is.null(location_keys)) stop("Must enter value for 'location_keys'")
  }
  
  if(!is.null(radius_unit)){
    if(!(radius_unit %in% c("mile", "kilometer"))) stop("Invalid 'radius_unit'; if specify radius_unit, must be either 'mile' or 'kilometer'")
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
  
  if(location_unit_type == "places"){
    if(is.null(radius))      stop("'radius' not spacified. When location_unit_type = 'places', must specify radius (and radius_unit).")
    if(is.null(radius_unit)) stop("'radius_unit' not spacified. When location_unit_type = 'places', must specify radius_unit (and radius).")
  }
  
  if(location_unit_type %in% c("countries","regions","zips","geo_markets","electoral_district","country_groups")){
    if(!is.null(radius))      stop(paste0("'radius' parameter not allowed when location_unit_type = '", location_unit_type, "'"))
    if(!is.null(radius_unit)) stop(paste0("'radius_unit' parameter not allowed when location_unit_type = '", location_unit_type, "'"))
  }
  
  if(location_unit_type %in% c("coordinates","places")){
    
    if(radius_unit == "mile"){
      if(radius > 50)   stop("Radius too large; radius must be between 0.63 and 50 miles when location_unit_type = '",location_unit_type,"'.")
      if(radius < 0.63) stop("Radius too small; radius must be between 0.63 and 50 miles when location_unit_type = '",location_unit_type,"'.")
    }
    
    if(radius_unit == "kilometer"){
      if(radius > 80) stop("Radius too large; radius must be between 1 and 80 kilometers when location_unit_type = '",location_unit_type,"'.")
      if(radius < 1)  stop("Radius too small; radius must be between 1 and 80 kilometers when location_unit_type = '",location_unit_type,"'.")
    }
    
  }
  
  if((location_unit_type %in% c("cities")) & (!is.null(radius))){
    
    if(radius_unit == "mile"){
      if(radius > 50) stop("Radius too large; if specify radius, radius must be between 0.63 and 50 miles when location_unit_type = '",location_unit_type,"'.")
      if(radius < 10) stop("Radius too small; if specify radius, radius must be between 0.63 and 50 miles when location_unit_type = '",location_unit_type,"'.")
    }
    
    if(radius_unit == "kilometer"){
      if(radius > 80) stop("Radius too large; if specify radius, radius must be between 1 and 80 kilometers when location_unit_type = '",location_unit_type,"'.")
      if(radius < 17) stop("Radius too small; if specify radius, radius must be between 1 and 80 kilometers when location_unit_type = '",location_unit_type,"'.")
    }
    
  }
  
  #### Location types
  location_types_stop_mssg <- 'Invalid "location_types"; "location_types" can either be: "home", "recent", "travel_in", or c("home", "recent")'
  if(length(location_types) == 1){
    if(!(location_types %in% c("home", "recent", "travel_in"))){
      stop(location_types_stop_mssg)
    }
  }
  if(length(location_types) == 2){
    location_types <- location_types %>% sort()
    
    if(paste0(location_types, collapse=",") != "home,recent"){
      stop(location_types_stop_mssg)
    }
  }
  if(length(location_types) >= 3){
    stop(location_types_stop_mssg)
  }
  
  #### flex_target
  flex_target_n_list <- n_embedded_lists(flex_target)
  if(flex_target_n_list >= 3){
    stop('"flex_target" cannot have 3 or more embedded lists')
  }
  
  # Check internet -------------------------------------------------------------
  # Stall if not connected to internet
  while(!curl::has_internet()){ Sys.sleep(5); print("Looking for internet")}
  
  # Make query -----------------------------------------------------------------
  
  #### Non-flex params ####
  query_all <- make_query_nonflex_params(location_unit_type = location_unit_type,
                                         lat_lon            = lat_lon,
                                         location_types     = location_types,
                                         radius             = radius,
                                         radius_unit        = radius_unit,
                                         location_keys      = location_keys,
                                         gender_param       = gender %>% paste(collapse = ","),
                                         age_min            = age_min,
                                         age_max            = age_max,
                                         version            = version, 
                                         creation_act       = creation_act, 
                                         token              = token)
  
  #### Flex params ####
  query_flex <- paste0(make_flex_spec(interests,          "interests"),
                       make_flex_spec(behaviors,          "behaviors"),
                       make_flex_spec(college_years,      "college_years"),
                       make_flex_spec(education_majors,   "education_majors"),
                       make_flex_spec(education_schools,  "education_schools"),
                       make_flex_spec(education_statuses, "education_statuses"),
                       make_flex_spec(family_statuses,    "family_statuses"),
                       make_flex_spec(income,             "income"),
                       make_flex_spec(industries,         "industries"),
                       make_flex_spec(work_positions,     "work_positions"),
                       make_flex_spec(work_employers,     "work_employers")) %>%
    str_replace_all(",$", "")
  
  #### Flex params - adv spec ####
  if(!is.null(flex_target)){
    
    ## Needs to have two list levels
    if(n_embedded_lists(flex_target) == 1){
      flex_target <- list(flex_target)
    }
    
    ## Make condition
    # Loop through list, where list elements separated as "and" condition
    query_flex_adv <- lapply(flex_target, function(flex_target_i){
      
      # Make "or" conditions for items within a list
      
      # Can't do lapply(flex_target_i), as that grabs the object using flex_target_i[[i]],
      # which does not keep the name (eg, $interests) -- where the name is needed. So 
      # use lapply(1:n), where can then do flex_target_i[i], which keeps the name
      params_or <- lapply(1:length(flex_target_i), function(i){
        flex_target_ii <- flex_target_i[i]
        
        make_flex_spec(flex_target_ii, 
                       names(flex_target_ii))
      }) %>%
        paste0(collapse = "") %>%
        str_replace_all(",$", "")
      
      # Wrap in curly brackets
      #params_or <- paste0("{", params_or, "}") # TODO: Delete??
    }) %>% 
      paste(collapse = ",")
    
    ## Add to query_flex object
    if(query_flex == ""){
      query_flex <- query_flex_adv
    } else{
      query_flex <- paste(query_flex, query_flex_adv, sep = ",")
    }
    
  }
  
  if(query_flex != ""){
    query_flex <- paste0("'flexible_spec':[{", query_flex, "}]")
    query_all <- paste0(query_all, ",", query_flex)
  }
  
  #### Exclusion parameters ####
  query_exclude <- paste0(make_flex_spec(excl_interests,          "interests"),
                          make_flex_spec(excl_behaviors,          "behaviors"),
                          make_flex_spec(excl_college_years,      "college_years"),
                          make_flex_spec(excl_education_majors,   "education_majors"),
                          make_flex_spec(excl_education_schools,  "education_schools"),
                          make_flex_spec(excl_education_statuses, "education_statuses"),
                          make_flex_spec(excl_family_statuses,    "family_statuses"),
                          make_flex_spec(excl_income,             "income"),
                          make_flex_spec(excl_industries,         "industries"),
                          make_flex_spec(excl_work_positions,     "work_positions"),
                          make_flex_spec(excl_work_employers,     "work_employers")) %>%
    str_replace_all(",$", "")
  
  if(query_exclude != ""){
    query_exclude <- paste0("'exclusions':{", query_exclude, "}")
    query_all <- paste0(query_all, ",", query_exclude)
  }
  
  #### Add ending curly bracket ####
  query <- query_all %>% paste0("}")
  
  # Make Query -----------------------------------------------------------------
  # Make query and prep dataframe with results and parameter
  try_api_call <- TRUE
  
  while(try_api_call){
    try_api_call <- FALSE
    
    query_val_df <- tryCatch({

      query_val <- url(query) %>% fromJSON
      
      #### If there is no error
      if(is.null(query_val$error)){
        
        ## Marketing info to dataframe
        query_val_df <- query_val$data
        query_val_df$daily_outcomes_curve <- NULL
        query_val_df$estimate_ready <- NULL
        
        #### Add parameter info
        ## Location params 
        query_val_df$location_unit_type <- location_unit_type %>% param_str()
        query_val_df$location_types     <- location_types     %>% param_str()
        query_val_df$radius             <- radius             %>% param_str()
        query_val_df$radius_unit        <- radius_unit        %>% param_str()
        query_val_df$location_keys      <- location_keys      %>% param_str()
        
        ## Flex targeting params
        query_val_df$interests          <- interests          %>% param_str()
        query_val_df$behaviors          <- behaviors          %>% param_str()
        query_val_df$college_years      <- college_years      %>% param_str()
        query_val_df$education_majors   <- education_majors   %>% param_str()
        query_val_df$education_schools  <- education_schools  %>% param_str()
        query_val_df$education_statuses <- education_statuses %>% param_str()
        query_val_df$family_statuses    <- family_statuses    %>% param_str()
        query_val_df$income             <- income             %>% param_str()
        query_val_df$industries         <- industries         %>% param_str()
        query_val_df$work_positions     <- work_positions     %>% param_str()
        query_val_df$work_employers     <- work_employers     %>% param_str()
        
        ## Exclude 
        query_val_df$excl_interests          <- excl_interests          %>% param_str()
        query_val_df$excl_behaviors          <- excl_behaviors          %>% param_str()
        query_val_df$excl_college_years      <- excl_college_years      %>% param_str()
        query_val_df$excl_education_majors   <- excl_education_majors   %>% param_str()
        query_val_df$excl_education_schools  <- excl_education_schools  %>% param_str()
        query_val_df$excl_education_statuses <- excl_education_statuses %>% param_str()
        query_val_df$excl_family_statuses    <- excl_family_statuses    %>% param_str()
        query_val_df$excl_income             <- excl_income             %>% param_str()
        query_val_df$excl_industries         <- excl_industries         %>% param_str()
        query_val_df$excl_work_positions     <- excl_work_positions     %>% param_str()
        query_val_df$excl_work_employers     <- excl_work_employers     %>% param_str()
        
        ## Non Flex Targetting Parameters
        query_val_df$relationship_statuses <- relationship_statuses %>% param_str()
        query_val_df$life_events           <- life_events           %>% param_str()
        query_val_df$user_os               <- user_os               %>% param_str()
        query_val_df$wireless_carrier      <- wireless_carrier      %>% param_str()
        query_val_df$gender                <- gender                %>% param_str()
        query_val_df$age_min               <- age_min               %>% param_str()
        query_val_df$age_max               <- age_max               %>% param_str()
        
        ## Flex Target Advanced
        query_val_df$flex_target           <- flex_target %>% param_str_flex_target()
        
        if(location_unit_type == "coordinates"){
          query_val_df$latitude  <- lat_lon[1]
          query_val_df$longitude <- lat_lon[2]
        }
        
        ## Add time
        query_val_df$api_call_time_utc <- Sys.time() %>% with_tz(tzone = "UTC")
        
        ## Add ID names
        # if(add_param_id_name_vars){
        #   query_val_df <- fill_fb_id_names(query_val_df,
        #                                    version = version,
        #                                    token = token)
        # }
        
        if(add_query){
          query_val_df$query <- query
          if(add_query_hide_credentials){
            query_val_df$query <- query_val_df$query %>%
              str_replace_all(paste0("act_", creation_act), "act_CREATION_ACT") %>%
              str_replace_all(paste0("access_token=", token), "access_token=TOKEN")
          }
          
        } 
        
        ## If no entry in dataframe ("" or NA), then remove the variable
        for(var in names(query_val_df)){
          if(is.na(query_val_df[[var]]))  query_val_df[[var]] <- NULL
          if(query_val_df[[var]] %in% "") query_val_df[[var]] <- NULL
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
        
        if(!is.null(query_val$error$code)){
          if((query_val$error$code == 80004)){
            try_api_call <- TRUE
            print("Too many calls, so pausing for 30 seconds then will try the query again; will only move to the next API query after the current query has successfully been called.")
            Sys.sleep(30)
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
      try_api_call <- F
      Sys.sleep(0.1)
      return(NULL)
    })
  }
  
  #### Return Result ####
  return(query_val_df)
}

# Query: ALL CALLS -------------------------------------------------------------
#' Query Facebook Marketing API
#' ## Location
#' @param location_unit_type Either `"coordinates"` (for buffer around single point) or `"country"`
#' ### If location_unit_type = "coordinates"
#' @param lat_lon Coordinates, c(lat, lon). For example, `c(38.90, -77.01)`
#' @param radius Radius around coordinate
#' @param radius_unit Unit for radius; either `"kilometer"` or `"mile"`
#' ### If location_unit_type = "country" 
#' @param country_code Country ISO2; for example, `"US"`.
#' ## Other location??
#' @param location_types Either: (1) `"home"` (people whose stated location in Facebook profile "current city" is in an area, valided by IP), (2) `"recent"` (people whose recent location is in the selected area, determined by mobile device data), (3) `"travel_in"` (people whose most recent location is in selected area and more than 100 miles from stated current city), (4) `c("home", "recent")` (for people in either category)
#' @param locales Words
#' ## Parameters. These are optional. If nothing specified, then searches for all users.
#' @param behavior Vector of behavior IDs. If multiple, uses `OR` condition; for example, `behavior = c(6002714895372, 6008297697383)` will target users who are either frequent travelers or returned from travels 2 weeks ago. Use `get_fb_parameters(type = "behaviors")` to get dataframe with IDs and descriptions. 
#' @param interest Vector of interest IDs. If multiple, uses `OR` condition; for example, `interest = c(6003349442621, 6003139266461)` will target users who are interested in either entertainment or movies. Use `get_fb_parameters(type = "interests")` to get dataframe with IDs and descriptions. 
#' @param relationship_statuses Vector of relationship status IDs. If multiple, uses `OR` condition; for example, `relationship_statuses = c(3,4)` targets those who are married or engaged. See `relationship_statuses` in the [Advanced Targeting Documentation](https://developers.facebook.com/docs/marketing-api/audiences/reference/advanced-targeting) to see relationship status ID options with descriptions. 
#' @param life_events Vector of life event IDs. If multiple, uses `OR` condition; for example, `life_events = c(6005149512172, 6005149512172)` targets users who recently moved or are in a new job. Use `get_fb_parameters(type = "demographics")` to get dataframe with IDs and descriptions. 
#' @param industries Vector of industries IDs. If multiple, uses `OR` condition; for example, `industries = c(6008888980183, 6008888972183)` targets users who work in sales or legal services. Use `get_fb_parameters(type = "demographics")` to get dataframe with IDs and descriptions. 
#' @param income Vector of income IDs. If multiple, uses `OR` condition; for example, `income = c(6107813553183, 6107813554583)` targets users with a household income in the top 10%-25% or 25%-50% of ZIP codes (US). Use `get_fb_parameters(type = "demographics")` to get dataframe with IDs and descriptions. 
#' @param family_statuses Vector of family status IDs. If multiple, uses `OR` condition; for example, `family_statuses = c(6023080302983, 6023005681983)` targets users who are parents with preteens or parents with teenagers. Use `get_fb_parameters(type = "demographics")` to get dataframe with IDs and descriptions. 
#' @param education_statuses Education status IDs. If multiple, uses `OR` condition; for example, `education_statuses = c(9,10)` will yeild those who report to have either a Master degree or professional degree. See `education_statuses` in the [Advanced Targeting Documentation](https://developers.facebook.com/docs/marketing-api/audiences/reference/advanced-targeting) to see education status options. 
#' @param locales Locales ID. 
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
#' @details FOR LOOP, USE LISTS. BUT JUST CAN'T LIST ON LOCATION TYPE.
#' @seealso [get_fb_parameters()] To get IDs and descriptions for behaviors, demographics, interests, and locales.
#' @export
query_fb_marketing_api <- function(location_unit_type,
                                   lat_lon = NULL,
                                   radius = NULL,
                                   radius_unit = NULL,
                                   location_keys = NULL,
                                   location_types = "home",
                                   locales = NULL,
                                   
                                   #### Specify here or in flex_targeting
                                   interests = NULL,
                                   behaviors = NULL,
                                   college_years = NULL,
                                   education_majors = NULL,
                                   education_schools = NULL,
                                   education_statuses = NULL,
                                   family_statuses = NULL,
                                   income = NULL,
                                   industries = NULL,
                                   work_positions = NULL,
                                   work_employers = NULL,
                                   
                                   ## Exclude 
                                   excl_interests = NULL,
                                   excl_behaviors = NULL,
                                   excl_college_years = NULL,
                                   excl_education_majors = NULL,
                                   excl_education_schools = NULL,
                                   excl_education_statuses = NULL,
                                   excl_family_statuses = NULL,
                                   excl_income = NULL,
                                   excl_industries = NULL,
                                   excl_work_positions = NULL,
                                   excl_work_employers = NULL,
                                   
                                   ## Non Flex Targetting Parameters
                                   relationship_statuses = NULL, 
                                   life_events = NULL, 
                                   user_os = NULL,
                                   wireless_carrier = NULL,
                                   gender = c(1,2),
                                   age_min = 18,
                                   age_max = 65,
                                   
                                   flex_target = NULL,
                                   
                                   ## API Keys/Info
                                   version, 
                                   creation_act, 
                                   token,
                                   
                                   ## Query info
                                   sleep_time = 0.1,
                                   show_result = T,
                                   
                                   ## Add to dataframe
                                   add_param_id_name_vars = F,
                                   add_query = F,
                                   add_query_hide_credentials = T){
  
  # Checks -----------------------------------------------------------------------
  
  if(length(location_unit_type) != 1){
    stop("'location_unit_type' must be a vector of length one; only one option allowed")
  } 
    
  if(!is.null(location_unit_type)){
    location_unit_type_valid_options <- c("countries", "country_groups", "regions", 
                                          "electoral_districts", "zips", "geo_markets", 
                                          "cities", "places")
    if(!(location_unit_type %in% location_unit_type_valid_options)){
      stop(paste0("Invalid `location_unit_type`; `location_unit_type` must be one of the following:\n",
                  paste(location_unit_type_valid_options, collapse = "\n")))
    }
  }
  
  
  
  
  
  # TODO: Checks for which ones can't have map_param
  
  # Convert param inputs to iterable list --------------------------------------
  ## Latitude/Longitude
  # Need to treat lat/lon bit differently as the input is a vector of lat/lon
  
  if(is.list(lat_lon)){
    
    if(lat_lon[[1]] == "map_param"){
      
      lat_lon <- lat_lon[lat_lon != "map_param"] %>% unlist()
      lat_lon <- split(lat_lon, ceiling(seq_along(lat_lon)/2))
    } else{
      stop('"lat_lon" cannot be a list')
    } 
    
  } else{
    lat_lon <- list(lat_lon)
  }
  
  ## Location Parameters
  radius         <- radius         %>% make_iterable()
  radius_unit    <- radius_unit    %>% make_iterable()
  location_keys  <- location_keys  %>% make_iterable()
  location_types <- location_types %>% make_iterable()
  locales        <- locales        %>% make_iterable()
  
  ## Flex targetting parameters
  interests          <- interests          %>% make_iterable()
  behaviors          <- behaviors          %>% make_iterable()
  college_years      <- college_years      %>% make_iterable()
  education_majors   <- education_majors   %>% make_iterable()
  education_schools  <- education_schools  %>% make_iterable()
  education_statuses <- education_statuses %>% make_iterable()
  family_statuses    <- family_statuses    %>% make_iterable()
  income             <- income             %>% make_iterable()
  industries         <- industries         %>% make_iterable()
  work_positions     <- work_positions     %>% make_iterable()
  work_employers     <- work_employers     %>% make_iterable()
  
  ## Exclude parameters
  excl_interests          <- excl_interests          %>% make_iterable()
  excl_behaviors          <- excl_behaviors          %>% make_iterable()
  excl_college_years      <- excl_college_years      %>% make_iterable()
  excl_education_majors   <- excl_education_majors   %>% make_iterable()
  excl_education_schools  <- excl_education_schools  %>% make_iterable()
  excl_education_statuses <- excl_education_statuses %>% make_iterable()
  excl_family_statuses    <- excl_family_statuses    %>% make_iterable()
  excl_income             <- excl_income             %>% make_iterable()
  excl_industries         <- excl_industries         %>% make_iterable()
  excl_work_positions     <- excl_work_positions     %>% make_iterable()
  excl_work_employers     <- excl_work_employers     %>% make_iterable()
  
  ## Non flex targetting parameters
  relationship_statuses <- relationship_statuses %>% make_iterable() 
  life_events           <- life_events           %>% make_iterable() 
  user_os               <- user_os               %>% make_iterable()
  wireless_carrier      <- wireless_carrier      %>% make_iterable()
  gender                <- gender                %>% make_iterable()
  age_min               <- age_min               %>% make_iterable()
  age_max               <- age_max               %>% make_iterable()
  
  ## Flex target, advanced
  flex_target <- flex_target %>% make_iterable()
  
  # Length parameter inputs to same length ---------------------------------------
  param_grid_df <- expand.grid(lat_lon        = lat_lon, 
                               radius         = radius, 
                               radius_unit    = radius_unit, 
                               location_keys  = location_keys, 
                               location_types = location_types, 
                               locales        = locales, 
                               
                               ## Flex targetting parameters
                               interests          = interests, 
                               behaviors          = behaviors, 
                               college_years      = college_years, 
                               education_majors   = education_majors, 
                               education_schools  = education_schools, 
                               education_statuses = education_statuses, 
                               family_statuses    = family_statuses, 
                               income             = income, 
                               industries         = industries, 
                               work_positions     = work_positions, 
                               work_employers     = work_employers, 
                               
                               ## Exclude parameters
                               excl_interests          = excl_interests,
                               excl_behaviors          = excl_behaviors,
                               excl_college_years      = excl_college_years,
                               excl_education_majors   = excl_education_majors, 
                               excl_education_schools  = excl_education_schools,
                               excl_education_statuses = excl_education_statuses, 
                               excl_family_statuses    = excl_family_statuses,
                               excl_income             = excl_income, 
                               excl_industries         = excl_industries, 
                               excl_work_positions     = excl_work_positions, 
                               excl_work_employers     = excl_work_employers, 
                               
                               ## Non flex targetting parameters
                               relationship_statuses = relationship_statuses,
                               life_events           = life_events,
                               user_os               = user_os,
                               wireless_carrier      = wireless_carrier,
                               gender                = gender,
                               age_min               = age_min, 
                               age_max               = age_max, 
                               
                               ## Flex target, advanced
                               flex_target = flex_target)
  
  # Length parameter inputs to same length -------------------------------------
  out_df <- mapply(query_fb_marketing_api_1call,
                   lat_lon        = param_grid_df$lat_lon, 
                   radius         = param_grid_df$radius, 
                   radius_unit    = param_grid_df$radius_unit, 
                   location_keys  = param_grid_df$location_keys, 
                   location_types = param_grid_df$location_types, 
                   locales        = param_grid_df$locales, 
                   
                   ## Flex targeting parameters
                   interests          = param_grid_df$interests, 
                   behaviors          = param_grid_df$behaviors, 
                   college_years      = param_grid_df$college_years, 
                   education_majors   = param_grid_df$education_majors, 
                   education_schools  = param_grid_df$education_schools, 
                   education_statuses = param_grid_df$education_statuses, 
                   family_statuses    = param_grid_df$family_statuses, 
                   income             = param_grid_df$income, 
                   industries         = param_grid_df$industries, 
                   work_positions     = param_grid_df$work_positions, 
                   work_employers     = param_grid_df$work_employers, 
                   
                   ## Exclude parameters
                   excl_interests          = param_grid_df$excl_interests,
                   excl_behaviors          = param_grid_df$excl_behaviors,
                   excl_college_years      = param_grid_df$excl_college_years,
                   excl_education_majors   = param_grid_df$excl_education_majors, 
                   excl_education_schools  = param_grid_df$excl_education_schools,
                   excl_education_statuses = param_grid_df$excl_education_statuses, 
                   excl_family_statuses    = param_grid_df$excl_family_statuses,
                   excl_income             = param_grid_df$excl_income, 
                   excl_industries         = param_grid_df$excl_industries, 
                   excl_work_positions     = param_grid_df$excl_work_positions, 
                   excl_work_employers     = param_grid_df$excl_work_employers, 
                   
                   ## Non flex targetting parameters
                   relationship_statuses = param_grid_df$relationship_statuses,
                   life_events           = param_grid_df$life_events,
                   user_os               = param_grid_df$user_os,
                   wireless_carrier      = param_grid_df$wireless_carrier,
                   gender                = param_grid_df$gender,
                   age_min               = param_grid_df$age_min, 
                   age_max               = param_grid_df$age_max, 
                   
                   ## Flex target, advanced
                   flex_target = param_grid_df$flex_target,
                   
                   MoreArgs = list(location_unit_type         = location_unit_type,
                                   add_param_id_name_vars     = add_param_id_name_vars,
                                   sleep_time                 = sleep_time,
                                   show_result                = show_result,
                                   add_query                  = add_query, 
                                   add_query_hide_credentials = add_query_hide_credentials,
                                   version                    = version,
                                   creation_act               = creation_act,
                                   token                      = token),
                   
                   SIMPLIFY = F
  ) %>% 
    bind_rows()
  
  return(out_df)
}

# Testing ----------------------------------------------------------------------
#### Default parameters ####
if(F){
  lat_lon = NULL
  radius = NULL
  radius_unit = NULL
  location_keys = NULL
  location_types = "home"
  locales = NULL
  
  #### Specify here or in flex_targeting
  interests = NULL
  behaviors = NULL
  college_years = NULL
  education_majors = NULL
  education_schools = NULL
  education_statuses = NULL
  family_statuses = NULL
  income = NULL
  industries = NULL
  work_positions = NULL
  work_employers = NULL
  
  ## Exclude 
  excl_interests = NULL
  excl_behaviors = NULL
  excl_college_years = NULL
  excl_education_majors = NULL
  excl_education_schools = NULL
  excl_education_statuses = NULL
  excl_family_statuses = NULL
  excl_income = NULL
  excl_industries = NULL
  excl_work_positions = NULL
  excl_work_employers = NULL
  
  ## Non Flex Targetting Parameters
  relationship_statuses = NULL
  life_events = NULL 
  #industries = NULL, 
  user_os = NULL
  wireless_carrier = NULL
  gender = c(1,2)
  age_min = 18
  age_max = 65
  
  flex_target = NULL
  
  ## API Keys/Info
  version = VERSION 
  creation_act = CREATION_ACT
  token = TOKEN
  
  ## Query info
  sleep_time = 20
  show_result = T
  
  ## Add to dataframe
  add_param_id_name_vars = F
  add_query = F
  add_query_hide_credentials = T
  
  #### Test Function ####
  out1 <- query_fb_marketing_api(location_unit_type = "coordinates",
                                 lat_lon = c(38.913744, -77.040018),
                                 radius = 10,
                                 radius_unit = "kilometer",
                                 location_types = "home",
                                 behaviors = list(6002714895372, 6002714898572),
                                 interests = c(6002839660079, 6002866718622),
                                 excl_interests = c(6002868910910, 6002884511422),
                                 excl_behaviors = c(6003986707172, 6003966451572),
                                 version = VERSION,
                                 creation_act = CREATION_ACT,
                                 token = TOKEN,
                                 add_query = T,
                                 add_query_hide_credentials = F)
  
  out2 <- query_fb_marketing_api(location_unit_type = "coordinates",
                                 lat_lon = c(38.913744, -77.040018),
                                 radius = 10,
                                 radius_unit = "kilometer",
                                 location_types = "home",
                                 
                                 flex_target = list(
                                   list(interests = c(6002839660079, 6002866718622),
                                        behaviors = c(6002714895372)),
                                   list(interests = c(6002868910910, 6002884511422),
                                        behaviors = c(6003986707172, 6003966451572))
                                 ),
                                 
                                 version = VERSION,
                                 creation_act = CREATION_ACT,
                                 token = TOKEN,
                                 add_query = T,
                                 add_query_hide_credentials = F)
  
  out2 <- query_fb_marketing_api(location_unit_type = "coordinates",
                                 lat_lon = c(38.913744, -77.040018),
                                 radius = 10,
                                 radius_unit = "kilometer",
                                 location_types = "home",
                                 
                                 flex_target = map_param(list(
                                   list(interests = c(6002839660079, 6002866718622),
                                        behaviors = c(6002714895372)),
                                   list(interests = c(6002868910910, 6002884511422),
                                        behaviors = c(6003986707172, 6003966451572))
                                 ),
                                 list(
                                   list(interests = c(6002839660079, 6002866718622),
                                        behaviors = c(6002714895372)),
                                   list(interests = c(6002868910910, 6002884511422),
                                        behaviors = c(6003986707172, 6003966451572))
                                 )),
                                 
                                 version = VERSION,
                                 creation_act = CREATION_ACT,
                                 token = TOKEN,
                                 add_query = T,
                                 add_query_hide_credentials = F)
}
