n_embedded_lists <- function(obj){
  # Count number of embedded lists in a list
  
  counter <- 0
  is_list <- T
  
  while(is_list){
    obj <- obj[[1]]
    
    counter <- counter + 1
    is_list <- is.list(obj)
  }
  
  return(counter)
}

#' Get Facebook Parameter IDs
#'
#' This function returns dataframes of Facebook parameters and their associated
#' IDs for different categories of information. Categories include behaviors,
#' interests, locales, job titles, education major, and location 
#' (e.g., country, city, zip code, etc). The returned dataframe contains ids 
#' that can be used in the query_fb_marketing_api function.
#'
#' @param type Type of data. Either: "behaviors", "demographics", "interests", "income", "industries", "life_events", "family_statuses", "work_positions", "work_employers", "education_statuses", "relationship_statuses", "education_majors", "locales", "country", "country_group", "region", "large_geo_area", "medium_geo_area", "small_geo_area", "city", "subcity", "neighborhood", "zip", "geo_market", "electoral_district", "zip"   
#' @param version Facebook Marketing API version; for example, "v14.0"
#' @param token Facebook Marketing API token
#' @param q Query string to limit search. For example, when searching job titles, setting `q="data"` will return jobs with "data" in the name, such as "data science."
#' @param country_code When searching locations, limit the search to a specific country; for example, only search for cities within a specific country.
#' @param region_id When searching locations, limit the search to a specific region; for example, only search for cities within a specific region.
#' @param key When searching locations, limit the search to a specific location key; for example, only search for neighborhood within a specific city.
#' @param limit Number of parameter IDs to search for. 
#' @param add_location_coords When querying location IDs (eg, when `type = "city`), add location coordinates---which will add the latitude and longitude, as well as the geometry when available. (Default: `FALSE`)
#' 
#' @details For additional information, see: https://developers.facebook.com/docs/marketing-api/audiences/reference/targeting-search/
#' @return Dataframe with parameter IDs and descriptions.
#' @examples
#' \dontrun{
#' 
#' #### Define version and token
#' VERSION = "enter-version"
#' TOKEN = "enter-token"
#' 
#' #### Query parameter IDs
#' get_fb_parameter_ids(type = "interests", version = VERSION, token = TOKEN)
#' get_fb_parameter_ids(type = "behaviors", version = VERSION, token = TOKEN)
#' get_fb_parameter_ids(type = "education_majors", q = "Computer", version = VERSION, token = TOKEN)
#' get_fb_parameter_ids(type = "education_schools", q = "Washington", version = VERSION, token = TOKEN)
#' get_fb_parameter_ids(type = "education_statuses", version = VERSION, token = TOKEN)
#' get_fb_parameter_ids(type = "family_statuses", version = VERSION, token = TOKEN)
#' get_fb_parameter_ids(type = "income", version = VERSION, token = TOKEN)
#' get_fb_parameter_ids(type = "industries", version = VERSION, token = TOKEN)
#' get_fb_parameter_ids(type = "work_positions", q = "Data", version = VERSION, token = TOKEN)
#' get_fb_parameter_ids(type = "work_employers", q = "World Bank", version = VERSION, token = TOKEN)
#' get_fb_parameter_ids(type = "relationship_statuses", version = VERSION, token = TOKEN)
#' get_fb_parameter_ids(type = "life_events", version = VERSION, token = TOKEN)
#' 
#' #### Location IDs
#' 
#' 
#' 
#' }
#' @export
get_fb_parameter_ids <- function(type,
                                 version,
                                 token,
                                 q = NULL,
                                 country_code = NULL,
                                 region_id = NULL,
                                 key=NULL,
                                 limit = NULL,
                                 add_location_coords = F){
  
  # Checks ---------------------------------------------------------------------
  #if(!(type %in% c("behaviors", "demographics", "interests", "locales"))) stop("Invalid type; type must be either: 'behaviors', 'demographics', 'interests', or 'locales'")
  #if( (type == "work_positions") & is.null(q)) stop("When type = 'work_positions', 'q' must be specified.")
  #if( (type == "work_employers") & is.null(q)) stop("When type = 'work_employers', 'q' must be specified.")
  
  valid_types <- c("behaviors", "demographics", "interests", "income", "industries", "life_events", "family_statuses",
                   "locales",
                   "work_positions",
                   "work_employers",
                   "education_statuses",
                   "relationship_statuses",
                   "education_majors",
                   "education_schools",
                   "country",
                   "country_group",
                   "region",
                   "large_geo_area",
                   "medium_geo_area",
                   "small_geo_area",
                   "city",
                   "subcity",
                   "neighborhood",
                   "zip",
                   "geo_market",
                   "electoral_district")
  
  if(!(type %in% valid_types)){
    stop(paste0("'type' not valid; 'type' must be one of the following:\n", valid_types %>% paste(collapse = "\n")))
  }
  
  # Call API -----------------------------------------------------------------
  if(type %in% c("behaviors", "demographics", "interests", "income", "industries", "life_events", "family_statuses")){
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        type='adTargetingCategory',
        class=type,
        access_token=token,
        limit = ifelse(is.null(limit), 2000, limit)
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } else if (type %in% "locales"){
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        type='adlocale',
        access_token=token,
        limit = ifelse(is.null(limit), 2000, limit)
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } else if (type %in% "work_positions"){
    if(is.null(q)) stop("'q' required")
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        type='adworkposition',
        q=q,
        access_token=token,
        limit = ifelse(is.null(limit), 5000, limit)
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } else if (type %in% "work_employers"){
    if(is.null(q)) stop("'q' required")
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        type='adworkemployer',
        q=q,
        access_token=token,
        limit = ifelse(is.null(limit), 5000, limit)
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } else if (type %in% "education_statuses"){
    
    out_df <- bind_rows(data.frame(id = 1, name = "HIGH_SCHOOL"),
                        data.frame(id = 2, name = "UNDERGRAD"),
                        data.frame(id = 3, name = "ALUM"),
                        data.frame(id = 4, name = "HIGH_SCHOOL_GRAD"),
                        data.frame(id = 5, name = "SOME_COLLEGE"),
                        data.frame(id = 6, name = "ASSOCIATE_DEGREE"),
                        data.frame(id = 7, name = "IN_GRAD_SCHOOL"),
                        data.frame(id = 8, name = "SOME_GRAD_SCHOOL"),
                        data.frame(id = 9, name = "MASTER_DEGREE"),
                        data.frame(id = 10, name = "PROFESSIONAL_DEGREE"),
                        data.frame(id = 11, name = "DOCTORATE_DEGREE"),
                        data.frame(id = 12, name = "UNSPECIFIED"),
                        data.frame(id = 13, name = "SOME_HIGH_SCHOOL"))
    
  } else if (type %in% "relationship_statuses"){
    
    out_df <- bind_rows(data.frame(id = 1, name = "single"),
                        data.frame(id = 2, name = "in_relationship"),
                        data.frame(id = 3, name = "married"),
                        data.frame(id = 4, name = "engaged"),
                        data.frame(id = 6, name = "not specified"),
                        data.frame(id = 7, name = "in a civil union"),
                        data.frame(id = 8, name = "in a domestic partnership"),
                        data.frame(id = 9, name = "In an open relationship"),
                        data.frame(id = 10, name = "It's complicated"),
                        data.frame(id = 11, name = "Separated"),
                        data.frame(id = 12, name = "Divorced"),
                        data.frame(id = 13, name = "Widowed"))
    
  } else if (type %in% "education_majors"){
    if(is.null(q)) stop("'q' required")
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        type='adeducationmajor',
        q=q,
        access_token=token,
        limit = ifelse(is.null(limit), 5000, limit)
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } else if (type %in% "education_schools"){
    if(is.null(q)) stop("'q' required")
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        type='adeducationschool',
        q=q,
        access_token=token,
        limit = ifelse(is.null(limit), 5000, limit)
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } else if (type %in% c("country", "country_group")){
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        location_types=type,
        type='adgeolocation',
        access_token=token,
        limit = ifelse(is.null(limit), 300, limit)
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } else if (type %in% c("region",
                         "large_geo_area",
                         "medium_geo_area",
                         "small_geo_area",
                         "city",
                         "subcity",
                         "neighborhood",
                         "zip",
                         "geo_market",
                         "electoral_district")){
    
    if(type %in% c("zip")){
      if(is.null(q)){
        stop("Parameter 'q' required")
      }
    }
    
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        location_types=type,
        type='adgeolocation',
        q=q,
        region_id=region_id,
        country_code=country_code,
        key=key,
        access_token=token,
        limit = ifelse(is.null(limit), 1000, limit)
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } 
  
  if(!is.null(out_df$code)){
    if(out_df$code != 200){
      warning(paste0("Error code: ", out_df$code ))
    }
  }
  
  if(!is.null(out_df$message)){
    warning(out_df$message)
  }
  
  if(length(out_df) == 0){
    out_df <- data.frame(NULL)
    warning("No results; may require adding or changing `q` parameter")
  }
  
  # Add location information ---------------------------------------------------
  if(add_location_coords){
    out_sf <- get_location_coords(location_unit_type = type,
                                  location_keys = out_df$key,
                                  version = version,
                                  token = token)
    
    vars_to_keep <- out_sf[!(names(out_sf) %in% names(out_df))] %>% names()
    vars_to_keep <- c("key", vars_to_keep)
    
    out_sf <- out_sf[,vars_to_keep]
    
    out_df <- out_df %>%
      left_join(out_sf, by = "key")
  }
  
  return(out_df)
}

