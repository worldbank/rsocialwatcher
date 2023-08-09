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
#' demographics, interests, locales, job titles, education major, and location 
#' (e.g., country, city, zip code, etc). The returned dataframe contains ids 
#' that can be used in the query_fb_marketing_api function.
#'
#' @param type Type of data; either "behaviors", "demographics", "interests", or "locales"
#' @param version Facebook Marketing API version; for example, "v14.0"
#' @param token Facebook Marketing API token
#' 
#' @return Dataframe with parameter IDs and descriptions.
#' @export
get_fb_parameter_ids <- function(type,
                                 version,
                                 token,
                                 q = NULL,
                                 country_code = NULL,
                                 region_id = NULL,
                                 key=NULL,
                                 limit = NULL){
  
  # Checks ---------------------------------------------------------------------
  #if(!(type %in% c("behaviors", "demographics", "interests", "locales"))) stop("Invalid type; type must be either: 'behaviors', 'demographics', 'interests', or 'locales'")
  if( (type == "job_titles") & is.null(q)) stop("When type = 'job_titles', 'q' must be specified.")
  
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
  } else if (type %in% "job_titles"){
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        type='adworkposition',
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
    
  } else if (type %in% "education_major"){
    if(is.null(q)) stop("'q' required")
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        type='adeducationmajor',
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
                         "metro_area",
                         "city",
                         "subcity",
                         "neighborhood",
                         "subneighborhood",
                         "zip",
                         "geo_market",
                         "electoral_district")){
    
    if(type %in% c("zip")){
      if(is.null(q)){
        #stop("Parameter 'q' required")
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
  
  if(is.null(nrow(out_df))){
    warning("No results; may require `q` parameter")
  }
  
  return(out_df)
}

