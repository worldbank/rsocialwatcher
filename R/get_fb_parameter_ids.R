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
                                 key=NULL){
  
  # Checks ---------------------------------------------------------------------
  #if(!(type %in% c("behaviors", "demographics", "interests", "locales"))) stop("Invalid type; type must be either: 'behaviors', 'demographics', 'interests', or 'locales'")
  
  # Call API -----------------------------------------------------------------
  if(type %in% c("behaviors", "demographics", "interests")){
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        type='adTargetingCategory',
        class=type,
        access_token=token,
        limit=2000
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } else if (type %in% "locales"){
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        type='adlocale',
        access_token=token,
        limit=2000
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } else if (type %in% "job_titles"){
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        type='adworkposition',
        q=q,
        access_token=token,
        limit=5000
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } else if (type %in% "education_major"){
    if(is.null(q)) stop("'q' required")
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        type='adeducationmajor',
        q=q,
        access_token=token,
        limit=5000
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } else if (type %in% c("country", "country_group")){
    out_df <- GET(
      paste0("https://graph.facebook.com/",version,"/search"),
      query=list(
        location_types=type,
        type='adgeolocation',
        access_token=token,
        limit=300
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
        limit=3000
      )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  } 
  
  return(out_df)
}

