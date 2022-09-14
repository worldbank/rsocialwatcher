#' Merge Facebook Data with Parameter Name
#' 
#' The Facebook data will have an ID for a number of variables, where it is also
#' useful to have the name associated with the ID. Using a dataframe of Facebook
#' results and a parameter dataframe (that contains the names and ids), add
#' the names to the Facebook data. Deals with cases where there are multiple
#' ids used.
#' 
#' @param fb_df Facebook dataframe, returned by `query_fb_marketing_api()`
#' @param param_df Parameter dataframe, returned by `get_fb_parameter_ids()`
#' @param var Parameter variable name (in `fb_df`)
#' 
#' @return Facebook dataframe, with name of variable
merge_fb_with_param_df <- function(fb_df, 
                                   param_df,
                                   var){
  
  ## Prep dataframes
  fb_df <- fb_df %>%
    dplyr::mutate(tmp_id = 1:n())
  
  param_df <- param_df %>%
    dplyr::select(id, name) %>%
    dplyr::mutate(id = id %>% as.numeric())
  
  data_col_df <- fb_df %>%
    dplyr::select(tmp_id, all_of(var)) %>%
    splitstackshape::cSplit(var, ',') %>%
    pivot_longer(cols = -tmp_id,
                 names_to = "var",
                 values_to = "id") %>%
    left_join(param_df, by = "id") %>%
    dplyr::filter(!is.na(id)) %>%
    group_by(tmp_id) %>%
    dplyr::summarise(name = name %>% paste(collapse="; ")) %>%
    dplyr::select(tmp_id, name) 
  
  names(data_col_df)[names(data_col_df) == "name"] <- paste0(var, "_name")
  
  fb_df <- fb_df %>%
    left_join(data_col_df, by = "tmp_id") %>%
    dplyr::relocate(all_of(var), paste0(var, "_name"), everything()) %>%
    dplyr::select(-tmp_id)
  
  return(fb_df)
}

#' Fill Names of IDs
#'
#' The Facebook data contains variables with IDs, where it can be useful to have
#' the name of the associated ID. This function creates variables that contain the
#' the name. In cases where there are multiple IDs, the names are separated by a
#' semicolon. For example, if there is an `interest` variable with the 
#' observation: `"6003349442621,6003020834693"`, an `interest_name` variable
#' with "Entertainment; Music" would be created.
#'
#' @param fb_df Facebook dataframe, returned by `query_fb_marketing_api()`
#' @param version Facebook Marketing API version; for example, "v14.0"
#' @param token Facebook Marketing API token
#' 
#' @return Dataframe with Facebook results, where additional variables are created that indicate the name of IDs
#' 
#' @examples
#' \dontrun{
#' fb_df <- query_fb_marketing_api(location_unit_type = "coordinates",
#'                                 lat_lon = c(38.90, -77.01),
#'                                 radius = 10,
#'                                 radius_unit = "kilometer",
#'                                 interest = list(c(6002839660079,
#'                                                   6002884511422),
#'                                                 6002839660079,
#'                                                 6002884511422),
#'                                 behavior = list(c(6002714895372,
#'                                                   6002714898572),
#'                                                 6002714895372,
#'                                                 6002714898572),
#'                                 version = VERSION, 
#'                                 creation_act = CREATION_ACT, 
#'                                 token = TOKEN)
#'
#'fb_df <- fill_fb_id_names(fb_df,
#'                          version = VERSION,
#'                          token = TOKEN)
#' }
#' @export 
fill_fb_id_names <- function(fb_df,
                             version,
                             token){
  
  #### Interest
  if(!is.null(df_out$interest)){
    
    interests_df <- get_fb_parameter_ids(type = "interests",
                                         version = VERSION,
                                         token = TOKEN)
    
    fb_df <- fb_df %>%
      merge_fb_with_param_df(interests_df, "interest")
  }
  
  #### Behavior
  if(!is.null(df_out$interest)){
    
    behavior_df <- get_fb_parameter_ids(type = "behaviors",
                                        version = VERSION,
                                        token = TOKEN)
    
    fb_df <- fb_df %>%
      merge_fb_with_param_df(behavior_df, "behavior")
  }
  
  
  return(fb_df)
}





