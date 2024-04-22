#' Map Parameters
#' Converts a vector or list into a `map_param` object. When a `map_param` object is entered as a parameter in `query_fb_marketing_api()`, `query_fb_marketing_api()` makes a separate query for each item within `map_param`. 
#' 
#' @param ... Vector or list
#' @return Object of class `map_param`; a list structured that contains different parameters for the `query_fb_marketing_api()` function.
#' @examples
#' \dontrun{
#' # Make 3 queries:
#' # 1. Number of males and females MAU/DAU
#' # 2. Number of male MAU/DAU
#' # 3. Number of female MAU/DAU
#' query_fb_marketing_api(
#'   location_unit_type = "countries",
#'   location_keys      = "US",
#'   gender             = map_param(c(1,2), 1, 2),
#'   version            = VERSION, 
#'   creation_act       = CREATION_ACT, 
#'   token              = TOKEN)
#' }
#' @export
#' 
map_param <- function(...){
  # Function will create a separate query for each item. 
  # Creates a list, where the first element in the list is "map_param", where the
  # function then interprets each element of the list as a separate query.
  # Complex queries can still be made:
  # map_param
  
  OUT <- list(...)
  #OUT <- as.list(c("map_param", OUT))
  class(OUT) <- "map_param"
  
  return(OUT)
}

#' Map Parameters over Vector
#' Converts a vector into a `map_param` object. When a `map_param` object is entered as a parameter in `query_fb_marketing_api()`, `query_fb_marketing_api()` makes a separate query for each item within `map_param` (ie, for each item in the original vector entered into `map_param`). 
#' 
#' @param ... Vector
#' @return Object of class `map_param`; a list structured that contains different parameters for the `query_fb_marketing_api()` function.
#' @examples
#' \dontrun{
#' # Make 2 queries:
#' # 1. Number of male MAU/DAU
#' # 2. Number of female MAU/DAU
#' query_fb_marketing_api(
#'   location_unit_type = "countries",
#'   location_keys      = "US",
#'   gender             = map_param_vec(1:2),
#'   version            = VERSION, 
#'   creation_act       = CREATION_ACT, 
#'   token              = TOKEN)
#' }
#' @export
#' 
map_param_vec <- function(...){
  
  OUT <- list(...)
  #OUT <- as.list(c("map_param", OUT))
  OUT <- unlist(OUT)
  class(OUT) <- "map_param"
  
  return(OUT)
}

