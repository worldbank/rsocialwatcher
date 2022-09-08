#' Get Suggested Radius
#' 
#' When targeting around a specific location, returns a suggested radius to reach enough people
#' 
#' @param location Vector of latitude and longitude (`c(lat, lon)`).
#' @param version Facebook Marketing API version; for example, "v14.0"
#' @param token Facebook Marketing API token
#' @param distance_unit Either `"kilometer"` or "`mile`"; defaults to "`kilometer`"
#' 
#' @return Dataframe with suggested radius and distance unit
#' 
#' @examples
#' \dontrun{
#' get_fb_suggested_radius(location = c(38.89831, -77.03658),
#'                         version  = "v14.0",
#'                         token    = "TOKEN-HERE")
#' }
#' 
#' @export
get_fb_suggested_radius <- function(location,
                                    version,
                                    token,
                                    distance_unit = "kilometer"){
  
  latitude  <- location[1]
  longitude <- location[1]
  
  out_df <- GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=list(
      type='adradiussuggestion',
      latitude=latitude,
      longitude=longitude,
      distance_unit=distance_unit,
      access_token=token
    )) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  return(out_df)
}

