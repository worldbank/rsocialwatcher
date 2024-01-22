#' Get Suggested Radius
#' 
#' When targeting around a specific location, returns a suggested radius to reach enough people
#' 
#' @param location Vector of latitude and longitude (`c(lat, lon)`).
#' @param distance_unit Either `"kilometer"` or "`mile`"; defaults to "`kilometer`"
#' @param version Facebook Marketing API version; for example, "v14.0"
#' @param token Facebook Marketing API token
#' 
#' @return Dataframe with suggested radius and distance unit
#' 
#' @details For more information, see the [Facebook documentation here](https://developers.facebook.com/docs/marketing-api/audiences/reference/targeting-search/#radius)
#' 
#' @examples
#' \dontrun{
#' get_fb_suggested_radius(location = c(38.89831, -77.03658),
#'                         version  = "v14.0",
#'                         token    = "TOKEN-HERE")
#' }
#' 
get_fb_suggested_radius <- function(location,
                                    distance_unit = "kilometer",
                                    version,
                                    token){
  
  latitude  <- location[1]
  longitude <- location[2]
  
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

