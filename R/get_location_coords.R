# Get Location Geometries

#' Query Facebook Marketing API
#' ## Location
#' @param location_unit_type Either `"coordinates"` (for buffer around single point) or type of geographic location, including: `"countries"`, `"regions"`, `"cities"`, `"zips"`, `"places"`, `"geo_markets"`, `"electoral_district"`, or `"country_groups"`. See the [Basic Targetting](https://developers.facebook.com/docs/marketing-api/audiences/reference/basic-targeting#location) documentation for more information. 
#' @param location_keys Key associated with location. Use the `get_fb_parameter_ids` function to get location keys; see [here](https://ramarty.github.io/rSocialWatcher/articles/rsocialwatcher-vignette.html#location-ids) for examples.
#' @param version API version. e.g., "v17.0"
#' @param token Facebook API token
#' 
#' @return Spatial features dataframe
#' 
#' @examples
#' \dontrun{
#' #### Define version, creation act, and token
#' VERSION = "enter-version"
#' TOKEN = "enter-token"
#' 
#' #### Grab locations
#' loc_sf <- get_location_coords(location_type = "countries",
#'                                  location_keys = c("US", "MX", "CA"),
#'                                  version = VERSION,
#'                                  token = TOKEN)
#' 
#' }
#' @export
get_location_coords <- function(location_unit_type,
                                location_keys,
                                version,
                                token){
  
  #### Checks
  valid_types <- c("countries",
                   "regions",
                   "cities",
                   "zips",                
                   "places",
                   "custom_locations",
                   "geo_markets",
                   "electoral_districts",
                   "country_groups",      
                   "subneighborhoods",    
                   "neighborhoods",       
                   "subcities",           
                   "metro_areas",        
                   "small_geo_areas",     
                   "medium_geo_areas",     
                   "large_geo_areas",     
                   "location_cluster_ids",
                   
                   "country",
                   "region",
                   "city",
                   "zip",                
                   "place",
                   "custom_location",
                   "geo_market",
                   "electoral_district",
                   "country_group",      
                   "subneighborhood",    
                   "neighborhood",       
                   "subcity",           
                   "metro_area",        
                   "small_geo_area",     
                   "medium_geo_area",     
                   "large_geo_area",     
                   "location_cluster_id")
  
  if(!(location_unit_type %in% valid_types)){
    stop(paste0("'", location_unit_type, "' is not a valid 'location_unit type'. Valid options include: \n", 
                paste(valid_types %>% sort(), collapse = "\n")))
  }
  
  #### Make plural
  if(location_unit_type == "country")             location_unit_type <- "countries"
  if(location_unit_type == "region")              location_unit_type <- "regions"
  if(location_unit_type == "city")                location_unit_type <- "cities"
  if(location_unit_type == "zip")                 location_unit_type <- "zips"
  if(location_unit_type == "place")               location_unit_type <- "places"
  if(location_unit_type == "custom_location")     location_unit_type <- "custom_locations"
  if(location_unit_type == "geo_market")          location_unit_type <- "geo_markets"
  if(location_unit_type == "electoral_district")  location_unit_type <- "electoral_districts"
  if(location_unit_type == "country_group")       location_unit_type <- "country_groups"
  if(location_unit_type == "subneighborhood")     location_unit_type <- "subneighborhoods"
  if(location_unit_type == "neighborhood")        location_unit_type <- "neighborhoods"
  if(location_unit_type == "subcity")             location_unit_type <- "subcities"
  if(location_unit_type == "metro_area")          location_unit_type <- "metro_areas"
  if(location_unit_type == "small_geo_area")      location_unit_type <- "small_geo_areas"
  if(location_unit_type == "medium_geo_area")     location_unit_type <- "medium_geo_areas"
  if(location_unit_type == "large_geo_area")      location_unit_type <- "large_geo_areas"
  if(location_unit_type == "location_cluster_id") location_unit_type <- "location_cluster_ids"
  
  #### Query data
  location_keys_str <- toJSON(location_keys)
  
  q_list <- list(
    type='adgeolocationmeta',
    access_token=token,
    show_polygons_and_coordinates="true",
    limit = ifelse(is.null(limit), 2000, limit)
  )
  q_list[[location_unit_type]] <- location_keys_str
  
  out_df <- GET(
    paste0("https://graph.facebook.com/",version,"/search"),
    query=q_list) %>% content(as="text") %>% fromJSON %>%. [[1]]
  
  loc_keys <- names(out_df[[location_unit_type]])
  
  #### Convert to sf object
  poly_all_sf <- map_df(loc_keys, function(loc_key_i){
    
    poly <- out_df[[location_unit_type]][[loc_key_i]]$polygons
    
    if(length(poly) == 0){
      poly_sf <- data.frame(matrix(nrow = 1, ncol = 0))
    } else{
      
      poly_sf <- map_df(1:length(poly), function(i){
        
        poly_i <- poly[[i]]
        poly_i <- poly_i %>%
          mutate(lng = lng %>% as.numeric,
                 lat = lat %>% as.numeric)
        
        # Combine the lng and lat columns into a matrix
        coords <- cbind(poly_i$lng, poly_i$lat)
        
        # Create a simple feature (sf) object with a polygon geometry
        polygon <- st_polygon(list(cbind(poly_i$lng, poly_i$lat)))
        sf_polygon <- st_sfc(polygon, crs = st_crs(4326)) %>%
          st_as_sf()
        
        return(sf_polygon)
      })
      
      poly_sf <- poly_sf %>%
        st_combine() %>%
        st_as_sf()
      
      poly_sf <- poly_sf %>%
        dplyr::rename(geometry = x)
      
    }
    
    poly_df <- out_df[[location_unit_type]][[loc_key_i]]
    poly_df$polygons <- NULL
    
    for(var in names(poly_df)){
      poly_sf[[var]] <- poly_df[[var]] %>% paste(collapse = ",")
    }
    
    return(poly_sf)
  })
  
  return(poly_all_sf)
}


