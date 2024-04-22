# Get Location Geometries

#' Get Coordinates/Geometries for Valid Location Keys
#' @param location_unit_type Either `"coordinates"` (for buffer around single point) or type of geographic location, including: `"countries"`, `"regions"`, `"cities"`, `"zips"`, `"places"`, `"geo_markets"`, `"electoral_district"`, or `"country_groups"`. See the [Basic Targetting](https://developers.facebook.com/docs/marketing-api/audiences/reference/basic-targeting#location) documentation for more information. 
#' @param location_keys Key associated with location. Use the `get_fb_parameter_ids` function to get location keys; see [here](https://worldbank.github.io/rsocialwatcher/articles/rsocialwatcher-vignette.html#location-ids) for examples.
#' @param version API version. e.g., "v19.0"
#' @param token Facebook API token
#' @param large_query_chunk_size The function will first try to query all locations using one API call. If too many locations are requested, the function will query in chunks. By default, the function will query 10 locations at a time. (Default: 10).
#' @param large_query_pause The function will first try to query all locations using one API call. If too many locations are requested, the function will query in chunks. After each query, the `large_query_pause` can be set to > 0 to sleep for `large_query_pause` seconds in order to not make too many API calls too quickly. (Default: 0).
#' @param limit Number of parameter IDs to search for. 
#' @param verbose If the function needs to make multiple queries to obtain location information for all location keys, print progress. (Default: `TRUE`).
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
                                token,
                                large_query_chunk_size = 10,
                                large_query_pause = 0,
                                limit = NULL){
  
  #### Query data
  out_sf <- get_location_coords_i(location_unit_type,
                                  location_keys,
                                  version,
                                  token,
                                  limit = limit)
  
  #### Query in chunks if too many observations
  if(length(out_sf) == 1){
    if(out_sf == "CODE_1"){
      location_keys_list <- split(location_keys, 
                                  rep(1:length(location_keys), 
                                      each = large_query_chunk_size, 
                                      length.out = length(location_keys)))
      
      out_sf <- map_df(location_keys_list, function(loc_keys_i){
        if(verbose){
          cat(paste0("Querying coordinates/geometries for location keys: ", paste(loc_keys_i, collapse = ", ") ))
        }
        
        out_sf_i <- get_location_coords_i(location_unit_type,
                                          loc_keys_i,
                                          version,
                                          token,
                                          limit = limit)
        
        if(out_sf_i == "CODE_1"){
          stop("Querying too many locations")
        }
        
        Sys.sleep(large_query_pause)
        
        return(out_sf_i)
      })
    }
  } 
  
  return(out_sf)
}

get_location_coords_i <- function(location_unit_type,
                                  location_keys,
                                  version,
                                  token,
                                  limit){
  
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
    query=q_list) %>% content(as="text") %>% fromJSON %>% pluck(1)
  
  if(!is.null(out_df$message)){
    if(out_df$code != 1){
      stop(out_df$message)
    }
  }
  
  if(!is.null(out_df$message)){
    if(out_df$code == 1){
      assign_code_1 <- T
    }
  } else{
    assign_code_1 <- F
  }
  
  if(assign_code_1){
    poly_all_sf = "CODE_1"
    
  } else{
    
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
            mutate(lng = .data$lng %>% as.numeric,
                   lat = .data$lat %>% as.numeric)
          
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
          dplyr::rename(geometry = .data$x)
        
      }
      
      poly_df <- out_df[[location_unit_type]][[loc_key_i]]
      poly_df$polygons <- NULL
      
      for(var in names(poly_df)){
        poly_sf[[var]] <- poly_df[[var]] %>% paste(collapse = ",")
      }
      
      if(!is.null(poly_sf$latitude))  poly_sf$latitude  <- poly_sf$latitude %>% as.numeric()
      if(!is.null(poly_sf$longitude)) poly_sf$longitude <- poly_sf$longitude %>% as.numeric()
      
      return(poly_sf)
    })
    
  }
  
  return(poly_all_sf)
}


