# Load libraries
pacman::p_load(tidyverse, tidycensus, sf, osmdata, dodgr, here, extrafont)

#Options, call stored Census API key, load fonts
options(scipen = 1000, stringsAsFactors = F, tigris_use_cache = TRUE)
invisible(Sys.getenv("CENSUS_API_KEY"))

# Set working directory
setwd(here::here())

# Increase memory limit
memory.limit(16000)



# ===============
# Street Network
# ===============

### Import Data

# Chicago street network
streets_raw <- opq("Chicago, Illinois, USA") %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf() %>% 
  pluck("osm_lines") %>%
  select(osm_id, highway, name, lanes, maxspeed, geometry) %>%
  mutate(highway = "pedestrian")

# Weight by mode, remove streets_raw
graph <- weight_streetnet(streets_raw, wt_profile = "foot")

rm(streets_raw)



### Clean and Tidy

# Find nodes closest to centroid of tracts, need temporary sf object
graph_sf <- st_as_sf(graph, coords = c("from_lon", "from_lat"), crs = 4326)

lu_chi_tract_centroids <- st_read("../Data/Census/Chi_Tract_Centroids.shp")

nn_sf <- graph_sf %>% 
  filter(row_number() %in% st_nearest_feature(lu_chi_tract_centroids, graph_sf))

# Free up some space
rm(graph_sf, lu_chi_tract_centroids)

# Calculate isochrone points
isodists <- dodgr_isodists(graph,
                           from = nn_sf$from_id,
                           dlim = 800) # 800 meters, ~1/2 mile

# Get flows
fun_get_flows <- function(x) {
  
  to_ids <- isodists %>% filter(from == !!enquo(x)) %>% pull(id)
  isodists_rows <- isodists %>% filter(from == !!enquo(x)) %>% nrow()
  fmat <- matrix(1, nrow = 1, ncol = isodists_rows)
  
  output <- dodgr_flows_aggregate(graph,
                                  from = x,
                                  to = to_ids,
                                  flows = fmat) %>%
    merge_directed_graph() %>%
    dodgr_to_sf()
  
  return(output)
  
}

flows_sf <- map(.x = nn_sf$from_id, .f = fun_get_flows)