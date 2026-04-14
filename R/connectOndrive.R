
library(AzureGraph)
gr <- create_graph_login()

library("Microsoft365R")

id <- get_business_onedrive()

id$list_items()
conusStatesPath <- (id$get_item("ForDis/conus_states.gpkg"))
conusStatesPath

temp_dir <- here::here()
conusStatesPath$download(file.path(temp_dir, "conus_states.gpkg"))

# Read the file
conus_states <- sf::st_read(file.path(temp_dir, "conus_states.gpkg"))

temp_dir
