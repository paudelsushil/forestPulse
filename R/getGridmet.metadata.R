lapply(c("rvest","stringr", "dplyr"), library, character.only = T)

getGridmet.metadata <- function(){
  base_url <- "https://www.northwestknowledge.net/metdata/data/"

  page <- tryCatch({
    rvest::read_html(base_url)
  }, error = function(e){
    stop("Unable to access the website !!!")
  })

  files <- page %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()

  variables <- files %>%
    stringr::str_extract("^[a-zA-Z]+(?=_\\d{4}\\.nc$)") %>%
    unique() %>%
    na.omit() %>%
    sort()

  meta_df <- data.frame(id = 1:length(variables),
                        variable = variables,
                        Description = c(
                          "BI (model-G)", "ERC (model-G)",
                          "Reference alfaalfa evapotranspiration",
                          "Reference grass evaportranspiration",
                          "precipitation_amount",
                          "relative_humidity",#max
                          "relative_humidity",#min
                          "Near-Surface Specific Humidity",
                          "surface_downwelling_shortwave_flux_in_air",
                          "Wind direction at 10 m",
                          "air_temperature",
                          "air_temperature",
                          "mean_vapor_pressure_deficit",
                          "wind_speed")

                        )



  return(meta_df)

}


