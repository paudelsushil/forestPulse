# Split data into fire and nonfire
fire_data <- samplePlots %>%
filter(!plotLabel %in% c("nonFire"))

nonfire_data <- samplePlots %>%
filter(plotLabel == "nonFire")


fire_data %>% group_by(fireYrs)
