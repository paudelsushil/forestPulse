samplePlots <- sf::st_read(file.path(analyzed_src, "plots/v25.01.30/samplePlots.gpkg"))

n11 <- nrow(samplePlots %>% dplyr::filter(event == "fire" & outbreak == 1))
n12 <- nrow(samplePlots  %>% dplyr::filter(event == "fire" & outbreak == 0))
n21 <- nrow(samplePlots  %>% dplyr::filter(event == "non_fire" & outbreak == 1))
n22 <- nrow(samplePlots %>% dplyr::filter(event == "non_fire" & outbreak == 0))
# For Fire and Outbreak
# Assuming your data is in a contingency table format:
fire_outbreak <- matrix(c(
    # Replace these numbers with your actual counts
    n11, n12,  # n11: both fire and outbreak, n12: fire but no outbreak
    n21, n22   # n21: outbreak but no fire, n22: neither
), nrow = 2)

# Add row and column names
rownames(fire_outbreak) <- c("Fire", "No Fire")
colnames(fire_outbreak) <- c("Outbreak", "No Outbreak")

# Perform chi-square test
chisq_test_outbreak <- chisq.test(fire_outbreak)
print(chisq_test_outbreak)

# ========================================================================================

# For Fire and Fuel Management

m11 <- nrow(samplePlots %>% dplyr::filter(event == "fire" & fuelmgmt == 1))
m12 <- nrow(samplePlots %>% dplyr::filter(event == "fire" & fuelmgmt == 0))
m21 <- nrow(samplePlots %>% dplyr::filter(event == "non_fire" & fuelmgmt == 1))
m22 <- nrow(samplePlots %>% dplyr::filter(event == "non_fire" & fuelmgmt == 0))

fire_fuelmgmt <- matrix(c(
    # Replace these numbers with your actual counts
    m11, m12,  # m11: both fire and fuel management, m12: fire but no fuel management
    m21, m22   # m21: fuel management but no fire, m22: neither
), nrow = 2)

rownames(fire_fuelmgmt) <- c("Fire", "No Fire")
colnames(fire_fuelmgmt) <- c("Fuel Mgmt", "No Fuel Mgmt")

# Perform chi-square test
chisq_test_fuelmgmt <- chisq.test(fire_fuelmgmt)
print(chisq_test_fuelmgmt)



# Load epitools if available
if (!requireNamespace("epitools", quietly = TRUE)) {
    stop("Package 'epitools' is required.", call. = FALSE)
}
library(epitools)

# Calculate odds ratios
outbreak_odds <- oddsratio.fisher(fire_outbreak)
fuelmgmt_odds <- oddsratio.fisher(fire_fuelmgmt)



# If you want odds ratios
outbreak_odds <- oddsratio(fire_outbreak)
fuelmgmt_odds <- oddsratio(fire_fuelmgmt)

par(mfrow = c(1, 2))
# Create mosaic plots
plot_a <- mosaicplot(fire_outbreak, 
           main = "Fire vs Outbreak Association",
           color = TRUE)


plot_b <- mosaicplot(fire_fuelmgmt,
           main = "Fire vs Fuel Management Association",
           color = TRUE)



ggsave(file.path(fig_src, "v25_1_31/Chi-square_mosaicPlot.png")
                width = 7, height = 6, dpi = 500)
