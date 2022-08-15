##      author:  Naeem Khoshnevis
##      created: August 2022
##      purpose: Plot exposure data in the county level in the United States

##      USGrid is located here: https://drive.google.com/drive/folders/1i4G0NdMklePYphnkaM1sEXuJLrxI-voI
##      Please donwload the folder and put it inside the supplument folder.

# load required source codes and libraries

library(rgdal)
library(sf)

# Get synthetic data
data("synthetic_us_2010", package="CausalGPS")

# Read County shape file
fpath <- file.path("supplements/gz_2010_us_050_00_500k/")

county_shape_file <-  rgdal::readOGR(fpath)
county_shape_file <- spTransform(county_shape_file,
                                 CRS("+proj=longlat +datum=WGS84"))

# Compute only contiguous united states

# We are interested in contiguous states.
# Here is the list of states and their codes:
# https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
# non-contiguous states:
# 02: Alaska, 15: Hawaii, 60: American Samoa, 66: Guam,
# 69: Northern Mariana Islands, 72: Puerto Rico, 78: Virgin Islands

non_c_states <- c("02","15","60","66","69","72","78")
cs_inland <- county_shape_file[!county_shape_file$STATE %in% non_c_states, ]

# Add FIPS code
cs_inland$FIPS <- paste(cs_inland$STATE,cs_inland$COUNTY,sep = "")

# Fix FIPS code format of the synthetic data.
synthetic_us_2010$FIPS <-  sprintf("%05d",
                                   synthetic_us_2010$FIPS)

# Merge data based on FIPS code
merged_obj <- merge(cs_inland, synthetic_us_2010, by=c("FIPS"))

pdf("exposure_plot.pdf")
spplot(merged_obj, zcol = "qd_mean_pm25",
       col.regions=heat.colors(51, rev = TRUE),
       xlab="Longitude", ylab="Latitude",
       main="Mean PM2.5 in the Contiguous United States (2010)")
dev.off()


## Plot covariates

library(ggplot2)

# Plotting all cause mortality
g1 <- ggplot(data = synthetic_us_2010)
g1 <- g1 + geom_histogram(aes(cms_mortality_pct),
                          fill="blue",
                          alpha = 0.2,
                          color="black",
                          binwidth = 0.005)
g1 <- g1 + theme_bw()
g1 <- g1 + labs(x = "All Cause Mortality Rate",
                y = "Count")
plot(g1)

# Plotting BMI
g2 <- ggplot(data = synthetic_us_2010)
synthetic_us_2010$cdc_mean_bmi[synthetic_us_2010$cdc_mean_bmi > 9000] <- NA
g2 <- g2 + geom_histogram(aes(cdc_mean_bmi),
                          fill="green",
                          na.rm = TRUE,
                          alpha = 0.2,
                          color="black",
                          binwidth = 100)
g2 <- g2 + theme_bw()
g2 <- g2 + labs(x = "Body Mass Index",
                y = "Count")
plot(g2)

# Plotting race as boxpplot
race_data <- synthetic_us_2010[, c("cs_hispanic", "cs_white","cs_asian",
                                   "cs_black", "cs_native", "cs_other")]

race_data_g <- tidyr::gather(race_data)
g3 <- ggplot(race_data_g, aes(value, key, colour=key))
g3 <- g3 + geom_boxplot() + coord_flip()

