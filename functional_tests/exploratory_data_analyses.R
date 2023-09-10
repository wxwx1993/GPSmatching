##      author:  Naeem Khoshnevis
##      created: June 2023
##      purpose: Reproducing examples in the paper.

# Load libraries
library(ggplot2)
library(CausalGPS)
library(data.table)

# Load data --------------------------------------------------------------------
data_file <- "zip_data.RData"
if (!file.exists(data_file)) {
  stop(paste0("Download the study data file from the following link:\n",
              "https://drive.google.com/file/d/",
              "1QFdbVU8Qir1gWf96c5h_ZhT-aPjhHpqn/view?usp=share_link"))
} else {
  load(local_path)
}

data.table::setDF(zip_data)
data <- zip_data

# Select confounders -----------------------------------------------------------
confounders   <- c("mean_bmi", "smoke_rate",
                   "hispanic", "pct_blk", "medhouseholdincome",
                   "medianhousevalue", "poverty", "popdensity",
                   "pct_owner_occ", "summer_tmmx", "winter_tmmx",
                   "summer_rmax", "winter_rmax", "year")

factor_cols <- c("year", "zip")
data[factor_cols] <- lapply(data[factor_cols], factor)


# Add id to the data
data$id <- 1:nrow(data)

# Summary of data
df <- data
sapply(names(df), function(colname) {
  coldata <- df[[colname]]

  # Check if the column is numeric
  if (is.numeric(coldata)) {
    cat(colname, "Min:", min(coldata), "Median:",
        median(coldata), "Max:", max(coldata), "\n")
  } else {
    cat(colname, "is not numeric\n")
  }
})

#write a code to print number of data samples
print(paste0("Number of data samples: ", nrow(data)))

#write a code to print number of unique zip codes
print(paste0("Number of unique zip codes: ", length(unique(data$zip))))


# Plot Exposure Density
quantiles <- quantile(data$pm25, probs = c(0.01, 0.99, 0.05, 0.95, 0.1, 0.9))


g <- ggplot(data, aes(x = pm25)) +
     geom_density(fill = "blue", alpha = 0.4) +
     geom_vline(aes(xintercept = quantiles[1]), 
                linetype = "dashed", color = "red", linewidth = 0.5) +
     geom_vline(aes(xintercept = quantiles[2]), 
                linetype = "dashed", color = "red", linewidth = 0.5) +
     geom_vline(aes(xintercept = quantiles[3]), 
                linetype = "dotdash", color = "green", linewidth = 0.5) +
     geom_vline(aes(xintercept = quantiles[4]), 
                linetype = "dotdash", color = "green", linewidth = 0.5) +
     geom_vline(aes(xintercept = quantiles[5]), 
                linetype = "longdash", color = "orange", linewidth = 0.5) +
     geom_vline(aes(xintercept = quantiles[6]), 
                linetype = "longdash", color = "orange", linewidth = 0.5) +
     labs(title = "Exposure density plot with percentile lines.") + 
     theme_minimal()

pdf("exposure_density.pdf")
plot(g)
dev.off()
