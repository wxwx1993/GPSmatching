##      author:  Naeem Khoshnevis
##      created: September 2023
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
  load(data_file)
}

data.table::setDF(zip_data)
data <- zip_data

# Add id to the data
data$id <- 1:nrow(data)

# Load Matched Data set --------------------------------------------------------
matched_data <- readRDS(
  "matching_3-matching-normal-q-0.1-0.9-seed-249-maxit-10-delta-1.9-sc-1-maximal-object.rds"
)

# Merge Matched data to outcome data

merged_data <- merge(data[, c("id", "education")],
                     matched_data$pseudo_pop,
                     by = "id")


head(merged_data)

# Exposure response curve ------------------------------------------------------
quant <- quantile(data$pm25, probs = c(0.1, 0.9))

start_time <- proc.time()
set.seed(290)
erf_obj <- estimate_npmetric_erf(merged_data$education,
                                 merged_data$pm25,
                                 merged_data$counter_weight,
                                 bw_seq=c(0.2, 1, 0.1),
                                 w_vals = seq(quant[1], quant[2], 0.1),
                                 nthread = 10,
                                 kernel_appr = "kernsmooth")
end_time <- proc.time()
write(paste0("ERF ",
             " | Wall clock time:  ",
             (end_time - start_time)[[3]]," seconds."),
      "wc_times.txt", append = TRUE)


pdf("erf_obj.pdf")
plot(erf_obj)
dev.off()
