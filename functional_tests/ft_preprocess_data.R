
set.seed(22)
n <- 4000
mydata <- generate_syn_data(sample_size=n)
year <- sample(x=c("2001","2002","2003","2004","2005"),size = n, replace = TRUE)
region <- sample(x=c("North", "South", "East", "West"),size = n, replace = TRUE)
mydata$year <- as.factor(year)
mydata$region <- as.factor(region)
mydata$cf5 <- as.factor(mydata$cf5)
set_logger(logger_level = "DEBUG")

id <- seq_along(1:nrow(mydata))

Y <- data.frame(id = id, Y = mydata$Y)
w <- data.frame(id = id, w = mydata$treat)
c <- data.frame(id = id, mydata[c("cf1","cf2","cf3","cf4","cf5","cf6","year","region")])


results <- preprocess_data(Y, w, c, c(0.1, 0.9))

original_data <- results$original_data
prep_data <- results$preprocessed_data

head(prep_data)
