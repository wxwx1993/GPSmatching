# Converting the function to be compatible with base R to be used
# in the python package.
#

absolute_weighted_corr_df <- function(w,
                                      vw,
                                      c_num,
                                      c_cat){
  # collect numerical column names
  col_n <- colnames(c_num)

  # collect factorial column names
  col_f <- colnames(c_cat)

  # double check factorial columns are factors.
  c_cat[] <- lapply(c_cat, as.factor)

  absolute_corr_n <- absolute_corr_f <- NULL

  if (length(col_n) > 0) {
    absolute_corr_n<- sapply(col_n,function(i){
      abs(wCorr::weightedCorr(x = w,
                              y = c_num[,i],
                              weights = vw,
                              method = c("spearman")))})
    absolute_corr_n <- unlist(absolute_corr_n)
    names(absolute_corr_n) <- col_n
  }

  if (length(col_f) > 0) {
    internal_fun<- function(i){
      abs(wCorr::weightedCorr(x = w,
                              y = c_cat[, i],
                              weights = vw,
                              method = c("Polyserial")))}

    absolute_corr_f <- c()
    for (item in col_f){
      if (length(unique(c_cat[[item]])) == 1 ){
        absolute_corr_f <- c(absolute_corr_f, NA)
      } else {
        absolute_corr_f <- c(absolute_corr_f, internal_fun(item))
      }
    }
    names(absolute_corr_f) <- col_f
  }

  absolute_corr <- c(absolute_corr_n, absolute_corr_f)

  df <- data.frame(name = character(), value = numeric())

  for (i in names(absolute_corr)){
    df <- rbind(df, data.frame(name=i, value=absolute_corr[[i]]))
  }

  if (sum(is.na(absolute_corr)) > 0){
    warning(paste("The following features generated missing values: ",
                  names(absolute_corr)[is.na(absolute_corr)],
                  "\nIn computing mean covariate balance, they will be ignored."))
  }
  return(df)
}

exposure <- read.csv("treatment.csv")
weight <- read.csv("weights.csv")
c_num <- read.csv("c_num.csv")
c_cat <- read.csv("c_cat.csv")

weight$X0[weight$X0 > 15] <- 15
weight$X0[weight$X0 < 0.01] <- 0.01

c_num$X <- NULL
c_cat$X <- NULL

data1 <- data.table::setDF(pseudo_pop_weight_test)
val1 <- absolute_weighted_corr_df(exposure$X0,
                                  weight$X0,
                                  c_num,
                                  c_cat)

print(val1)
