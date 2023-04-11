
load("R/sysdata.rda")

data.table::setDF(pseudo_pop_covar_test)
# val <- absolute_corr_fun(pseudo_pop_covar_test[,2],
#                          pseudo_pop_covar_test[,4:length(pseudo_pop_covar_test)])
#
# expect_equal(val$mean_absolute_corr, 0.2826464, tolerance=0.0001)
# expect_equal(val$median_absolute_corr, 0.2850419, tolerance=0.0001)
# expect_equal(val$maximal_absolute_corr, 0.4719418, tolerance=0.0001)
# expect_equal(length(val$absolute_corr), 6)

# Test with categorical data
data2 <- data.table::setDF(pseudo_pop_weight_test)
val2 <- absolute_corr_fun(data2[,2],
                          data2[,5:12])
