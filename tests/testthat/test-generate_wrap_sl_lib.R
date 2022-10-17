test_that("generate_wrap_sl_lib works as expected.", {


  wrap_result <- gen_wrap_sl_lib(lib_name = "m_xgboost",
                   params = list(xgb_max_depth = c(4),
                                 xgb_nrounds=c(20)),
                                 nthread = 1)

  expect_equal(length(wrap_result), 2L)

  # expect modifying library that is implemented
  expect_true(gen_wrap_sl_lib(lib_name = "m_xgboost",
                        params = list(xgb_max_depth = c(4),
                                      xgb_nrounds=c(20)),
                        nthread = 1
  )[[1]])

  m_xgb <- formals(m_xgboost_internal)
  expect_equal(m_xgb$nthread, 1)
  expect_equal(m_xgb$ntrees, 20)
  expect_equal(m_xgb$shrinkage, 0.3, tolerance = 0.001)
  expect_equal(m_xgb$max_depth, 4)
  expect_equal(m_xgb$verbose, 0)

  # expect false for library that is not implemented.
  expect_false(gen_wrap_sl_lib(lib_name = "m_library",
                              params = list(xgb_max_depth = c(3,4,5),
                                            xgb_nrounds=c(10,20,30,40,50,60)),
                              nthread = 1
  )[[1]])

  if (!requireNamespace("ranger", quietly = TRUE)) {
    expect_error(gen_wrap_sl_lib(lib_name = "m_ranger",
                                 params = list(rgr_num.trees = c(55)),
                                 nthread = 12
    )[[1]])} else {
    expect_true(gen_wrap_sl_lib(lib_name = "m_ranger",
                                params = list(rgr_num.trees = c(55)),
                                nthread = 12)[[1]])

    m_rgr <- formals(m_ranger_internal)
    expect_equal(m_rgr$num.threads, 12)
    expect_equal(m_rgr$num.trees, 55)
    expect_true(m_rgr$replace)
    }

})
