test_that("testing cross collection", {
  CROSS_NAMES <- c("from", "to", "time", "index", "animal")
  expect_silent(res <- collect_crosses(obj))
  expect_equal(colnames(res), CROSS_NAMES)
  
  # SINGLE ---
  expect_silent(res <- collect_crosses(obj_single))
  expect_equal(colnames(res), CROSS_NAMES)
})
