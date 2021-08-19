test_that("testing cross collection", {
  expect_silent(res <- collect_crosses(obj))
  expect_equal(colnames(res), c("from", "to", "time", "index", "animal"))
  
  # SINGLE ---
  expect_silent(res <- collect_crosses(obj_single))
  
  expect_equal(colnames(res), c("from", "to", "time", "index"))
})
