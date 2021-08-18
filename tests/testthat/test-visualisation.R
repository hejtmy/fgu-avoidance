test_that("testing visualising path", {
})

test_that("testing visualising crosses", {
})


test_that("testing visualising presence", {
  expect_warning(plot_area_presence(obj))
  
  expect_silent(plt <- plot_area_presence(obj_single))
  expect_s3_class(plt, "ggplot")
})