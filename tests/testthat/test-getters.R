test_folder <- system.file("extdata", package = "fgu.avoidance")
fs <- file.path(test_folder, "run", "three-animals.CSV")
obj <- load_data(fs)

test_that("testing animal selection", {
  animal_names <- names(obj)
  expect_error(selection <- filter_animals(obj, animal_names[1:2]), NA)
  expect_length(selection, 2)
  expect_s3_class(selection, "avoidance.multiple")
  expect_length(filter_animals(obj, animal_names[1]), 1)
  expect_length(filter_animals(obj, "Nobody would name animal in this way"), 0)
})