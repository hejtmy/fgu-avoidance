test_folder <- system.file("extdata", package = "fgu.avoidance")

test_that("Can load a single animal files", {
  fs <- file.path(test_folder, "run", "single-animal.CSV")
  obj <- load_data(fs)
  expect_s3_class(obj, "avoidance.multiple")
  expect_s3_class(obj_single, "avoidance.single")
  expect_match(names(obj), "animal_8")
})

test_that("Can load a single file with multiple recordings", {
  fs <- file.path(test_folder, "run", "three-animals.CSV")
  expect_silent(obj <- load_data(fs))
  expect_s3_class(obj, "avoidance.multiple")
  expect_length(obj, 3)
})

test_that("Can load folder with multliple files", {
  directory <- file.path(test_folder, "run") 
  expect_silent(obj <- load_folder(directory))
  expect_s3_class(obj, "avoidance.multiple")
  expect_length(obj, 4)
  expect_equal(head(obj$animal_8_2$position$data[, 1:5]), 
               head(obj$animal_8$position$data[, 1:5]))
})