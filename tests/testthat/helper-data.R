test_folder <- system.file("extdata", package = "fgu.avoidance")
fs <- file.path(test_folder, "run", "three-animals.CSV")
obj <- load_data(fs)
obj_single <- obj$animal_8
