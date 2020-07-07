context("Testing freezing")

min_duration = 2

test_that("navr search stops is the same as freezing", {
  freezes_navr <- search_stops(obj_single$position, speed_threshold = 10,
                               min_duration = min_duration)
  freezes_fgu <- collect_freezes(obj_single, speed_threshold = 10, 
                                 min_duration = min_duration)
  expect_identical(freezes_navr$time_since_start, freezes_fgu$time_since_start)
})

test_that("collecting freezes returns proper object", {
  freezes <- collect_freezes(obj_single, speed_threshold = 10, 
                             min_duration = min_duration)
  expect_setequal(c("time_since_start", "time", "duration"), names(freezes))
})

test_that("freezes with higher speed return more than lower speeds", {
  freezes_high <- collect_freezes(obj_single, speed_threshold = 10,
                                  min_duration = min_duration)
  freezes_low <- collect_freezes(obj_single, speed_threshold = 1,
                                 min_duration = min_duration)
  expect_lt(length(freezes_low$time), length(freezes_high$time))
})

test_that("No fishiness", {
  freezes <- collect_freezes(obj_single, speed_threshold = 0, 
                             min_duration = 100)
  expect_equal(length(freezes$time), 0)
  freezes <- collect_freezes(obj_single, speed_threshold = 100, 
                             min_duration = Inf)
  expect_equal(length(freezes$time), 0)
  freezes <- collect_freezes(obj_single, speed_threshold = -1, 
                             min_duration = 1)
  expect_equal(length(freezes$time), 0)
})