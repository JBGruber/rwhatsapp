context("test-rwhatsapp")

history <- system.file("extdata", "sample.txt", package = "rwhatsapp")

test_that("time is converted correctly", {
  expect_equal(
    rwa_read(txt = c(
      "12.07.17, 22:35:22: Johannes Gruber: Was it good?",
      "13.07.17, 09:12:44: R: Yes, it was"
    ))$time,
    structure(c(1499895322.017, 1499933564.017), class = c("POSIXct", "POSIXt")),
    tolerance = 1 # 1 seconds tolerance
  )
  expect_equal(
    rwa_read(txt = c(
      "12.07.17, 22:35:22 - Johannes Gruber: Was it good?",
      "13.07.17, 09:12:44 - R: Yes, it was"
    ))$time,
    structure(c(1499895322.708, 1499933564.708), class = c("POSIXct", "POSIXt")),
    tolerance = 1 # 1 seconds tolerance
  )
  expect_equal(
    rwa_read(txt = c(
      "12.07.17, 10:35 PM - Johannes Gruber: Was it good?",
      "13.07.17, 10:36 PM  - R: Yes, it was"
    ))$time,
    structure(c(1499852115.097, 1499938575.097), class = c("POSIXct", "POSIXt")),
    tolerance = 60 # 60 seconds tolerance
  )
  expect_equal(
    rwa_read(txt = c(
      "[20.09.17, 16:54:32] Johannes Gruber: Was it good?",
      "[20.09.17, 16:54:43] R: Yes, it was"
    ))$time,
    structure(c(1505922872.163, 1505922883.163), class = c("POSIXct", "POSIXt")),
    tolerance = 1 # 1 seconds tolerance
  )
})

test_that("See if author is converted correctly", {
  expect_equal(
    rwa_read(txt = c(
      "12.07.17, 22:35:22: Johannes Gruber: Was it good?",
      "13.07.17, 09:12:44: R: Yes, it was"
    ))$author,
    structure(1:2, .Label = c("Johannes Gruber", "R"), class = "factor")
  )
  expect_equal(
    rwa_read(txt = c(
      "12.07.17, 22:35:22 - Johannes Gruber: Was it good?",
      "13.07.17, 09:12:44 - R: Yes, it was"
    ))$author,
    structure(1:2, .Label = c("Johannes Gruber", "R"), class = "factor")
  )
  expect_equal(
    rwa_read(txt = c(
      "12.07.17, 10:35 PM - Johannes Gruber: Was it good?",
      "13.07.17, 10:36 PM  - R: Yes, it was"
    ))$author,
    structure(1:2, .Label = c("Johannes Gruber", "R"), class = "factor")
  )
  expect_equal(
    rwa_read(txt = c(
      "[20.09.17, 16:54:32] Johannes Gruber: Was it good?",
      "[20.09.17, 16:54:43] R: Yes, it was"
    ))$author,
    structure(1:2, .Label = c("Johannes Gruber", "R"), class = "factor")
  )
})
