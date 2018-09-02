context("test-rwhatsapp")

history <- system.file("extdata", "sample.txt", package = "rwhatsapp")

test_that("time is converted correctly", {
  expect_equal(
    rwa_read(txt = c(
      "12.07.17, 22:35:22: Johannes Gruber: Was it good?",
      "13.07.17, 09:12:44: R: Yes, it was"
    ), tz = "GMT")$time,
    structure(c(1499898922.18, 1499937164.18),
              tzone = "GMT",
              class = c("POSIXct", "POSIXt")),
    tolerance = 5 # 5 seconds tolerance
  )
  expect_equal(
    rwa_read(txt = c(
      "12.07.17, 22:35:22 - Johannes Gruber: Was it good?",
      "13.07.17, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    structure(c(1499898922.406, 1499937164.406),
              tzone = "GMT",
              class = c("POSIXct", "POSIXt")),
    tolerance = 5 # 5 seconds tolerance
  )
  expect_equal(
    rwa_read(txt = c(
      "12.07.17, 10:35 PM - Johannes Gruber: Was it good?",
      "13.07.17, 10:36 PM  - R: Yes, it was"
    ), tz = "GMT")$time,
    structure(c(-61614177899.875, -61614091439.875),
              tzone = "GMT",
              class = c("POSIXct", "POSIXt")),
    tolerance = 60 # 60 seconds tolerance
  )
  expect_equal(
    rwa_read(txt = c(
      "[20.09.17, 16:54:32] Johannes Gruber: Was it good?",
      "[20.09.17, 16:54:43] R: Yes, it was"
    ), tz = "GMT")$time,
    structure(c(1505926472.43, 1505926483.43),
              tzone = "GMT",
              class = c("POSIXct", "POSIXt")),
    tolerance = 5 # 5 seconds tolerance
  )
})

test_that("See if author is converted correctly", {
  expect_equal(
    rwa_read(txt = c(
      "12.07.17, 22:35:22: Johannes Gruber: Was it good?",
      "13.07.17, 09:12:44: R: Yes, it was"
    ))$author,
    structure(1:2, .Label = c("Johannes Gruber", "R"),
              class = "factor")
  )
  expect_equal(
    rwa_read(txt = c(
      "12.07.17, 22:35:22 - Johannes Gruber: Was it good?",
      "13.07.17, 09:12:44 - R: Yes, it was"
    ))$author,
    structure(1:2, .Label = c("Johannes Gruber", "R"),
              class = "factor")
  )
  expect_equal(
    rwa_read(txt = c(
      "12.07.17, 10:35 PM - Johannes Gruber: Was it good?",
      "13.07.17, 10:36 PM  - R: Yes, it was"
    ))$author,
    structure(1:2, .Label = c("Johannes Gruber", "R"),
              class = "factor")
  )
  expect_equal(
    rwa_read(txt = c(
      "[20.09.17, 16:54:32] Johannes Gruber: Was it good?",
      "[20.09.17, 16:54:43] R: Yes, it was"
    ))$author,
    structure(1:2, .Label = c("Johannes Gruber", "R"),
              class = "factor")
  )
})


test_that("reading from file", {
  expect_equal({
    rwa_read(txt = history, tz = "GMT")
    }, readRDS("../files/rwa_read.RDS"),
    tolerance = 60 # 60 seconds tolerance
  )
  expect_equal({
    rwa_read(txt = c(history, history), tz = "GMT")
  }, rbind(readRDS("../files/rwa_read.RDS"), readRDS("../files/rwa_read.RDS")),
  tolerance = 60 # 60 seconds tolerance
  )
})


test_that("warning", {
  expect_error(rwa_read(txt = 1),
               "Provide either a path to one or multiple txt files of a whatsapp history or the history itself as character object.")
})
