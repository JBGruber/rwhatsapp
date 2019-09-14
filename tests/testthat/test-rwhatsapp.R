context("test-rwhatsapp")

history <- system.file("extdata", "sample.txt", package = "rwhatsapp")
converted <- structure(c(1499898922.226, 1499937164.226),
                       tzone = "GMT",
                       class = c("POSIXct",
                                 "POSIXt"))

test_that("time is converted correctly", {
  # standard
  # dd.MM.yyyy, hh:mm:ss a
  expect_equal(
    rwa_read(x = c(
      "12.07.2017, 10:35:22 PM - Johannes Gruber: Was it good?",
      "13.07.2017, 09:12:44 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5 # 5 seconds tolerance
  )
  # dd.MM.yyyy, hh:mm a
  expect_equal(
    rwa_read(x = c(
      "12.07.2017, 10:35 PM - Johannes Gruber: Was it good?",
      "13.07.2017, 09:12 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60 # 60 seconds tolerance
  )
  # dd.MM.yyyy, HH:mm:ss
  expect_equal(
    rwa_read(x = c(
      "12.07.2017, 22:35:22 - Johannes Gruber: Was it good?",
      "13.07.2017, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # dd.MM.yyyy, HH:mm
  expect_equal(
    rwa_read(x = c(
      "12.07.2017, 22:35 - Johannes Gruber: Was it good?",
      "13.07.2017, 09:12 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # MM.dd.yyyy, hh:mm:ss a
  expect_equal(
    rwa_read(x = c(
      "07.12.2017, 10:35:22 PM - Johannes Gruber: Was it good?",
      "07.13.2017, 09:12:44 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # MM.dd.yyyy, hh:mm a
  expect_equal(
    rwa_read(x = c(
      "07.12.2017, 10:35 PM - Johannes Gruber: Was it good?",
      "07.13.2017, 09:12 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # MM.dd.yyyy, HH:mm:ss
  expect_equal(
    rwa_read(x = c(
      "07.12.2017, 22:35:22 - Johannes Gruber: Was it good?",
      "07.13.2017, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # MM.dd.yyyy, HH:mm
  expect_equal(
    rwa_read(x = c(
      "07.12.2017, 22:35 - Johannes Gruber: Was it good?",
      "07.13.2017, 09:12 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  ##### two-digit year
  # dd.MM.yy, hh:mm:ss a
  expect_equal(
    rwa_read(x = c(
      "12.07.17, 10:35:22 PM - Johannes Gruber: Was it good?",
      "13.07.17, 09:12:44 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # dd.MM.yy, hh:mm a
  expect_equal(
    rwa_read(x = c(
      "12.07.17, 10:35 PM - Johannes Gruber: Was it good?",
      "13.07.17, 09:12 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # dd.MM.yy, HH:mm:ss
  expect_equal(
    rwa_read(x = c(
      "12.07.17, 22:35:22 - Johannes Gruber: Was it good?",
      "13.07.17, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # dd.MM.yy, HH:mm
  expect_equal(
    rwa_read(x = c(
      "12.07.17, 22:35 - Johannes Gruber: Was it good?",
      "13.07.17, 09:12 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # MM.dd.yy, hh:mm:ss a
  expect_equal(
    rwa_read(x = c(
      "07.12.17, 10:35:22 PM - Johannes Gruber: Was it good?",
      "07.13.17, 09:12:44 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # MM.dd.yy, hh:mm a
  expect_equal(
    rwa_read(x = c(
      "07.12.17, 10:35 PM - Johannes Gruber: Was it good?",
      "07.13.17, 09:12 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # MM.dd.yy, HH:mm:ss
  expect_equal(
    rwa_read(x = c(
      "07.12.17, 22:35:22 - Johannes Gruber: Was it good?",
      "07.13.17, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # MM.dd.yy, HH:mm
  expect_equal(
    rwa_read(x = c(
      "07.12.17, 22:35 - Johannes Gruber: Was it good?",
      "07.13.17, 09:12 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  ##### /
  # dd/MM/yyyy, hh:mm:ss a
  expect_equal(
    rwa_read(x = c(
      "12/07/2017, 10:35:22 PM - Johannes Gruber: Was it good?",
      "13/07/2017, 09:12:44 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # dd/MM/yyyy, hh:mm a
  expect_equal(
    rwa_read(x = c(
      "12/07/2017, 10:35 PM - Johannes Gruber: Was it good?",
      "13/07/2017, 09:12 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # dd/MM/yyyy, HH:mm:ss
  expect_equal(
    rwa_read(x = c(
      "12/07/2017, 22:35:22 - Johannes Gruber: Was it good?",
      "13/07/2017, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # dd/MM/yyyy, HH:mm
  expect_equal(
    rwa_read(x = c(
      "12/07/2017, 22:35 - Johannes Gruber: Was it good?",
      "13/07/2017, 09:12 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # MM/dd/yyyy, hh:mm:ss a
  expect_equal(
    rwa_read(x = c(
      "07/12/2017, 10:35:22 PM - Johannes Gruber: Was it good?",
      "07/13/2017, 09:12:44 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # MM/dd/yyyy, hh:mm a
  expect_equal(
    rwa_read(x = c(
      "07/12/2017, 10:35 PM - Johannes Gruber: Was it good?",
      "07/13/2017, 09:12 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # MM/dd/yyyy, HH:mm:ss
  expect_equal(
    rwa_read(x = c(
      "07/12/2017, 22:35:22 - Johannes Gruber: Was it good?",
      "07/13/2017, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # MM/dd/yyyy, HH:mm
  expect_equal(
    rwa_read(x = c(
      "07/12/2017, 22:35 - Johannes Gruber: Was it good?",
      "07/13/2017, 09:12 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  ##### / + two-digit year
  # dd/MM/yy, hh:mm:ss a
  expect_equal(
    rwa_read(x = c(
      "12/07/17, 10:35:22 PM - Johannes Gruber: Was it good?",
      "13/07/17, 09:12:44 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # dd/MM/yy, hh:mm a
  expect_equal(
    rwa_read(x = c(
      "12/07/17, 10:35 PM - Johannes Gruber: Was it good?",
      "13/07/17, 09:12 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # dd/MM/yy, HH:mm:ss
  expect_equal(
    rwa_read(x = c(
      "12/07/17, 22:35:22 - Johannes Gruber: Was it good?",
      "13/07/17, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # dd/MM/yy, HH:mm
  expect_equal(
    rwa_read(x = c(
      "12/07/17, 22:35 - Johannes Gruber: Was it good?",
      "13/07/17, 09:12 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # MM/dd/yy, hh:mm:ss a
  expect_equal(
    rwa_read(x = c(
      "07/12/17, 10:35:22 PM - Johannes Gruber: Was it good?",
      "07/13/17, 09:12:44 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # MM/dd/yy, hh:mm a
  expect_equal(
    rwa_read(x = c(
      "07/12/17, 10:35 PM - Johannes Gruber: Was it good?",
      "07/13/17, 09:12 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # MM/dd/yy, HH:mm:ss
  expect_equal(
    rwa_read(x = c(
      "07/12/17, 22:35:22 - Johannes Gruber: Was it good?",
      "07/13/17, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # MM/dd/yy, HH:mm
  expect_equal(
    rwa_read(x = c(
      "07/12/17, 22:35 - Johannes Gruber: Was it good?",
      "07/13/17, 09:12 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  ##### -
  # dd-MM-yyyy, hh:mm:ss a
  expect_equal(
    rwa_read(x = c(
      "12-07-2017, 10:35:22 PM - Johannes Gruber: Was it good?",
      "13-07-2017, 09:12:44 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # dd-MM-yyyy, hh:mm a
  expect_equal(
    rwa_read(x = c(
      "12-07-2017, 10:35 PM - Johannes Gruber: Was it good?",
      "13-07-2017, 09:12 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # dd-MM-yyyy, HH:mm:ss
  expect_equal(
    rwa_read(x = c(
      "12-07-2017, 22:35:22 - Johannes Gruber: Was it good?",
      "13-07-2017, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # dd-MM-yyyy, HH:mm
  expect_equal(
    rwa_read(x = c(
      "12-07-2017, 22:35 - Johannes Gruber: Was it good?",
      "13-07-2017, 09:12 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # MM-dd-yyyy, hh:mm:ss a
  expect_equal(
    rwa_read(x = c(
      "07-12-2017, 10:35:22 PM - Johannes Gruber: Was it good?",
      "07-13-2017, 09:12:44 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # MM-dd-yyyy, hh:mm a
  expect_equal(
    rwa_read(x = c(
      "07-12-2017, 10:35 PM - Johannes Gruber: Was it good?",
      "07-13-2017, 09:12 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # MM-dd-yyyy, HH:mm:ss
  expect_equal(
    rwa_read(x = c(
      "07-12-2017, 22:35:22 - Johannes Gruber: Was it good?",
      "07-13-2017, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # MM-dd-yyyy, HH:mm
  expect_equal(
    rwa_read(x = c(
      "07-12-2017, 22:35 - Johannes Gruber: Was it good?",
      "07-13-2017, 09:12 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  ##### - + two-digit year
  # dd-MM-yy, hh:mm:ss a
  expect_equal(
    rwa_read(x = c(
      "12-07-17, 10:35:22 PM - Johannes Gruber: Was it good?",
      "13-07-17, 09:12:44 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # dd-MM-yy, hh:mm a
  expect_equal(
    rwa_read(x = c(
      "12-07-17, 10:35 PM - Johannes Gruber: Was it good?",
      "13-07-17, 09:12 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # dd-MM-yy, HH:mm:ss
  expect_equal(
    rwa_read(x = c(
      "12-07-17, 22:35:22 - Johannes Gruber: Was it good?",
      "13-07-17, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # dd-MM-yy, HH:mm
  expect_equal(
    rwa_read(x = c(
      "12-07-17, 22:35 - Johannes Gruber: Was it good?",
      "13-07-17, 09:12 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # MM-dd-yy, hh:mm:ss a
  expect_equal(
    rwa_read(x = c(
      "07-12-17, 10:35:22 PM - Johannes Gruber: Was it good?",
      "07-13-17, 09:12:44 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # MM-dd-yy, hh:mm a
  expect_equal(
    rwa_read(x = c(
      "07-12-17, 10:35 PM - Johannes Gruber: Was it good?",
      "07-13-17, 09:12 AM - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  # MM-dd-yy, HH:mm:ss
  expect_equal(
    rwa_read(x = c(
      "07-12-17, 22:35:22 - Johannes Gruber: Was it good?",
      "07-13-17, 09:12:44 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # MM-dd-yy, HH:mm
  expect_equal(
    rwa_read(x = c(
      "07-12-17, 22:35 - Johannes Gruber: Was it good?",
      "07-13-17, 09:12 - R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  ##### weirdo format
  expect_equal(
    rwa_read(x = c(
      "10:35 PM, 07 20 - Johannes Gruber: Was it good?",
      "9:12 AM, 07 20 - R: Yes, it was"
    ), tz = "GMT")$time,
    as.POSIXct(paste0(format(Sys.Date(), "%Y"),
                      c("-07-20 22:35:14 GMT", "-07-20 09:12:14 GMT")),
               tz = "GMT"),
    tolerance = 60
  )
  ##### special separators
  # []
  expect_equal(
    rwa_read(x = c(
      "[12.07.2017, 22:35:22] Johannes Gruber: Was it good?",
      "[13.07.2017, 09:12:44] R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # :
  expect_equal(
    rwa_read(x = c(
      "12.07.2017, 22:35:22: Johannes Gruber: Was it good?",
      "13.07.2017, 09:12:44: R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 5
  )
  # : + no-seconds
  expect_equal(
    rwa_read(x = c(
      "12.07.2017, 22:35: Johannes Gruber: Was it good?",
      "13.07.2017, 09:12: R: Yes, it was"
    ), tz = "GMT")$time,
    converted,
    tolerance = 60
  )
  ##### custom format and warning
  expect_equal(
    rwa_read(x = c(
      "07,12,2017, 22:35 - Johannes Gruber: Was it good?",
      "07,13,2017, 09:12 - R: Yes, it was"
    ), tz = "GMT", format = "MM,dd,yy, HH:mm")$time,
    converted,
    tolerance = 60
  )
  expect_warning(
    rwa_read(x = c(
      "20 09 17, 16:54 - Johannes Gruber: Was it good?",
      "20 09 17, 16:54 -  R: Yes, it was"
    )),
    "Time conversion did not work correctly. Provide a custom format or add an issue at www.github.com/JBGruber/rwhatsapp."
  )
})

test_that("See if author is converted correctly", {
  expect_equal(
    rwa_read(x = c(
      "12.07.17, 22:35:22: Johannes Gruber: Was it good?",
      "13.07.17, 09:12:44: R: Yes, it was"
    ))$author,
    structure(1:2, .Label = c("Johannes Gruber", "R"),
              class = "factor")
  )
  expect_equal(
    rwa_read(x = c(
      "12.07.17, 22:35:22 - Johannes Gruber: Was it good?",
      "13.07.17, 09:12:44 - R: Yes, it was"
    ))$author,
    structure(1:2, .Label = c("Johannes Gruber", "R"),
              class = "factor")
  )
  expect_equal(
    rwa_read(x = c(
      "12.07.17, 10:35 PM - Johannes Gruber: Was it good?",
      "13.07.17, 10:36 PM  - R: Yes, it was"
    ))$author,
    structure(1:2, .Label = c("Johannes Gruber", "R"),
              class = "factor")
  )
  expect_equal(
    rwa_read(x = c(
      "[20.09.17, 16:54:32] Johannes Gruber: Was it good?",
      "[20.09.17, 16:54:43] R: Yes, it was"
    ), verbose = TRUE)$author,
    structure(1:2, .Label = c("Johannes Gruber", "R"),
              class = "factor")
  )
})


test_that("reading from file", {
  expect_equal({
    out <- rwa_read(x = history,
                    tz = "GMT",
                    encoding = "UTF-8",
                    verbose = TRUE)
    # weird behaviour of tibble for comparison
    as.data.frame(out)[, 1:3]
  }, {
    out <- readRDS("../files/rwa_read.RDS")
    as.data.frame(out)[, 1:3]
  },
  tolerance = 60
  )
  expect_equal({
    as.data.frame(rwa_read(x = c(history, history),
                           tz = "GMT",
                           encoding = "UTF-8",
                           verbose = TRUE))[, 1:3]
  }, as.data.frame(rbind(readRDS("../files/rwa_read.RDS"),
                         readRDS("../files/rwa_read.RDS")))[, 1:3],
  tolerance = 60
  )
  expect_equal({
    out <- rwa_read(x = system.file("extdata", "sample.txt", package = "rwhatsapp"),
                    tz = "GMT",
                    encoding = "UTF-8",
                    verbose = TRUE)
    # weird behaviour of tibble for comparison
    as.data.frame(out)[, 1:3]
  }, {
    out <- readRDS("../files/rwa_read.RDS")
    as.data.frame(out)[, 1:3]
  }, tolerance = 60)
  expect_equal({
    dir <- paste0(tempdir(), "/test/")
    dir.create(dir)
    file.copy(system.file("extdata", "sample.txt", package = "rwhatsapp"),
              dir)
    utils::zip(paste0(dir, "test.zip"), paste0(dir, "sample.txt"), flags = "-jr9X")
    out <- rwa_read(x = paste0(dir, "test.zip"),
                    tz = "GMT",
                    encoding = "UTF-8",
                    verbose = TRUE)
    unlink(dir, recursive = TRUE)
    # weird behaviour of tibble for comparison
    as.data.frame(out)[, 1:3]
  }, {
    out <- readRDS("../files/rwa_read.RDS")
    as.data.frame(out)[, 1:3]
  }, tolerance = 60)
})

test_that("status", {
  expect_message(rwhatsapp:::status("test"), "test")
  expect_s3_class(rwhatsapp:::status("test"), "POSIXct")
  expect_message({
    start_time <- Sys.time()
    rwhatsapp:::status("test2")
  }, regexp = "	...test2 \\[\\d+.+")
})

test_that("warning", {
  expect_error(rwa_read(x = 1),
               "Provide either a path to one or multiple txt or zip files of a WhatsApp history or the history itself as character object.")
})
