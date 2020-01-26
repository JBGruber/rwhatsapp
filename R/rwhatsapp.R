#' Read WhatsApp history into R
#'
#' Takes a history file from the ``WhatsApp'' messenger app (txt or zip) and
#' returns a formatted data.frame with descriptions of the used emojis.
#'
#' @details The history can be obtained going to the menu in a chat on the
#'   ``WhatsApp'' app, choosing "more", then "Export chat".
#'
#' @param x Path to a txt or zip file of a WhatsApp history or the history
#'   itself as character object.
#' @param tz A time zone for date conversion. Set NULL or "" for the default
#'   time zone or a single string with a timezone identifier, see
#'   \link[stringi]{stri_timezone_list}.
#' @param format Most formats are automatically detected. If you encounter
#'   problems you can provide a custom format here. Refer to
#'   \link[stringi]{stri_datetime_parse} for guidance.
#' @param verbose A logical flag indicating whether information should be
#'   printed to the screen.
#' @param encoding Input encoding. Should usually be "UTF-8" if files haven't
#'   changed since export from WhatsApp.
#' @param ... Further arguments passed to \link[base]{readLines}.
#'
#' @return A tibble with the information parsed from the history file.
#' @export
#' @import stringi
#' @importFrom tibble tibble
#' @importFrom utils head unzip
#'
#' @examples
#' history <- system.file("extdata", "sample.txt", package = "rwhatsapp")
#' df <- rwa_read(history)
rwa_read <- function(x,
                     tz = NULL,
                     format = NULL,
                     verbose = FALSE,
                     encoding = "UTF-8",
                     ...) {

  if (verbose) {
    start_time <- status("Reading chat history from",
                         appendLF = FALSE, ppfix = "")
  } else {
    start_time <- NULL
  }

  chat_raw <- rwa_read_lines(x, verbose, start_time, encoding, ...)

  chat_raw <- chat_raw[!chat_raw == ""]
  time <- stri_extract_first_regex(
    str = chat_raw,
    pattern = "^\\d+-\\d+-\\d+.*-|[^-]+ - "
  )
  if (sum(is.na(time)) > (length(time) / 2)) {
    time <- stri_extract_first_regex(str = chat_raw,
                                     pattern = "[^]]+] ")
  }
  if (sum(is.na(time)) == length(time)) {
    time <- stri_extract_first_regex(str = chat_raw,
                                     pattern = "^.*\\d+:\\d+")
  }
  for (l in rev(which(is.na(time)))) {
    chat_raw[l - 1] <- stri_paste(chat_raw[l - 1], chat_raw[l],
                                  sep = "\n")
  }

  chat_raw <- chat_raw[!is.na(time)]
  time <- time[!is.na(time)]
  if (verbose) status("timestamps extracted")

  source <- names(chat_raw)
  chat_raw <- stri_replace_first_fixed(str = chat_raw,
                                       pattern = time,
                                       replacement = "")

  time <- stri_replace_all_regex(
    str = time,
    pattern = c("\\[", "\\]", "-$", "- $"),
    replacement = c("", "", "", ""),
    vectorize_all = FALSE
  )

  time <- rwa_parse_time(time, format, tz)

  if (verbose) status("timestamps converted")

  if (sum(is.na(time)) > (length(time) / 10)) {
    warning("Time conversion did not work correctly. Provide a custom format",
            " or add an issue at www.github.com/JBGruber/rwhatsapp.")
  }

  author <- stri_extract_first_regex(str = chat_raw,
                                     pattern = "[^:]+: ")
  chat_raw[!is.na(author)] <- stri_replace_first_fixed(
    str = chat_raw[!is.na(author)],
    pattern = author[!is.na(author)],
    replacement = ""
  )
  author <- stri_replace_last_fixed(str = author,
                                    pattern = ": ",
                                    replacement = "")

  if (verbose) status("author extracted")

  tbl <- tibble::tibble(
    time = time,
    author = as.factor(stri_trim_both(author)),
    text = chat_raw,
    source = source
  )

  tbl <- rwa_add_emoji(tbl)

  if (verbose) {
    status("emoji extracted")
    status(nrow(tbl),
           " messages from ",
           length(unique(tbl$author)),
           " authors extracted. ",
           "Elapsed time:",
           ppfix = "", indent = "")
  }

  return(tbl)
}

#' Read in files from supported formats
#'
#' @param start_time For verbose messages.
#' @inherit rwa_read
#' @import stringi
#' @noRd
rwa_read_lines <- function(x, verbose, start_time = NULL, encoding, ...) {
  # get files
  zps <- grep(".zip$", x, ignore.case = TRUE)
  temp <- NULL
  src <- NULL
  if (length(zps) > 0) {
    src <- x[zps]
    x[zps] <- vapply(x[zps], FUN.VALUE = character(1), FUN = function(x) {
      content <- unzip(x, list = TRUE)
      content <- content[grepl(".txt$", content$Name, ignore.case = TRUE), ]
      temp <- paste0(tempdir(), "/whatsapp")
      unzip(x, files = content$Name, overwrite = TRUE, exdir = temp)
      return(list.files(temp, pattern = content$Name, full.names = TRUE))
    })
  }

  if (f_exist_s(x)) {
    if (length(x) == 1) {
      chat_raw <- readLines(x, encoding = encoding, ...)
      names(chat_raw) <- rep(x, length(chat_raw))
      if (verbose) {
        message(" one log file...")
        status("one log file loaded")
      }
    } else {
      chat_raw <- unlist(lapply(x, function(t) {
        cr <- readLines(t, encoding = encoding, ...)
        names(cr) <- rep(t, length(cr))
        return(cr)
      }))
      if (verbose) {
        message(" ", length(x), " log files...")
        status("files loaded ")
      }
    }
  } else if (is.character(x) && length(x) > 1) {
    chat_raw <- x
    names(chat_raw) <- rep("text input", length(chat_raw))
    if (verbose) {
      message(" character object...")
      status("object loaded ")
    }
  } else {
    stop("Provide either a path to one or multiple txt or zip files of a ",
         "WhatsApp history or the history itself as character object.")
  }
  if (length(zps) > 0) {
    names(chat_raw) <- stri_replace_last_fixed(names(chat_raw), x[zps], src)
    unlink(temp, recursive = TRUE)
  }
  return(chat_raw)
}


#' Parse time
#'
#' @param time A character object with times to parse.
#' @inherit rwa_read
#' @import stringi
#' @noRd
rwa_parse_time <- function(time, format, tz) {
  if (is.null(format)) {
    formats <- c(
      "dd.MM.yyyy hh:mm:ss a",
      "dd.MM.yyyy hh:mm a",
      "dd.MM.yyyy HH:mm:ss",
      "dd.MM.yyyy HH:mm",
      "MM.dd.yyyy hh:mm:ss a",
      "MM.dd.yyyy hh:mm a",
      "MM.dd.yyyy HH:mm:ss",
      "MM.dd.yyyy HH:mm"
    )

    time <- stri_replace_all_regex(
      time,
      c("[^[0-9.:/\\-APM]]", "\\s+"),
      c(" ", " "),
      vectorize_all = FALSE
    )

    if (any(stri_detect_fixed(time, "."))) {
      if (sum(stri_detect_regex(time, "\\d+.\\d+.\\d{1,2}")) >
          (length(time) * 0.9)) {
        formats <- stri_replace_all_fixed(
          formats,
          "yyyy",
          "yy"
        )
      }
    } else if (any(stri_detect_fixed(time, "/"))) {
      formats <- stri_replace_all_fixed(
        formats,
        ".",
        "/"
      )
      if (sum(stri_detect_regex(time, "\\b\\d{1,2}/\\d{1,2}/\\d{2}")) >
          (length(time) * 0.9)) {
        formats <- stri_replace_all_fixed(
          formats,
          "yyyy",
          "yy"
        )
      } else if (sum(stri_detect_regex(time, "\\b\\d{4}/\\d+/\\d{1,2}")) >
                 (length(time) * 0.9)) {
        formats <- stri_replace_all_fixed(
          formats,
          "dd/MM/yyyy",
          "yyyy/MM/dd",
          vectorize_all = FALSE
        )
      }
    } else if (any(stri_detect_fixed(time, "-"))) {
      formats <- stri_replace_all_fixed(
        formats,
        ".",
        "-"
      )
      if (sum(stri_detect_regex(time, "\\b\\d{1,2}-\\d{1,2}-\\d+")) >
          (length(time) * 0.9)) {
        formats <- stri_replace_all_fixed(
          formats,
          "yyyy",
          "yy"
        )
      } else if (sum(stri_detect_regex(time, "\\b\\d{4}-\\d+-\\d{1,2}")) >
                 (length(time) * 0.9)) {
        formats <- stri_replace_all_fixed(
          formats,
          "dd-MM-yyyy",
          "yyyy-MM-dd",
          vectorize_all = FALSE
        )
      }
    } else {
      formats <- c(
        "hh:mma, MM dd"
      )
    }
    test <- sapply(formats, function(f) {
      test <- stri_datetime_parse(str = head(time, n = 1000),
                                  format = f,
                                  lenient = FALSE,
                                  tz = tz)
      sum(is.na(test))
    })
    format <- names(which.min(test))
  }

  time <- stri_datetime_parse(str = time,
                              format = format,
                              tz = tz)

  return(time)
}


#' @noRd
#' @importFrom tibble tibble add_column
#' @importFrom stringi stri_replace_all_regex stri_replace_all_charclass
#'   stri_split_boundaries
rwa_add_emoji <- function(x) {

  id <- seq_along(x[["text"]])
  x <- add_column(x, id = id)
  text <- x[["text"]]

  text <- stri_replace_all_charclass(text, "[[:punct:][:whitespace:]]", "")
  l <- stri_split_boundaries(text, type = "character")

  out <- tibble(id = rep(id, sapply(l, length)), emoji = unlist(l))

  out <- add_column(out,
                    emoji_name = rwhatsapp::emojis$name[
                      match(out$emoji,
                            rwhatsapp::emojis$emoji)
                      ])

  out <- out[!is.na(out$emoji_name), ]

  out <- tibble(id = unique(out$id),
                emoji = unname(split(out$emoji, out$id)),
                emoji_name = unname(split(out$emoji_name, out$id)))

  x <- add_column(
    x,
    emoji = out$emoji[match(x$id, out$id)],
    emoji_name = out$emoji_name[match(x$id, out$id)]
  )

  x$id <- NULL

  return(x)
}


# creates status message and exports start_time if not in parent environment yet
status <- function(...,
                   sep = "",
                   appendLF = TRUE,
                   ppfix = "...",
                   indent = "\t") {

  if (exists("start_time", envir = parent.frame())) {
    start_time <- mget("start_time", envir = parent.frame())[[1]]
    diff <- format((Sys.time() - start_time), digits = 2, nsmall = 2)
    message(paste(indent, ppfix, ..., " [", diff, "]", sep = sep),
            appendLF = appendLF)
  } else {
    export <- Sys.time()
    start_time <- export
    message(paste(..., ppfix, sep = sep), appendLF = appendLF)
  }

  if (exists("export")) {
    return(export)
  }
}


# safely test if files exist
f_exist_s <- function(x) {
  isTRUE(any(
    tryCatch(file.exists(x),
             error = function(e) {

             })
  ))
}


#' List of emojis and corresponding descriptions.
#'
#' A dataset containing emojis and corresponding descriptions. This is a subset
#' of the emojis provided by the emo package.
#'
#' @format A tibble with 3570 rows and 3 columns: \itemize{
#'   \item emoji character representation of the emoji
#'   \item name of the emoji
#'   \item hex_runes hexadecimal representations of emoji
#' }
#' @details \code{hex_runes} can be used to easily look up image files of
#'   emojis.
#' @source \url{https://github.com/hadley/emo/}
"emojis"
