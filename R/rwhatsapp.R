#' Read WhatsApp history into R
#'
#' The history can be obtained going to the menu in a chat on the WhatsApp app,
#' choosing "more", then "Export chat".
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
#' @param ... Further arguments passed to \link[stringi]{stri_read_lines}.
#'
#' @return a tibble
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
                     ...) {

  zps <- grep(".zip$", x, ignore.case = TRUE)
  temp <- NULL
  src <- NULL
  if (length(zps) > 0) {
    src <- x[zps]
    x[zps] <- vapply(x[zps], function(x) {
      content <- unzip(x, list = TRUE)
      content <- content[grepl(".txt$", content$Name, ignore.case = TRUE), ]
      temp <- paste0(tempdir(), "/whatsapp")
      unzip(x, files = content$Name, overwrite = TRUE, exdir	= temp)
      return(list.files(temp, pattern = content$Name, full.names = TRUE))
    }, FUN.VALUE = character(1))
  }
  if (verbose) {
    start_time <- Sys.time()
    cat("Reading chat history from ")
  }
  if (isTRUE(any(
    tryCatch(file.exists(x),
             error = function(e) {

             })
  ))) {
    if (length(x) == 1) {
      chat_raw <- stringi::stri_read_lines(x, ...)
      names(chat_raw) <- rep(x, length(chat_raw))
      if (verbose) cat("one log file...\n\t...one log file loaded [",
                       format(
                         (Sys.time() - start_time), digits = 2, nsmall = 2
                       ),
                       "]\n", sep = "")
    } else {
      chat_raw <- unlist(lapply(x, function(t) {
        cr <- stringi::stri_read_lines(t)#, ...)
        names(cr) <- rep(t, length(cr))
        return(cr)
      }))
      if (verbose) cat(length(x), " log files...\n\t...files loaded [",
                       format(
                         (Sys.time() - start_time), digits = 2, nsmall = 2
                       ),
                       "]\n", sep = "")
    }
  } else if (is.character(x) && length(x) > 1) {
    chat_raw <- x
    names(chat_raw) <- rep("text input", length(chat_raw))
    if (verbose) cat("character object...\n\t...object loaded [",
                     format(
                       (Sys.time() - start_time), digits = 2, nsmall = 2
                     ),
                     "]\n", sep = "")
  } else {
    stop("Provide either a path to one or multiple txt or zip files of a WhatsApp ",
         "history or the history itself as character object.")
  }
  if (length(zps) > 0) {
    names(chat_raw) <- stringi::stri_replace_last_fixed(names(chat_raw), x[zps], src)
    unlink(temp, recursive = TRUE)
  }
  chat_raw <- chat_raw[!chat_raw == ""]
  time <- stringi::stri_extract_first_regex(
    str = chat_raw,
    pattern = "^\\d+-\\d+-\\d+.*-|[^-]+ - "
  )
  if (sum(is.na(time)) > (length(time) / 2)) {
    time <- stringi::stri_extract_first_regex(str = chat_raw,
                                              pattern = "[^]]+] ")
  }
  if (sum(is.na(time)) == length(time)) {
    time <- stringi::stri_extract_first_regex(str = chat_raw,
                                              pattern = "^.*\\d+:\\d+")
  }
  for (l in which(is.na(time))) {
    chat_raw[l - 1] <- stringi::stri_paste(chat_raw[l - 1], chat_raw[l],
                                           sep = "\n")
  }

  chat_raw <- chat_raw[!is.na(time)]
  time <- time[!is.na(time)]
  if (verbose) cat("\t...timestamps extracted [",
                   format(
                     (Sys.time() - start_time),
                     digits = 2, nsmall = 2
                   ),
                   "]\n", sep = "")

  source <- names(chat_raw)
  chat_raw <- stringi::stri_replace_first_fixed(str = chat_raw,
                                                pattern = time,
                                                replacement = "")

  time <- stringi::stri_replace_all_regex(
    str = time,
    pattern = c("\\[", "\\]", "-$", "- $"),
    replacement = c("", "", "", ""),
    vectorize_all = FALSE
  )

  if (is.null(format)) {
    formats <- c(
      "dd.MM.yyyy, hh:mm:ss a",
      "dd.MM.yyyy, hh:mm a",
      "dd.MM.yyyy, HH:mm:ss",
      "dd.MM.yyyy, HH:mm",
      "MM.dd.yyyy, hh:mm:ss a",
      "MM.dd.yyyy, hh:mm a",
      "MM.dd.yyyy, HH:mm:ss",
      "MM.dd.yyyy, HH:mm"
    )
    if (any(stringi::stri_detect_fixed(time, "."))) {
      if (sum(stringi::stri_detect_regex(time, "\\d+.\\d+.\\d{2}")) >
          (length(time) * 0.9)) {
        formats <- stringi::stri_replace_all_fixed(
          formats,
          "yyyy",
          "yy"
        )
      }
    } else if (any(stringi::stri_detect_fixed(time, "/"))) {
      formats <- stringi::stri_replace_all_fixed(
        formats,
        ".",
        "/"
      )
      if (sum(stringi::stri_detect_regex(time, "\\d+/\\d+/\\d{2}")) >
          (length(time) * 0.9)) {
        formats <- stringi::stri_replace_all_fixed(
          formats,
          "yyyy",
          "yy"
        )
      }
    } else if (any(stringi::stri_detect_fixed(time, "-"))) {
      formats <- stringi::stri_replace_all_fixed(
        formats,
        ".",
        "-"
      )
      if (sum(stringi::stri_detect_regex(time, "\\d+-\\d+-\\d{2}")) >
          (length(time) * 0.9)) {
        formats <- stringi::stri_replace_all_fixed(
          formats,
          "yyyy",
          "yy"
        )
      }
    } else {
      formats <- c(
        "hh:mma, MM dd"
      )
    }
    test <- sapply(formats, function(f) {
      test <- stringi::stri_datetime_parse(str = head(time, n = 1000),
                                           format = f,
                                           lenient = FALSE,
                                           tz = tz)
      sum(is.na(test))
    })
    format <- names(which.min(test))
  }

  time <- stringi::stri_datetime_parse(str = time,
                                       format = format,
                                       tz = tz)

  if (verbose) cat("\t...timestamps converted [",
                   format(
                     (Sys.time() - start_time),
                     digits = 2, nsmall = 2
                   ),
                   "]\n", sep = "")

  if (sum(is.na(time)) > (length(time) / 10)) {
    warning("Time conversion did not work correctly. Provide a custom format",
            " or add an issue at www.github.com/JBGruber/rwhatsapp.")
  }

  author <- stringi::stri_extract_first_regex(str = chat_raw,
                                              pattern = "[^:]+: ")
  chat_raw[!is.na(author)] <- stringi::stri_replace_first_fixed(
    str = chat_raw[!is.na(author)],
    pattern = author[!is.na(author)],
    replacement = ""
  )
  author <- stringi::stri_replace_last_fixed(str = author,
                                             pattern = ": ",
                                             replacement = "")
  if (verbose) cat("\t...author extracted [",
                   format(
                     (Sys.time() - start_time),
                     digits = 2, nsmall = 2
                   ),
                   "]\n", sep = "")
  tbl <- tibble::tibble(
    time = time,
    author = as.factor(stringi::stri_trim_both(author)),
    text = chat_raw,
    source = source
  )

  tbl <- dplyr::bind_cols(tbl, rwa_add_emoji(tbl))
  if (verbose) cat("\t...emoji extracted [",
                   format(
                     (Sys.time() - start_time),
                     digits = 2, nsmall = 2
                   ),
                   "]\n", sep = "")

  if (verbose) cat(
    nrow(tbl),
    " messages from ",
    length(unique(tbl$author)),
    " authors extracted. ",
    "Elapsed time: ", format(
      (Sys.time() - start_time), digits = 2, nsmall = 2
    ), "\n", sep = ""
  )

  return(
    tbl
  )
}


#' @noRd
#' @importFrom tidytext unnest_tokens
#' @importFrom stringi stri_replace_all_regex
#' @importFrom dplyr left_join group_by summarise select ungroup
#' @importFrom rlang .data
rwa_add_emoji <- function(x) {
  x$id <- seq_along(x$text)
  x$text <- stringi::stri_replace_all_regex(
    x$text,
    "[[:alnum:]]",
    "x"
  )
  out <- tidytext::unnest_tokens_(
    x,
    output = "emoji",
    input = "text",
    token = "characters",
    format = "text",
    to_lower = FALSE,
    drop = FALSE,
    collapse = FALSE,
    strip_non_alphanum = FALSE
  )
  out <- dplyr::left_join(out, rwhatsapp::emojis, by = "emoji")
  out$emoji[is.na(out$name)] <- NA
  out <- dplyr::group_by(out, .data$id)
  out <- dplyr::summarise(
    out,
    emoji = list(.data$emoji[!is.na(.data$emoji)]),
    emoji_name = list(.data$name[!is.na(.data$name)])
  )
  out <- dplyr::ungroup(out)
  out$emoji_count <- sapply(out$emoji, length)
  return(dplyr::select(out, .data$emoji, .data$emoji_name))
}


#' List of emojis and corresponding descriptions.
#'
#' A dataset containing emojis and corresponding descriptions. This is a subset
#' of the emojis provided by the emo package.
#'
#' @format A tibble with 3570 rows and 2 columns
#' - emoji: character representation of the emoji
#' - name: name of the emoji
#' @source \url{https://github.com/hadley/emo/}
"emojis"
