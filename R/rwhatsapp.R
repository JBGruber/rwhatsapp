#' Read whatsapp history into R
#'
#' The history can be obtained going to the menu in a chat on the whatsapp app,
#' choosing "more", then "Export chat".
#'
#' @param txt Path to a txt file of a whatsapp history or the history itself as
#'   character object.
#' @param tz A time zone for date conversion. Set NULL or "" for the default
#'   time zone or a single string with a timezone identifier, see
#'   \link[stringi]{stri_timezone_list}.
#' @param ... Further arguments passed to \link[stringi]{stri_read_lines}.
#'
#' @return a tibble
#' @export
#' @importFrom stringi stri_read_lines stri_extract_first_regex stri_paste
#'   stri_datetime_parse stri_replace_first_fixed stri_replace_last_fixed
#'   stri_trim_both
#' @importFrom tibble data_frame
#' @importFrom utils head
#'
#' @examples
#' history <- system.file("extdata", "sample.txt", package = "rwhatsapp")
#' df <- rwa_read(history)
rwa_read <- function(txt, tz = NULL, ...) {
  if (isTRUE(any(
    tryCatch(file.exists(txt),
             error = function(e) {})
  ))) {
    if (length(txt) == 1) {
      chat_raw <- stringi::stri_read_lines(txt, ...)
    } else {
      chat_raw <- unlist(lapply(txt, function(t) {
        stringi::stri_read_lines(t)
      }))
    }
  } else if (is.character(txt)) {
    chat_raw <- txt
  } else {
    stop("Provide either a path to one or multiple txt files of a whatsapp ",
         "history or the history itself as character object.")
  }
  chat_raw <- chat_raw[!chat_raw == ""]
  time <- stringi::stri_extract_first_regex(str = chat_raw,
                                            pattern = "[^-]+ - ")
  if (sum(is.na(time)) == length(time)) {
    time <- stringi::stri_extract_first_regex(str = chat_raw,
                                              pattern = "[^]]+] ")
  }
  if (sum(is.na(time)) == length(time)) {
    time <- stringi::stri_extract_first_regex(str = chat_raw,
                                              pattern = "^.*\\d+:\\d+:")
  }
  for (l in which(is.na(time))) {
    chat_raw[l - 1] <- stringi::stri_paste(chat_raw[l - 1], chat_raw[l],
                                           sep = "\n")
  }
  chat_raw <- chat_raw[!is.na(time)]
  time <- time[!is.na(time)]
  chat_raw <- stringi::stri_replace_first_fixed(str = chat_raw,
                                                pattern = time,
                                                replacement = "")

  time <- stringi::stri_replace_all_fixed(str = time,
                                          pattern = c("[", "]"),
                                          replacement = c("", ""),
                                          vectorize_all = FALSE)
  formats <- c(
    "dd.MM.yy, HH:mm:ss",
    "dd.MM.yy, HH:mm",
    "dd/MM/yyyy, HH:mm:ss",
    "dd/MM/yyyy, HH:mm",
    "dd-MM-yyyy, HH:mm:ss",
    "dd-MM-yyyy, HH:mm",
    "HH:mm:ssa, MM dd",
    "HH:mma, MM dd"
  )
  test <- sapply(formats, function(f) {
    test <- stringi::stri_datetime_parse(str = head(time, n = 1000),
                                         format = f,
                                         lenient = TRUE,
                                         tz = tz)
    sum(is.na(test))
  })
  format <- names(which.min(test))
  time <- stringi::stri_datetime_parse(str = time,
                                       format = format,
                                       tz = tz)
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
  return(
    tibble::data_frame(
      time = time,
      author = as.factor(stringi::stri_trim_both(author)),
      text = chat_raw
    )
  )
}
