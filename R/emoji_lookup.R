#' Lookup emojis from text
#'
#' @description Takes a character string or data.frame with text, looks up all
#'   emoji characters in it and also returns their description. Supports the
#'   full unicode Emoji List v13.0 (see \link{emojis}).
#'
#' @param x A character vector or data.frame.
#' @param text_field the character name or numeric index of the source
#'   `data.frame` indicating the variable to be read in as text.
#' @param ... not currently used.
#'
#' @importFrom tibble tibble add_column
#' @importFrom stringi stri_replace_all_regex stri_replace_all_charclass
#'   stri_split_boundaries
#' @export
lookup_emoji <- function(x, ...) {
  UseMethod("lookup_emoji")
}

#' @rdname lookup_emoji
#' @export
lookup_emoji.data.frame <- function(x, text_field = "text", ...) {

  id <- seq_along(x[[text_field]])
  text <- x[[text_field]]

  emojis <- lookup_emoji(text)
  emojis$id <- NULL
  emojis$text <- NULL

  return(tibble::add_column(x, emojis))
}


#' @rdname lookup_emoji
#' @export
lookup_emoji.character <- function(x, ...) {

  id <- seq_along(x)
  text <- stri_replace_all_charclass(x, "[[:punct:][:whitespace:]]", "")
  l <- stri_split_boundaries(text, type = "character")

  lookup <- tibble(id = rep(id, sapply(l, length)), emoji = unlist(l))

  lookup <- add_column(lookup,
                       emoji_name = rwhatsapp::emojis$name[
                         match(lookup$emoji,
                               rwhatsapp::emojis$emoji)
                       ])

  lookup <- lookup[!is.na(lookup$emoji_name), ]

  lookup <- tibble(id = unique(lookup$id),
                   emoji = unname(split(lookup$emoji, lookup$id)),
                   emoji_name = unname(split(lookup$emoji_name, lookup$id)))

  out <- tibble(
    id = id,
    text = x,
    emoji = lookup$emoji[match(id, lookup$id)],
    emoji_name = lookup$emoji_name[match(id, lookup$id)]
  )

  return(out)
}
