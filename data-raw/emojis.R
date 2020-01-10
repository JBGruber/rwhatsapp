em <- "http://unicode.org/Public/emoji/latest/emoji-data.txt"

library(data.table)

emo <- fread(em)

library(rvest)
library(dplyr)
emoji_list <- "https://unicode.org/emoji/charts-13.0/full-emoji-list.html"
emoji_modifiers <- "https://unicode.org/emoji/charts-13.0/full-emoji-modifiers.html"

html_table2 <- function(x, header) {
  tables <- xml2::xml_find_all(x, ".//table")[[1]]
  rows <- html_nodes(tables, "tr")
  n <- length(rows)
  cells <- lapply(rows, "html_nodes", xpath = ".//td|.//th")

  ncols <- lapply(cells, html_attr, "colspan", default = "1")
  ncols <- lapply(ncols, as.integer)
  nrows <- lapply(cells, html_attr, "rowspan", default = "1")
  nrows <- lapply(nrows, as.integer)

  p <- unique(vapply(ncols, sum, integer(1)))
  maxp <- max(p)

  values1 <- lapply(cells, html_text, trim = TRUE)
  values2 <- lapply(lapply(cells, html_node, "img"), html_attr, name = "src")
  values <- mapply(function(x, y){
    ifelse(x == "", y, x)
  }, x = values1, y = values2)

  out <- matrix(NA_character_, nrow = n, ncol = maxp)

  # fill colspans right with repetition
  for (i in seq_len(n)) {
    row <- values[[i]]
    ncol <- ncols[[i]]
    col <- 1
    for (j in seq_len(length(ncol))) {
      out[i, col:(col+ncol[j]-1)] <- row[[j]]
      col <- col + ncol[j]
    }
  }

  # fill rowspans down with repetition
  for (i in seq_len(maxp)) {
    for (j in seq_len(n)) {
      rowspan <- nrows[[j]][i]; colspan <- ncols[[j]][i]
      if (!is.na(rowspan) & (rowspan > 1)) {
        if (!is.na(colspan) & (colspan > 1)) {
          # special case of colspan and rowspan in same cell
          nrows[[j]] <- c(utils::head(nrows[[j]], i),
                          rep(rowspan, colspan-1),
                          utils::tail(nrows[[j]], length(rowspan)-(i+1)))
          rowspan <- nrows[[j]][i]
        }
        for (k in seq_len(rowspan - 1)) {
          l <- utils::head(out[j+k, ], i-1)
          r <- utils::tail(out[j+k, ], maxp-i+1)
          out[j + k, ] <- utils::head(c(l, out[j, i], r), maxp)
        }
      }
    }
  }
  # Convert matrix to list to data frame
  df <- lapply(seq_len(maxp), function(i) {
    utils::type.convert(out[, i], as.is = TRUE)
  })
  names(df) <- out[header, ]
  class(df) <- "data.frame"
  attr(df, "row.names") <- .set_row_names(length(df[[1]]))

  df
}


emoji_df <- read_html(emoji_list) %>%
  html_table2(header = 3) %>%
  filter(grepl("\\d", `№`)) %>%
  select(emoji = Browser, name = `CLDR Short Name`, hex_runes = Code, img = Appl) %>%
  mutate(hex_runes = gsub("U+", "", hex_runes, fixed = TRUE))

# In case pictures should be stored
# lapply(seq_along(emoji_df$img), function(i) {
#   magick::image_write(image = magick::image_read(emoji_df$img[i]),
#                       path = paste0("./data/img/", emoji_df$hex_runes[i], ".png"))
# })

emoji_modifiers_df <- read_html(emoji_modifiers) %>%
  html_table2(header = 3) %>%
  filter(grepl("\\d", `№`)) %>%
  select(emoji = Browser, name = `CLDR Short Name`, hex_runes = Code, img = Appl) %>%
  mutate(hex_runes = gsub("U+", "", hex_runes, fixed = TRUE))

emojis <- bind_rows(emoji_df, emoji_modifiers_df) %>%
  select(-img)

# save data
usethis::use_data(emojis, overwrite = TRUE)

cmp <- emojis[!emojis$emoji %in% rwhatsapp::emojis$emoji, ]


emoji_df %>%
  head(500) %>%
  pull(emoji) %>%
  paste(collapse = " ") %>%
  clipr::write_clip()

