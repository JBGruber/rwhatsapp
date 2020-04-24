# install newest version of emo to get data
devtools::install_github("hadley/emo")

# extract emoji data
library(dplyr)
emojis_emo <- emo::jis %>%
  select(emoji, name, hex_runes = runes)

# save data
usethis::use_data(emojis, overwrite = TRUE)

# scrape new data from unicode website
library(rvest)
library(stringi)

emo_url <- "http://unicode.org/emoji/charts/emoji-list.html"
emo_full_url <- "https://unicode.org/emoji/charts/full-emoji-list.html"
modifier_url <- "https://unicode.org/emoji/charts/full-emoji-modifiers.html"

# emo
lines <- readLines(emo_url)
entries <- which(stri_detect_regex(
  lines,
  pattern = "<tr><td class='rchars'>\\d+</td>"
))
entries <- tibble(entry_start = entries,
                  entry_stop = lead(entry_start) - 1) %>%
  mutate(entry_stop = ifelse(is.na(entry_stop), length(lines), entry_stop)) %>%
  mutate(html = map2(entry_start, entry_stop, function(x, y) {
    lines[x:y]
  }))


extract_table <- function(x) {
  emoji <- x %>%
    stri_subset_fixed("class='andr'")
  if (length(emoji) == 0) {
    emoji <- x %>%
      stri_subset_fixed("class='andr alt'") %>%
      .[1]
  }
  emoji <- emoji %>%
    read_html() %>%
    html_nodes("img") %>%
    html_attr("alt")

  name <- x %>%
    stri_subset_fixed("class='name'") %>%
    .[1] %>%
    read_html() %>%
    html_text()

  hex_runes <- x %>%
    stri_subset_fixed("class='code'") %>%
    read_html() %>%
    html_text()

  tibble::tibble(
    emoji = emoji,
    name = name,
    hex_runes = hex_runes
  )
}

emo_df <- map_df(entries$html, extract_table)

# emo_full
lines <- readLines(emo_full_url)
entries <- which(stri_detect_regex(
  lines,
  pattern = "<tr><td class='rchars'>\\d+</td>"
))
entries <- tibble(entry_start = entries,
                  entry_stop = lead(entry_start) - 1) %>%
  mutate(entry_stop = ifelse(is.na(entry_stop), length(lines), entry_stop)) %>%
  mutate(html = map2(entry_start, entry_stop, function(x, y) {
    lines[x:y]
  }))

emo_full_df <- map_df(entries$html, extract_table)

# modifier
lines <- readLines(modifier_url)
entries <- which(stri_detect_regex(
  lines,
  pattern = "<tr><td class='rchars'>\\d+</td>"
))
entries <- tibble(entry_start = entries,
                  entry_stop = lead(entry_start) - 1) %>%
  mutate(entry_stop = ifelse(is.na(entry_stop), length(lines), entry_stop)) %>%
  mutate(html = map2(entry_start, entry_stop, function(x, y) {
    lines[x:y]
  }))

modifier_df <- map_df(entries$html, extract_table)

emojis <- bind_rows(emo_df, emo_full_df, modifier_df, emojis_emo) %>%
  mutate(hex_runes = str_remove(hex_runes, "^U\\+")) %>%
  distinct(emoji)

# save data
usethis::use_data(emojis, overwrite = TRUE)
