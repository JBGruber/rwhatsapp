# install newest version of emo to get data
devtools::install_github("hadley/emo")

# extract emoji data
emojis <- emo::jis %>%
  select(emoji, name)

# test data
library(rwhatsapp)
library(tidytext)
library(dplyr)
history <- system.file("extdata", "sample.txt", package = "rwhatsapp")
chat <- rwa_read(txt = history)

chat_characters <- chat %>%
  unnest_tokens(output = "emoji",
                input = "text",
                token = "characters")

chat_emojis <- chat_characters %>%
  left_join(y, by = "emoji") %>%
  filter(!duplicated(emoji)) %>%
  filter(!grepl("[A-z]|<|>", emoji))

table(is.na(chat_emojis$name))

# save data
devtools::use_data(emojis, overwrite = TRUE)
