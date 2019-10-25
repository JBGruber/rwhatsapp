# install newest version of emo to get data
devtools::install_github("hadley/emo")

# extract emoji data
library(dplyr)
emojis <- emo::jis %>%
  select(emoji, name, hex_runes = runes)

# save data
usethis::use_data(emojis, overwrite = TRUE)
