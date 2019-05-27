
# rwhatsapp

[![Travis-CI Build
Status](https://travis-ci.org/JBGruber/rwhatsapp.svg?branch=master)](https://travis-ci.org/JBGruber/rwhatsapp)
[![Coverage
Status](https://codecov.io/gh/JBGruber/rwhatsapp/branch/master/graph/badge.svg)](https://codecov.io/github/JBGruber/rwhatsapp?branch=master)

## Motivation

WhatsApp seems to become increasingly important not just as a messaging
service but also as a social network—thanks to its group chat
capabilities. Furthermore, retrieving chat logs from the Android or iOS
app is very straightforward: Simply choose `More` in the menu of a chat,
then `Export chat` and export the history to a txt file.

![alt-text-1](https://i.stack.imgur.com/KBZvOb.png "menu")
![alt-text-2](https://i.stack.imgur.com/J1WoIb.png "more")
![alt-text-2](https://i.stack.imgur.com/c8uqJb.png "export")

This package is intended to provide some infrastructure to work with
this text data in `R`. So far it reads txt files into R and separates
username, time and the messages into columns of a `data.frame` and
extracts emojis. **Please create an issue if you have ideas for what can
be useful functions or if you have problems with the existing
function:** [**issue
report**](https://github.com/JBGruber/rwhatsapp/issues).

## Installation

``` r
devtools::install_github("JBGruber/rwhatsapp")
```

## Demo

The package comes with a small sample that you can use to get going.

``` r
history <- system.file("extdata", "sample.txt", package = "rwhatsapp")
history
```

    ## [1] "/home/johannes/R/x86_64-pc-linux-gnu-library/3.6/rwhatsapp/extdata/sample.txt"

The main function of the package, `rwa_read()` can handle txt files
directly, which means that you can simply provide the path to a file to
get started:

``` r
library("rwhatsapp")
chat <- rwa_read(history)
chat
```

    ## # A tibble: 9 x 5
    ##   time                author    text                      emoji  emoji_name
    ##   <dttm>              <fct>     <chr>                     <list> <list>    
    ## 1 2017-07-12 22:35:54 <NA>      Messages to this group a… <chr … <chr [0]> 
    ## 2 2017-07-12 22:35:54 <NA>      "You created group \"Tes… <chr … <chr [0]> 
    ## 3 2017-07-12 22:35:54 Johannes… <Media omitted>           <chr … <chr [0]> 
    ## 4 2017-07-12 22:35:54 Johannes… Fruit bread with cheddar… <chr … <chr [2]> 
    ## 5 2017-07-13 09:12:54 Test      "It's fun doing text ana… <chr … <chr [0]> 
    ## 6 2017-07-13 09:16:54 Johannes… Haha it sure is 😅        <chr … <chr [1]> 
    ## 7 2018-09-28 13:27:54 Johannes… Did you know there is an… <chr … <chr [0]> 
    ## 8 2018-09-28 13:28:54 Johannes… 😀😃😄😁😆😅😂🤣☺😊😇🙂🙃😉😌😍😘😗😙😚😋😛😝😜… <chr … <chr [242…
    ## 9 2018-09-28 13:30:54 Johannes… 🤷‍♀🤷🏻‍♂🙎‍♀🙎‍♂🙍‍♀🙍‍♂💇‍♀💇‍…         <chr … <chr [87]>

Now, this isn’t very interesting so you will probably want to use your
own data. For this demonstration, I use one of my own chat logs from a
conversation with friends:

``` r
chat <- rwa_read("/home/johannes/WhatsApp Chat.txt")
chat
```

    ## # A tibble: 16,816 x 5
    ##    time                author    text                     emoji  emoji_name
    ##    <dttm>              <fct>     <chr>                    <list> <list>    
    ##  1 2015-12-10 19:57:54 Artur Ku… <Media omitted>          <chr … <chr [0]> 
    ##  2 2015-12-10 22:31:54 Erika Ils 😂😂😂😂😂😂             <chr … <chr [6]> 
    ##  3 2015-12-11 02:13:54 Alexandr… 🙈                       <chr … <chr [1]> 
    ##  4 2015-12-11 02:23:54 Johannes… 😂                       <chr … <chr [1]> 
    ##  5 2015-12-11 02:24:54 Johannes… Die Petitionen Trump di… <chr … <chr [1]> 
    ##  6 2015-12-11 03:51:54 Erika Ils Läääuft                  <chr … <chr [0]> 
    ##  7 2015-12-12 07:49:54 Johannes… <Media omitted>          <chr … <chr [0]> 
    ##  8 2015-12-12 07:53:54 Erika Ils was macht ihr huete?     <chr … <chr [0]> 
    ##  9 2015-12-12 07:55:54 Johannes… Alex arbeitet weil sie … <chr … <chr [0]> 
    ## 10 2015-12-12 07:55:54 Johannes… und ich spiele auf mein… <chr … <chr [0]> 
    ## # … with 16,806 more rows

We write a lot in this group\! Let’s see over how much time we managed
to accumulate 16,816 messages.

``` r
library("dplyr")
library("ggplot2")
library("lubridate")
chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Messages per day") +
  theme_bw()
```

![](https://i.imgur.com/ONW927b.png)<!-- -->

The chat has been going on for a while and on some days there were more
than a hundred messages. Who’s responsible for all of this?

``` r
chat %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages") +
  theme_bw()
```

![](https://i.imgur.com/MGbIdSf.png)<!-- -->

Looks like we contributed more or less the same number of messages, with
Erika slightly leading the field.

Another thing that is fun to do is finding out what people’s favourite
emojis are:

``` r
library("tidyr")
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Most often used emojis") +
  theme_bw()
```

![](https://i.imgur.com/6bTX4u5.png)<!-- -->

Looks like we have a clear winner: all of us like the :joy: (“face with
tears of joy”) most. :sweat\_smile: (“grinning face with sweat”) is also
very popular, except with Erika who has a few more flamboyant
favourites. I apparently tend to use fewer emojis overall while Erika is
leading the field (again).

How does it look if we compare favourite words? I use the excellent
`tidytext` package to get this task done:

``` r
library("tidytext")
chat %>%
  unnest_tokens(input = "text",
                output = "word") %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = author)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used words") +
  theme_bw()
```

![](https://i.imgur.com/vOL4pf1.png)<!-- -->

This doesn’t make much sense. First of all, because we write in German
which you might not understand :wink:. But it also looks weird that
Artur and Erika seem to often use the words “media” and “omitted”. Of
course, this is just the placeholder WhatsApp puts into the log file
instead of a picture or video. But the other words don’t look
particularly useful either. They are what’s commonly called stopwords:
words that are used often but don’t carry any substantial meaning. “und”
for example is simply “and” in English. “der”, “die” and “das” all mean
“the” in English (which makes German a pain to learn for an English
native speaker).

To get around this mess, I remove these words before making the plot
again:

``` r
library("stopwords")
chat %>%
  unnest_tokens(input = "text",
                output = "word") %>%
  filter(!word %in% c(
    stopwords(language = "de"),
    "media",
    "omitted",
    "ref",
    "dass",
    "schon",
    "mal"
  )) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = author)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used words") +
  theme_bw()
```

![](https://i.imgur.com/KfRKCCd.png)<!-- -->

Still not very informative, but hey, this is just a private
conversation, what did you expect? It seems though that we agree with
each other a lot, as “ja” (“yes”) and ok are among the top words for all
of us. The antonym “ne” (“nope”) is far less common and only on Artur’s
and Erika’s top lists. I seem to send a lot of links as both “https” and
“ref” appear on my top list. Alexandra is talking to or about Erika and
me pretty often and Artur is the only one who mentions euro (as in the
currency) pretty often.

Another way to determine favourite words is to calculate the term
frequency–inverse document frequency (tf–idf). Basically, what the
measure does, in this case, is to find words that are common within the
messages of one author but uncommon in the rest of the messages.

``` r
chat %>%
  unnest_tokens(input = "text",
                output = "word") %>%
  select(word, author) %>%
  filter(!word %in% c(
    stopwords(language = "de"),
    "media",
    "omitted",
    "ref",
    "dass",
    "schon",
    "mal",
    "android.s.wt"
  )) %>%
  mutate(word = gsub(".com", "", word)) %>%
  mutate(word = gsub("^gag", "9gag", word)) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  filter(n > 10) %>%
  group_by(author) %>%
  top_n(n = 6, tf_idf) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = author)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Important words using tf–idf by author") +
  theme_bw()
```

![](https://i.imgur.com/PPQ1ru8.png)<!-- -->

Another common text mining tool is to calculate lexical diversity.
Basically, you just check how many unique words are used by an author.

``` r
chat %>%
  unnest_tokens(input = "text",
                output = "word") %>%
  filter(!word %in% c(
    stopwords(language = "de"),
    "media",
    "omitted",
    "ref",
    "dass",
    "schon",
    "mal"
  )) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
                          y = lex_diversity,
                          fill = author)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("unique words") +
  xlab("") +
  ggtitle("Lexical Diversity") +
  theme_bw() +
  coord_flip()
```

![](https://i.imgur.com/m5p3SGP.png)<!-- -->

Overall, WhatsApp data is just a fun source to play around with text
mining methods. If you have more serious data, a proper text analysis
might also be possible.
