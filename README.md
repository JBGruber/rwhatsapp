
# rwhatsapp

[![Travis-CI Build Status](https://travis-ci.org/JBGruber/rwhatsapp.svg?branch=master)](https://travis-ci.org/JBGruber/rwhatsapp) [![Coverage Status](https://codecov.io/gh/JBGruber/rwhatsapp/branch/master/graph/badge.svg)](https://codecov.io/github/JBGruber/rwhatsapp?branch=master)

## Motivation

Whatsapp seems to become increasingly important not just as a messaging service but also as a social network---thanks to its group chat capabilities.
Furthermore, retrieving chat logs from the Android or iOS app is very straightforward:
Simply choose `More` in the menu of a chat, then `Export chat` and export the history to a txt file.

![alt-text-1](https://i.stack.imgur.com/KBZvOb.png "menu") ![alt-text-2](https://i.stack.imgur.com/J1WoIb.png "more") ![alt-text-2](https://i.stack.imgur.com/c8uqJb.png "export")

This package is intended to provide some infrastructure to work with this text data in `R`.
So far it reads txt files into R and separates username, time and the messages into columns of a `data.frame`.
**Please create an issue if you have ideas for what can be useful functions or if you have problems with the existing function:** [**issue report**](https://github.com/JBGruber/rwhatsapp/issues).

## Installation

``` r
devtools::install_github("JBGruber/rwhatsapp")
```
