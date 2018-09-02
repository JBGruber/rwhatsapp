# how to write an R package https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

library("devtools")
library("roxygen2")

setwd("/home/johannes/Documents/Github/rwhatsapp")

#devtools::build_vignettes()

# update description
desc <- readLines("DESCRIPTION")
date <- desc[grepl("^Date:", desc)]
date2 <- gsub("[^[:digit:]-]", "", date)
desc[grepl("^Date:", desc)] <- gsub(date2, Sys.Date(), desc[grepl("^Date:", desc)])
vers <- desc[grepl("^Version:", desc)]
vers2 <- gsub("[^[:digit:].]", "", vers)
vers3 <- readline(prompt = paste("New Version? Old:", vers2))
if (vers3 == "") {
  vers3 <- vers2
}
desc[grepl("^Version:", desc)] <- gsub(vers2, vers3, desc[grepl("^Version:", desc)])
writeLines(desc, "DESCRIPTION")

# update documentation
roxygen2::roxygenise(clean = TRUE)
setwd("..")
devtools::check("rwhatsapp")

devtools::spell_check("rwhatsapp", dict = "en_GB", ignore = c(
  "rwhatsapp",
  "whatsapp"
))

hunspell::hunspell(readLines("rwhatsapp/README.md"), dict = "en_GB", ignore = c(
  "rwhatsapp",
  "whatsapp"
))


library(lintr)
lint_package("rwhatsapp")

devtools::install("rwhatsapp")

# create the package in wd
path <- devtools::build("rwhatsapp", vignettes = T, manual = TRUE)
devtools::check_built(path = path)

devtools::build

# build manual
setwd("/home/johannes/Documents/Github/")
unlink("rwhatsapp.pdf")
system("R CMD Rd2pdf rwhatsapp")


### test
library("rwhatsapp")
?lnt_diff

covr::package_coverage("/home/johannes/Documents/Github/rwhatsapp")

setwd("/home/johannes/Documents/Github/rwhatsapp")
roxygen2::roxygenise(clean = TRUE)
setwd("..")
#devtools::check("rwhatsapp")
devtools::install("rwhatsapp")

