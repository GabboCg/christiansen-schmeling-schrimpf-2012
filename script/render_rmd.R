# run long sample
source("script/long_sample.R")

# run short sample
source("script/short_sample.R")

# run overview
rmarkdown::render("rmd/overview.Rmd", "pdf_document")
  