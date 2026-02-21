#!/usr/bin/env Rscript

source("renv/activate.R")
renv::install("./livrec", prompt = FALSE)
livrec::gha_validate_recipes()
