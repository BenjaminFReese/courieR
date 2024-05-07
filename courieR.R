## Packages
library(tidyverse)
library(haven)
library(readr)
library(readxl)
library(here)
library(jsonlite)
library(RSQLite)

#' Read in various types of datasets with one single function
#'
#' This function consolidates common packages that read in different file types into one
#' unified function. It reads in datasets of various formats including CSV, Excel, JSON,
#' Stata, RData, tab-delimited text, and SQLite databases. It also fully incorporates 
#' the functionality from the library(here) package. Instead of matching packages to 
#' file types, users can load in courieR and read in most file types.
#'
#' @param file A character string specifying the path to the dataset.
#' @param ... Additional arguments to be passed to the corresponding read functions for
#' the respective file types. This incorporates the total functionality of existing packages.
#'
#' @return A data frame or other appropriate R object containing the data from the file.
#'
#' @examples
#' courier("data.csv")
#' courier("data.xlsx", sheet = 1)
#'
#' @importFrom readxl read_excel
#' @importFrom readr read_csv read_tsv
#' @importFrom haven read_dta
#' @importFrom jsonlite fromJSON
#' @import RSQLite
#' @importFrom DBI dbConnect dbGetQuery
#' @importFrom here here
#'
#' @export
courier <- function(file, ...) {
  
  ## Constructing the full file path using here()
  full_path <- here(file)
  
  ## Check if the file exists
  if (!file.exists(full_path)) {
    stop("File not found.")
  }
  
  file_ext <- tools::file_ext(full_path)
  
  tryCatch({
    if (file_ext %in% c("xlsx", "xls")) {
      readxl::read_excel(full_path, ...)
    }
    else if (file_ext == "csv") {
      readr::read_csv(full_path, ...)
    }
    else if (file_ext == "dta") {
      haven::read_dta(full_path, ...)
    }
    else if (file_ext == "RData") {
      load(full_path)
      dta
    }
    else if (file_ext == "json") {
      jsonlite::fromJSON(file = full_path, ...)
    }
    else if (file_ext == "txt" || file_ext == "text" || file_ext == "tsv") {
      readr::read_tsv(full_path, ...)
    }
    else if (file_ext == "sqlite" || file_ext == "db") {
      RSQLite::dbGetQuery(dbConnect(SQLite(), dbname = full_path), "SELECT * FROM table_name", ...)
    }
    else {
      stop(" This is an unsupported file type. Supported file types include CSV, Excel, JSON, Stata, RData, tab-delimited text, and SQLite databases.")
    } 
  }, error = function(e) {
    stop("An error occurred while reading file:", " ", file, ".", conditionMessage(e))
  })
}
