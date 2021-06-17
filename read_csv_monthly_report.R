#' @title
#' Specific Monthly Report Function: Read csv file.
#'
#' @description
#' Reads csv generated from call data, and specifies col_type and col_names.
#'
#' @details
#' The expected columns are: trailLogId, customerId, username, newCustomer,
#' duration, outcome, quailityConversation, colouredConversation, gcBlues,
#' gcTotalBlues, gcReds, gcTotalReds, gcGreens, gcTotalGreens, gcPurples,
#' gcTotalPurples.
#' There was a need to specify the data types manually here. We use a default of
#' col_skip() to allow for the addition of new columns easily
#'
#' @param filename The filename/path of the csv file.
#' @importFrom magrittr %>%
#' @import tidyverse
#' @import formattable
#' @import aod
#' @export
read_csv_monthly_report <- function(filename){
  core_data <<- readr::read_csv(
    file = filename,
    col_types = readr::cols(
      trailLogId = readr::col_integer(),
      customerId = readr::col_integer(),
      username = readr::col_character(),
      newCustomer = readr::col_integer(),
      duration = readr::col_integer(),
      outcome = readr::col_character(),
      qualityConversation = readr::col_integer(),
      coloredConversation = readr::col_integer(),
      gcBlues = readr::col_integer(),
      gcTotalBlues = readr::col_integer(),
      gcReds = readr::col_integer(),
      gcTotalReds = readr::col_integer(),
      gcGreens = readr::col_integer(),
      gcTotalGreens = readr::col_integer(),
      gcPurples = readr::col_integer(),
      gcTotalPurples = readr::col_integer(),
      .default = readr::col_skip()
    )
  )%>%
  tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
  core_data[(is.na(core_data))] <- 0
}
