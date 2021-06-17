#'@title
#'Find total number of several instances, used in the 'overview' section of the report
#'
#'@description
#'Generates three integer values: the total amount of outcome = "Sale",
#'"Referral", "GuidedConversation". These are global environment variables named:
#'"sale_total", "guided_total" and "referral_total". Also creates a tibble
#'containing all unique customer Ids and the number of calls they made during the month.
#'
#'@details
#'This is a specific function used for generation of monthly report
#'These global environment variables are not to be confused with the similarly
#'named columns of the agent_data tibble
#'
#'@param data The data set containing the call data. Default = core_data.
#'@export

#Imports are found in the read_csv_monthly_report()

find_total <- function(data = core_data){
  guided_total <<- data %>%
    dplyr::filter(outcome == "GuidedConversation") %>%
    dplyr::tally() %>%
    dplyr::pull()
  referral_total <<- data %>%
    dplyr::filter(outcome == "Referral") %>%
    dplyr::tally() %>%
    dplyr::pull()
  sale_total <<- data %>%
    dplyr::filter(outcome == "Sale") %>%
    dplyr::tally() %>%
    dplyr::pull()
  total_customers <<- core_data %>%
    dplyr::select(customerId) %>%
    dplyr::group_by(customerId) %>%
    dplyr::tally()
}





