#'@title
#'Evaluate if a guided conversation was successful or not.
#'
#'@description
#'Given a call ID and call data this function searches all calls after given ID,
#'with the same customer ID, and returns TRUE if a sale/referral took place,
#'returns FALSE if not
#'
#'@details
#'This is a specific function used for generation of monthly report.
#'If the input call is not a guided conversation this function will print
#'an error statement.
#'
#'
#'@param data The data set containing the call data. Default = core_data.
#'@param call_id The trailLogID of the call to be evaluated.
#'@return Logical TRUE//FALSE statement depending on outcome of guided conversation
#'will print error warning if given Id is not a guided conversation
#'@export

#Imports are found in the read_csv_monthly_report()

eval_guided <- function(data = core_data, call_id){
  #Select outcome of given call from call set
  test_for_guided <- data %>%
    dplyr::select(outcome) %>%
    dplyr::filter(data$trailLogId == call_id) %>%
    tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
  #Check if entry is a Guided Conversation
  if(test_for_guided$outcome == "GuidedConversation"){
    #If TRUE:
    #Find the customer who made the given call
    temp <- data %>%
      dplyr::select(customerId) %>%
      dplyr::filter(data$trailLogId == call_id) %>%
      tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
    #Store all followup calls (id & outcome) made to//from that customer
    followup <- data %>%
      dplyr::select(trailLogId, outcome) %>%
      dplyr::filter(data$customerId == temp$customerId && data$trailLogId > call_id) %>%
      tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
    #Filter out any followup calls with outcome = "Other" or "GuidedConversation"
    followup <- followup %>%
      dplyr::select(trailLogId, outcome) %>%
      dplyr::filter(followup$outcome == "Sale" | followup$outcome == "Referral") %>%
      tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
    if(nrow(followup) > 0){
      #If at least 1 subsequent call
      return(TRUE)
    }
    else{
      #If no subsequent call
      return(FALSE)
    }
  }
  else{
    #ERROR: Input Id not a Guided Conversation
    message("Error: Not a Guided Conversation")
  }
}
