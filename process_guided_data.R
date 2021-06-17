#'@title
#'Generates the guided_data tibble used for Guided Conversation analysis
#'
#'@description
#'Filters all 'Sale', 'Referral' and 'Other' calls out of the core_data tibble,
#'also adding five new columns to store the percentage of coloured clicks made
#'in each guided conversation, and to store the result of a guided conversation.
#'The eval_guided() function is called here to determine that outcome.
#'
#'@details
#'Iterates over core_data filtering out irrelevant calls.
#'The new "gc%Blues", "gc%Reds", "gc%Greens" and "gc%Purples" should remain consistent
#'if the guided conversation architecture changes throughout the period, as it is calculated
#'from the 'total' value in each call log.
#'
#'@param data The data set containing the call data. Default = core_data.
#'@export

#NOTE: Currently the eval_guided() function is commented out, and the success col
#is populated with a random variable [0,1]


process_guided_data <- function(data = core_data){
  #Select relevant data
  guided_data <- data %>%
    dplyr::filter(outcome == "GuidedConversation") %>%
    dplyr::mutate(`gc%Blues` = gcBlues/gcTotalBlues, `gc%Reds` = gcReds/gcTotalReds,
                  `gc%Greens` = gcGreens/gcTotalGreens, `gc%Purples` = gcPurples/gcTotalPurples,
                  success = 0, outcome = NULL) %>%
    tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
  #Can't use mutate for success column
  #Must loop over guided_data instead as eval_guided() takes int argument not array
  #Loop counter
  i=1
  while(i < nrow(guided_data)){
    #Store all successful results
    #if(ReportFun::eval_guided(data, guided_data$trailLogId[[i]]) == TRUE){
    #guided_data$success[[i]] <- 1
    #}

    #Temporary workaround here, use a random variable as dataset is unsuitable
    guided_data$success[[i]] <- rbinom(1,1,0.5) #Set to 1,0 at random
    #Please remove this line when implementing ^^^

    #Increase loop counter
    i <- i + 1
  }
  #Send guided_data to the global environment
  guided_data <<- guided_data %>%
    tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
}
