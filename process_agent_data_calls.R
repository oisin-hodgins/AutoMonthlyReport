#'@title
#'Generates agent_data tibble.
#'
#'@description
#'Generates agent_data tibble with following columns:
#'"username", "total_calls", "total_sales", "total_referrals", "total_guided",
#'"sale_percent", "ref_percent", "guided_percent", "total_guided_success",
#'"guided_success_percent".
#'
#'@details
#'This function uses the dplyr package to sort through the core_data tibble for data
#'relating to calls, then uses the same package to join the data together.
#'The resulting agent_data tibble will be expanded further by the "process_agent_data_duration"
#'and the "process_agent_data_colours" functions. This tibble is used in the agent performance
#'analysis.
#'This is a specific function used for generation of monthly report.
#'
#'@param data The data set containing the call data. Default = core_data.
#'@param min_calls The minimum number of calls required for an agent's data to be stored. Default = 10.
#'@param at_least_one If true an agent needs at least one call of each outcome for their data to be stored. Default = TRUE.
#'@export

process_agent_data_calls <- function(data = core_data, min_calls = 10, at_least_one = TRUE){
  #NTS run process_guided_data first
  #Select total calls for each agent
  agent_all_call <- data %>%
    dplyr::select(username) %>%
    dplyr::group_by(username) %>%
    dplyr::tally() %>%
    tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
  #Change column name
  colnames(agent_all_call)[which(names(agent_all_call) == "n")] <- "total_calls"
  #Select total sales for each user
  agent_sale_only <- data %>%
    dplyr::select(username, outcome) %>%
    dplyr::filter(outcome == "Sale") %>%
    dplyr::group_by(username) %>%
    dplyr::tally() %>%
    tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
  colnames(agent_sale_only)[which(names(agent_sale_only) == "n")] <- "total_sales"
  #Define agent_data <- total calls & total sales
  #First join
  agent_data <- dplyr::full_join(agent_all_call,agent_sale_only)
  agent_data[is.na(agent_data)] <- 0
  #Select referral for each user, repeat process above used for sale
  #Possible room for code improvement here:
  #Define new function to perform joins, which searches core_data for column?
  #Then adds a new total column to tibble for each "instance" of that column?
  agent_ref_only <- data %>%
    dplyr::select(username, outcome) %>%
    dplyr::filter(outcome == "Referral") %>%
    dplyr::group_by(username) %>%
    dplyr::tally() %>%
    tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
  colnames(agent_ref_only)[which(names(agent_ref_only) == "n")] <- "total_referrals"
  agent_data <- dplyr::left_join(agent_data,agent_ref_only)
  agent_data[is.na(agent_data)] <- 0
  #Repeat for guided conversation
  agent_guided_only <- data %>%
    dplyr::select(username, outcome) %>%
    dplyr::filter(outcome == "GuidedConversation") %>%
    dplyr::group_by(username) %>%
    dplyr::tally() %>%
    tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
  colnames(agent_guided_only)[which(names(agent_guided_only) == "n")] <- "total_guided"
  agent_data <- dplyr::left_join(agent_data,agent_guided_only)
  agent_data[is.na(agent_data)] <- 0
  #Filter out any insignificant agents
  agent_data <- agent_data %>%
    dplyr::filter(total_calls > min_calls)
  if(at_least_one == TRUE){
    agent_data <- agent_data %>%
      dplyr::filter(total_sales > 1 & total_referrals > 1 & total_guided > 1)
  }
  #Now add the percentage columns
  agent_data <- agent_data %>%
    dplyr::mutate(sale_percent = round(total_sales/total_calls,3)) %>%
    dplyr::mutate(ref_percent = round(total_referrals/total_calls,3)) %>%
    dplyr::mutate(guided_percent = round(total_guided/total_calls,3))
  #Append guided_success column to record an agents record with guided conversations
  agent_guided_success_total <- guided_data %>%
    dplyr::select(username, success) %>%
    dplyr::filter(success == 1) %>%
    dplyr::group_by(username) %>%
    dplyr::tally()
  colnames(agent_guided_success_total)[which(names(agent_guided_success_total) == "n")] <- "total_guided_success"
  agent_data <- dplyr::left_join(agent_data,agent_guided_success_total)
  agent_data[is.na(agent_data)] <- 0
  #Add agent_data to global environment using `<<-`
  #Append the guided_success column
  agent_data <<- agent_data %>%
    dplyr::mutate(guided_success_percent = round(total_guided_success/total_guided,3))
}
