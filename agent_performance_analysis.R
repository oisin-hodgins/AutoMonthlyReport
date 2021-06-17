#'@title
#'Creates agent_kpi_analysis tibble, which stores the agent performance data.
#'
#'@description
#'The agent_kpi_analysis tibble ranks all agents from [0,1] on a number of metrics,
#'and also combines these metrics into one overall predictor of performance. This
#'is a multiple criteria decision analysis problem we are solving, and the method used
#'is goal programming.
#'
#'@details
#'Stores relevant data in agent_kpi tibble, before standardising and normalizing
#'it in the agent_kpi_analysis tibble. The columns here are explicitly coded, with
#'the decision of min, max or mean standardisation already addressed. The means
#'of blue,green and purple clicks are calculated from successful guided conversations only.
#'Note: process_guided_data() must be run before this function.
#'
#'@param data The dataset containing relevant agent data. Default = agent_data.
#'@export
agent_performance_analysis <- function(data = agent_data){
  #Select relevant data from the agent_data tibble.
  #We are not concerned with the total amount of each outcome here, only the
  #proportion relative to the number of calls made.
  agent_kpi <<- agent_data %>%
    dplyr::select(-c(total_sales, total_referrals, total_guided, total_guided_success))
  #To correctly prepare the colour data from the GC interface for analysis, we must
  #calculate the average number of clicks for each colour for a successful guided conversation.

  #Here, select only relevant guided conversations
  guided_data_successful_only <- guided_data %>%
    dplyr::filter(success == 1)
  #Store mean values
  avg_successful_blue <- mean(guided_data_successful_only$`gc%Blues`)
  avg_successful_green <- mean(guided_data_successful_only$`gc%Greens`)
  avg_successful_purple <- mean(guided_data_successful_only$`gc%Purples`)

  #Now mutate all columns using the standardise() and normalize() functions, selecting
  #min,max or mean where relevant.

  agent_kpi_analysis <<- agent_kpi %>%
    dplyr::mutate(total_calls = normalize(max_standardise(total_calls))) %>%
    dplyr::mutate(sale_percent = normalize(max_standardise(sale_percent))) %>%
    dplyr::mutate(ref_percent = normalize(max_standardise(ref_percent))) %>%
    dplyr::mutate(guided_percent = normalize(max_standardise(guided_percent))) %>%
    dplyr::mutate(guided_success_percent = normalize(max_standardise(guided_success_percent))) %>%
    dplyr::mutate(avg_percentage_red = normalize(max_standardise(avg_percentage_red))) %>%
    dplyr::mutate(AHT_other = normalize(min_standardise(AHT_other))) %>%
    dplyr::mutate(AHT = normalize(mean_standardise(AHT))) %>%
    #For these three colours we explicity standardise them using the means from successful GC's
    dplyr::mutate(avg_percentage_blue = normalize(-abs((avg_percentage_blue-avg_successful_blue)/sd(avg_percentage_blue)))) %>%
    dplyr::mutate(avg_percentage_green = normalize(-abs((avg_percentage_green-avg_successful_green)/sd(avg_percentage_green)))) %>%
    dplyr::mutate(avg_percentage_purple = normalize(-abs((avg_percentage_purple-avg_successful_purple)/sd(avg_percentage_purple)))) %>%
    #Calculate the agent_performance column, by average all other columns(except username)
    dplyr::mutate(agent_performance = signif((total_calls + sale_percent + ref_percent + guided_percent + guided_success_percent + AHT_other + AHT + avg_percentage_red + avg_percentage_blue + avg_percentage_green + avg_percentage_purple)/11,2))
}
