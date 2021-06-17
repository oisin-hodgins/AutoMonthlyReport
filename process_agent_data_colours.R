#'@title
#'Calculates the average number of coloured clicks in each Guided Conversation for each Agent
#'
#'@description
#'Appends the following columns to the agent_data tibble: "avg_percentage_blue",
#'"avg_percentage_red", "avg_percentage_green", "avg_percentage_purple". These
#'columns store the average number of clicks made by an agent accross all of their
#'guided conversations.
#'
#'@details
#'Iterates over all of core_data, storing the proportion of clicks made in each 
#'guided conversation (gcBlue/gcTotalBlue for example). Then divides this by the 
#'total number of guided conversations that agent had appends it to the agent_data
#'tibble.
#'This is a specific function used for generation of monthly report
#'Must be ran AFTER process_agent_data_calls
#'
#'@param data The data set containing the call data. Default = core_data.
#'@export

#Imports are found in the read_csv_monthly_report()
process_agent_data_colours <- function(data = core_data){
  #Append new columns to the agent_data tibble to store sums temporarily
  agent_data <- agent_data %>%
    dplyr::mutate(total_gcblue = 0) %>%
    dplyr::mutate(total_gcred = 0) %>%
    dplyr::mutate(total_gcgreen = 0) %>%
    dplyr::mutate(total_gcpurple = 0) 
  #loop counter
  i <- 1
  while(i <= nrow(agent_data)){
    j <- 1
    while(j <= nrow(data)){
      if(data$username[[j]] == agent_data$username[[i]] && data$outcome[[j]] == "GuidedConversation"){
        #Add the proportion of the total coloured clicks that were made in the call, to the sum
        #Note: *We are dividing by the total clicks each time we add a new call to the sum
        #in case the number of total clicks is not consistent throughout the whole dataset(ie.
        #the guided conversation interface has been changed during the period) however
        #this is not the whole solution, as the agent_performance_analysis will still
        #assume that the gcInterface has remained consistent throughout the period*
        agent_data$total_gcblue[[i]] <- agent_data$total_gcblue[[i]] + (data$gcBlues[[j]]/data$gcTotalBlues[[j]])
        agent_data$total_gcred[[i]] <- agent_data$total_gcred[[i]] + (data$gcReds[[j]]/data$gcTotalReds[[j]])
        agent_data$total_gcgreen[[i]] <- agent_data$total_gcgreen[[i]] + (data$gcGreens[[j]]/data$gcTotalGreens[[j]])
        agent_data$total_gcpurple[[i]] <- agent_data$total_gcpurple[[i]] + (data$gcPurples[[j]]/data$gcTotalPurples[[j]])
        }
      j <- j + 1
    }
    i <- i + 1
  }
  #Append these average columns to the agent data tibble
  agent_data <<- agent_data %>%
    dplyr::mutate(avg_percentage_blue = round((total_gcblue/total_guided),5)) %>%
    dplyr::mutate(avg_percentage_red = round((total_gcred/total_guided),5)) %>%
    dplyr::mutate(avg_percentage_green = round((total_gcgreen/total_guided),5)) %>%
    dplyr::mutate(avg_percentage_purple = round((total_gcpurple/total_guided),5)) %>%
    #Remove the sum columns as they are not needed anymore
    dplyr::select(-c(total_gcblue, total_gcred, total_gcgreen, total_gcpurple))
}