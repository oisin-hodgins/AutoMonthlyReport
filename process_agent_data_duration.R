#'@title
#'Calculates average duration of all calls for all agents
#'
#'@description
#'Appends AHT_other and AHT columns to existing agent_data tibble, which stores
#'the average duration of all calls an agent has made within current period, first
#'for calls which had the outcome "other" (AHT_other), then for all remaining
#'outcomes altogether(AHT)
#'
#'@details
#'This is a specific function used for generation of monthly report
#'Must be ran AFTER process_agent_data_calls
#'
#'@param data The data set containing the call data. Default = core_data.
#'@export

#Imports are found in the read_csv_monthly_report()

process_agent_data_duration <- function(data = core_data){
  #SELECT all calls, but only the associated duration, user name and outcome
  total_user_duration <- data %>%
    dplyr::select(username, outcome, duration) %>%
    tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
  #Add new columns to agent_data to record average duration across all calls
  agent_data <- agent_data %>%
    dplyr::mutate(AHT_other = 0) %>%
    dplyr::mutate(AHT = 0)
  #loop counter
  i=1
  j=1
  #While: iterate over all of agent_data
  while(i <= nrow(agent_data)){
    #While: iterate over all of total_user_duration
    while(j <= nrow(total_user_duration)){
      #If usernames match
      if(agent_data$username[[i]] == total_user_duration$username[[j]]){
        #If the call recorded had outcome == other
        if(total_user_duration$outcome[[j]] == "Other"){
          agent_data$AHT_other[[i]] <- agent_data$AHT_other[[i]] + total_user_duration$duration[[j]]
        }
        #If the call recorded had outcome == sale, referral or guided conversation
        else{
          agent_data$AHT[[i]] <- agent_data$AHT[[i]] + total_user_duration$duration[[j]]
        }
      }
      j <- j + 1 #Increase index
    }
    #If agent has no recorded calls [Although check has been previously made]
    #To prevent dividing by zero error
    if(agent_data$total_calls[[i]] == 0){
      agent_data$AHT_other[[i]] <- 0
      agent_data$AHT[[i]] <- 0
    }
    #If agent has at least one call in each category
    #Divide by total number of calls to get the average, as normal
    else if(agent_data$AHT_other[[i]] != 0 & agent_data$AHT[[i]] != 0){
      agent_data$AHT_other[[i]] <- round(agent_data$AHT_other[[i]]/(agent_data$total_calls[[i]]-agent_data$total_sales[[i]]-agent_data$total_guided[[i]]-agent_data$total_referrals[[i]]),3)
      agent_data$AHT[[i]] <- round(agent_data$AHT[[i]]/agent_data$total_calls[[i]],3)
    }
    #If AHT_other category is 0
    #Leave it at 0, and perform division on AHT as normal
    else if(agent_data$AHT_other[[i]] == 0){
      agent_data$AHT[[i]] <- round(agent_data$AHT[[i]]/agent_data$total_calls[[i]],3)
    }
    #If AHT category is 0
    #Leave it at 0, and perform division on AHT_other as normal
    else if(agent_data$AHT[[i]] == 0){
      agent_data$AHT_other[[i]] <- round(agent_data$AHT_other[[i]]/(agent_data$total_calls[[i]]-agent_data$total_sales[[i]]-agent_data$total_guided[[i]]-agent_data$total_referrals[[i]]),3)
    }
    j <- 1 #Reset index for next username
    i <- i + 1 #Increase index
  }
  agent_data <<- agent_data #Add to global environment
}
