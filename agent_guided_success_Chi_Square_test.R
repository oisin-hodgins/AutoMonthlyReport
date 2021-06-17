#'@title
#'Performs Chi-Square test for independence between agent Username and success of a Guided Conversation.
#'
#'@description
#'A frequency of at least 5 for every outcome, that is 5 successes and failures
#'for each agent, is expected for statistical accuracy. If this condition is not
#'met the test will go ahead but report all offending agents first. The result
#'of the is outputted as well.
#'
#'@details
#'This is a Chi-Square test for independence between two categorical variables,
#'with the null and alternate hypotheses stated in the output.
#'Plot data is also generated here, it will be used to visualise the data within
#'the HTML presentation. Note: process_guided_data() must be run before this function.
#'
#'@param data The dataset containing relevant agent data. Default = agent_data.
#'@export
agent_guided_success_Chi_Square_test <- function(data = agent_data){
  #First, check to see we have at least 5 successes and failures for each agent
  #Any less than this will result in a statistically inaccurate test
  users <- c() #List to store usernames of any agents with less than 5 successes or failures
  i<-1 #Loop counter
  while(i < nrow(data)){
    #If successes or failures of agent are less than 5
    if(data$total_guided_success[[i]] < 5 | (data$total_guided[[i]] - data$total_guided_success[[i]]) < 5){
      #Add username to the list
      users <- c(users, data$username[[i]])
    }
    i <- i + 1 #Increase counter
  }
  #If at least one agent was added to the list then the test will not be statistically
  #significant.
  if(length(users) >  0){
    message("Warning: There are too few instances of either successful or non-successful \nguided conversations to perform a statistically accurate test.")
    message("The following users have caused this issue")
    cat(users, sep = "\n") #Print the list of usernames, each on a new line
  }
  #Test for independence between two categorical variables:
  #agent username (the agent making a given call)
  #& guided_success (whether or not a guided conversation was successful)
  # H0 [null hypothesis] there is no relationship between username & success
  # H1 [alternate hypothesis] there is a relationship between username & success
  #Select relevant data
  agent_guided_success_table <- guided_data %>%
    dplyr::select(username, success) %>%
    tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
  #NOTE: The plot used to visualise this table above, is in the HTML presentation

  #manipulate data for graph, creating a new tibble as binary int success will be
  #converted to a factor
  plot_data <- agent_guided_success_table %>%
    tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
  #Convert to factor, so the order in ggplot legend is correct
  plot_data$success <- factor(as.logical(as.integer(plot_data$success)), levels = c("TRUE", "FALSE")) #levels specifies order
  plot_data <<- plot_data %>%
    tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
  #Perform Chi-Square test of independence
  #Warning: chisq.test() expects a frequency of 5 in each cell
  #For statistical accuracy
  agent_guided_success_test <- stats::chisq.test(agent_guided_success_table$username, agent_guided_success_table$success)
  #report result
  message("Performing Chi-Sqaure test for independence")
  message("Does the agent making a particular call, have a significant impact on its outcome?")
  message("H0: Null Hypothesis: These are independent variables")
  message("H1: Alternate Hypothesis: There is a significant relationship between these variables")
  cat("Sample size, n = ", nrow(agent_guided_success_table))
  message("Performing test...\n")
  if(agent_guided_success_test$p.value < 0.05){
    cat("Reject null hypothesis from p-value of : ", round(agent_guided_success_test$p.value,2))
    message("There is significant evidence to conclude that the agent making a call has a significant effect on its success")
  }
  else{
    cat("Fail to reject null hypothesis from p-value of: ", round(agent_guided_success_test$p.value,2))
    message("There is insufficient evidence to conclude that the agent making a call has a significant effect on its success")
  }
}
