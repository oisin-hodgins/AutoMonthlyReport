#'@title
#'Performs multivariate logistic regression on guided_data
#'
#'@description
#'Adds guided_logit and guided_logit_stat_signif to the global environment.
#'
#'@details
#'Multivariate logistic regression. Success is a binary response variable, while
#'duration, gc%Blues, gc%Reds, gc%Greens and gc%Purples are all predictor variables.
#'Regression is performed using glm() function(from stats).
#'A Wald-test is performed on each predictor using the wald.test() function from
#'the "aod" package, and the p-values are recorded in the guideded_logit_stat_signif
#'tibble, along with a TRUE/FALSE depending on the 0.05 significance threshold.
#'The goodness of fit of the model is tested through a likelihood ratio test,
#'and a series of text outputs describe the result.
#'
#'@param data The dataset containing the guided conversation specific data. Default = guided_data.
#'@export
logit_reg_guided <- function(data = guided_data){
  #success is a binary response variable
  #Consider the following predictor variables:
  #duration, gc%Blues, gc%Reds, gc%Greens, gc%Purples
  logit_data <- data %>%
    dplyr::select(duration, `gc%Blues`,`gc%Reds`, `gc%Greens`, `gc%Purples`, success) %>%
    tibble::as_tibble(validate = FALSE, .name_repair = "minimal")
  #Apply standard deviation function to the data
  sapply(logit_data, sd)
  #Create the multivariate logistic regression model
  guided_logit <<- stats::glm(success ~ duration+`gc%Blues`+`gc%Reds`+`gc%Greens`+`gc%Purples`, data = logit_data, family = "binomial")
  #Show summary of the model
  summary(guided_logit)
  ## CIs using profiled log-likelihood
  stats::confint(guided_logit)
  ## CIs using standard errors
  stats::confint.default(guided_logit)
  ##Test for overall effect of each coefficient
  #Create tibble to store P-values
  guided_logit_stat_signif <<- tidyr::tibble(
    `coeffs` = c("duration", "gc_percent_Blues","gc_percent_Reds", "gc_percent_Greens", "gc_percent_Purples"),
    `P-value` = 0,
    `stat_significance` = FALSE #True//False value if (P<0.05)
  )
  #Declare loop counter at 2, as we need to reference the position of the
  #column in the guided_logit_stat_signif tibble.
  i <- 2
  #Loop over coefficients (each predictor variable)
  while(i <= 6){
    #Perform wald-test
    single_param_wald_test <- aod::wald.test(b = coef(guided_logit), Sigma = vcov(guided_logit), Terms = i)
    #Store P-value
    guided_logit_stat_signif$`P-value`[[(i-1)]] <- signif(single_param_wald_test$result$chi2[[3]],2)
    #Set statistical significance to TRUE if true
    if(single_param_wald_test$result$chi2[[3]] < 0.05){
      guided_logit_stat_signif$stat_significance[[(i-1)]] <- TRUE
    }
    i <- i + 1 #Increase counter
  }
  #Test for model goodness of fit
  #Likelihood ratio test
  model_fit_p <- with(guided_logit, stats::pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
  #Output the result
  print("Testing the goodness of fit of the model")
  #State P-Value
  cat("With a p-value of: ", signif(model_fit_p,3))
  if(model_fit_p < 0.01){
    #If the P-Value is less than 0.01 we can say...
    message("We say that the model as a whole fits significantly better than the null model")
    message("With 99% confidence")
  }
  else if(model_fit_p < 0.05){
    #If the P-Value is less than 0.05 we can say...
    message("We say that the model as a whole fits significantly better than the null model")
    message("With 95% confidence")
  }
  else{
    #The P-Value is greater than 0.05 and will have to be interpreted elsewhere
    message("We cannot conclude that the model is significantly better than the null model")
    message("With a confidence level of 95%")
  }
  #Now present the guided_logit_stat_signif table
  #Using the formattable() function
  #From the package of the same name
  formattable::formattable(guided_logit_stat_signif)
}
