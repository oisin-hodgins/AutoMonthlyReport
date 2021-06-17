#'@title
#'Creates a heat table to visualise the agent performance data
#'
#'@description
#'Uses the formattable package to create this table specifically for a HTML presentation.
#'
#'@details
#'The column names are altered to be aesthetically pleasing, in a copy of the agent
#'performance analysis dataset. The formattable object is created using formattable()
#'and then changed to a data table through as.datatable() due to formating issues in
#'the HTML presentation.
#'Must be run after agent_performance_analysis().
#'
#'@param data The dataset containing the performance analysis data. Default = agent_kpi_analysis.
#'@export

display_agent_performance_table <- function(data = agent_kpi_analysis){
  #Alter the column names, to make the data more presentable
  agent_kpi_display <- data %>%
    dplyr::rename(Username = username) %>%
    dplyr::rename(`Total Calls` = total_calls) %>%
    dplyr::rename(`Sale Percent` = sale_percent) %>%
    dplyr::rename(`Referral Percent` = ref_percent) %>%
    dplyr::rename(`GC Percent` = guided_percent) %>%
    dplyr::rename(`AHT (Other)` = AHT_other) %>%
    dplyr::rename(gcBlue = avg_percentage_blue) %>%
    dplyr::rename(gcRed = avg_percentage_red) %>%
    dplyr::rename(gcGreen = avg_percentage_green) %>%
    dplyr::rename(gcPurple = avg_percentage_purple)
  #The formattable() function creates a HTML table, using the color_tile() function
  #To create the heatmap. This heatmap will colour values near 0 light pink, and those
  #near 1 light blue. All columns except the first column "username" are coloured.
  formattable::as.datatable(formattable::formattable(agent_kpi_display, lapply(1:nrow(data), function(row) {
    area(row, col = -1) ~ formattable::color_tile("lightpink", "lightblue")
  })))
  #as.datatable() function is used due to formatting issues with HTML presentation
}



