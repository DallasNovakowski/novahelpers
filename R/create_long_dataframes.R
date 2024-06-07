#' Create Long Dataframes and Save to Excel
#'
#' This function pivots a dataframe to a long format for each specified keyword, processes the variable names,
#' and saves the resulting long dataframes as separate sheets in an Excel workbook.
#'
#' @param data A dataframe to be pivoted to long format.
#' @param keywords A vector of keywords used to select columns for pivoting.
#' @param whitelist A vector of column names to be included in the final long dataframe along with the pivoted data.
#' @param filename The name of the output Excel file. Default is "data_long.xlsx".
#' @param overwrite A logical value indicating whether to overwrite the existing file if it already exists. Default is FALSE.
#' @return None. The function saves the long dataframes as sheets in an Excel workbook.
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Checks if the output file already exists. If it does and `overwrite` is FALSE, the function stops with an error message.
#'   \item Creates a new Excel workbook.
#'   \item Iterates over each keyword in `keywords`:
#'   \item Pivots the dataframe to long format for columns containing the keyword.
#'   \item Cleans up the variable names by removing the keyword, leading/trailing underscores, and double underscores.
#'   \item Converts underscores to spaces and capitalizes the variable names.
#'   \item Selects columns specified in the `whitelist` along with the cleaned variable names and their values.
#'   \item Adds the long dataframe as a new sheet in the Excel workbook.
#'   \item Saves the Excel workbook to the specified filename.
#' }
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   ID = 1:5,
#'   Age = c(23, 34, 45, 56, 67),
#'   Keyword_A_Score = c(10, 20, 30, 40, 50),
#'   Keyword_B_Score = c(15, 25, 35, 45, 55)
#' )
#' keywords <- c("Keyword_A", "Keyword_B")
#' whitelist <- c("ID", "Age")
#' create_long_dataframes(data, keywords, whitelist, filename = "data_long.xlsx", overwrite = TRUE)
#' }
#' @export
create_long_dataframes <- function(data, 
                                   keywords, 
                                   whitelist, 
                                   filename = "data_long.xlsx", 
                                   overwrite = FALSE) {
  # Check if the file already exists
  if (file.exists(filename) && !overwrite) {
    stop("File already exists! Set overwrite = TRUE to overwrite the existing file.")
  }
  
  # Create a new Excel workbook
  wb <- openxlsx::createWorkbook()
  
  # Iterate over each keyword
  for (keyword in keywords) {
    # Pivot longer to gather columns containing the keyword
    long_df <- tidyr::pivot_longer(data, 
                                   cols = matches(keyword), 
                                   names_to = "variable", 
                                   values_to = "value")
    
    # Remove the targeted keywords from the variable names
    for (kw in keywords) {
      long_df$variable <- gsub(kw, "", long_df$variable, ignore.case = TRUE)
    }
    
    # Remove leading and trailing underscores from variable names
    long_df$variable <- gsub("^_|_$", "", long_df$variable)
    
    # Replace double underscores with a single underscore
    long_df$variable <- gsub("__", "_", long_df$variable)
    
    long_df$variable <- gsub("_", " ", long_df$variable)
    
    long_df$variable <- str_to_title(long_df$variable)
    
    # Include only selected variables
    long_df <- long_df %>% 
      select(all_of(whitelist), variable, value)
    
    
    
    # Add the long dataframe as a new sheet in the Excel workbook
    openxlsx::addWorksheet(wb, sheetName = keyword)
    openxlsx::writeData(wb, sheet = keyword, x = long_df)
  }
  
  # Save the Excel workbook
  openxlsx::saveWorkbook(wb, file = filename, overwrite = overwrite)
}
