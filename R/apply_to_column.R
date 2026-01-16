apply_to_column <- function(file_path,function_name,columns,include_columns = TRUE)
{
  #' apply_to_column: Apply a specified function to selected columns of a CSV file.
  #' @param file_path A character string containing the file path to a CSV file to be processed.
  #' @param function_name A function to be applied to the selected columns.
  #' @param columns A character vector of column names to include or exclude from processing.
  #' @param include_columns A logical value indicating whether to include (TRUE) or exclude (FALSE) the specified columns.
  #'
  #' @return A table containing the results of applying the specified function to the selected columns.
  #'
  #' @import dplyr
  #' @export
  #'
  #' @examples
  #' do_application("/home/caspian-lilypad/Downloads/move/summer 2025/capm_edited - capm_S25_fixed.csv",mean,c("Date"),TRUE)
  #'result:
  #'           BB         BBMR         TSXC       TSXCMR         GVTB       GVTBMR
  #' 7.368983e+00 1.301547e-02 2.052131e+04 9.028865e-03 2.487576e+00 2.045685e-03
  #'           XT           YT
  #' 6.983180e-03 1.096979e-02
  #'
  #'
  #'

  # Read the CSV file
  file_to_process <- data.frame(read.csv(file_path))

  # Select or exclude specified columns
  if(include_columns == TRUE) {required_columns <- select(file_to_process,all_of(columns))}
  else  {required_columns <- select(file_to_process,-all_of(columns))}

  # Apply the specified function to each selected column
  result <- sapply(required_columns,function_name)
  # Return the result
  print(result)
}
