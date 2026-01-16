limits_scan <- function(file_path, column_name, lower_limit, upper_limit, charac=FALSE)
{
  #' limits_scan: Scan a column for values outside specified limits and return matching rows.
  #' @param file_path A character string containing the file path to a CSV file to be scanned.
  #' @param column_name A character string specifying the column name to scan.
  #' @param lower_limit A numeric value specifying the lower limit (a). Values < lower_limit are considered outside.
  #' @param upper_limit A numeric value specifying the upper limit (b). Values > upper_limit are considered outside.
  #' @param charac A logical value indicating whether the limit is a character limit (TRUE) or numeric limit (FALSE). Default is FALSE.
  #'
  #' @return A data frame containing all rows where the specified column has values outside [lower_limit, upper_limit].
  #'
  #' @import dplyr
  #' @import readr
  #' @export
  #'
  #' @examples
  #'

  file_to_process <- data.frame(read.csv(file_path))

  # Check if column exists
  if (!column_name %in% names(file_to_process)) {
    stop(paste("Error: Column '", column_name, "' not found in CSV.", sep = ""))
  }

  # Filter rows based on limits and charac flag:
  # return rows where values are outside the limits on either value or length
if (charac) {
    outlier_rows <- file_to_process %>%
        filter(nchar(as.character(!!sym(column_name))) < lower_limit | nchar(as.character(!!sym(column_name))) > upper_limit)
  }
  else {
    outlier_rows <- file_to_process %>%
      filter(!!sym(column_name) < lower_limit | !!sym(column_name) > upper_limit)
  }

  return(outlier_rows)
}
