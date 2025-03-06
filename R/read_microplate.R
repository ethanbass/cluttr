#' Read KC4 microplate
#'
#' Read microplate data from BioTek KC4 \code{.txt} format
#'
#' @importFrom utils read.csv
#' @param path Path to BioTek KC4 \code{.txt} file.
#' @param long_format Logical. Whether to return data in long format. Defaults
#' to \code{TRUE}.
#' @author Ethan Bass
#' @export

read_microplate <- function(path, long_format = TRUE){
  xx <- readLines(path)
  start_lines <- grep(";1;2;3;4;5;6;7;8;9;10;11;12", xx) - 1
  lapply(start_lines, function(i){
    df <- read.csv(path, sep = ";", skip = i, nrows = 8)
    if (long_format){
      df <- tidyr::pivot_longer(df, cols = -c("X"))
      colnames(df) <- c("row", "column", "value")
      df$column <- as.numeric(gsub("X", "", df$column))
    }
    as.data.frame(df)
  })
}
