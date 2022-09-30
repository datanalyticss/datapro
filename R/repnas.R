#' Replace NAs of a a data frame by a metric, such as median or mean

#' @param data A data frame
#'
#' @param metric column of the data frame
#'
#' @return A Data Frame
#'
#' @examples
#'dfna<-data.frame(col1 = c("cat1", "cat2", "cat3", "cat2", "cat1", NA), col2 = c(1, NA, 3, 4, NA, 6))
#'repnas(dfna,"median")


repnas <- function(data,metric) {
  dim <- dim(data)
  for (i in 1:dim[2])
  {
    data[,i][is.na(data[,i])] <- if (metric == "median") {
      stats::median( data[,i],na.rm = TRUE)} else {
        if (metric == "mean") {
          mean( data[,i],na.rm = TRUE)}

      }
  }
  data
}
