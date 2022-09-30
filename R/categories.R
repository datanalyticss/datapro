#' Shows a vector with the categories of the column of a data frame

#' @param x A data frame
#'
#' @param y column of the data frame
#'
#' @return A character vector.
#'
#' @examples
#' cat<-data.frame(col1=c("cat1","cat2","cat3","cat2","cat1"),col2=c(1,2,3,4,5))
#' categ(cat,"col1")

categ <- function(x,y){

  x[,y][!duplicated(x[,y])]

}
