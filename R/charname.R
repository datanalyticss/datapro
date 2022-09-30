#' Shows a vector of the categorical column name
#'
#' @param x A data frame
#'
#' @return A character vector.
#'
#' @examples
#' cat<-data.frame(col1=c("cat1","cat2","cat3","cat2","cat1"),col2=c(1,2,3,4,5))
#' charname(cat)

charname <- function(x)
{
  char_name <- c()

  for (i in 1:dim(x)[2])
  {
    if (methods::is(x[1,i], "character"))
    {
      char_name <- c(char_name,colnames(x[i]))
    }
  }

  if (length(char_name) == 0) {
    char_name <- "There are no character columns"
  }
  char_name
}
