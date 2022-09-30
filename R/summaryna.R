#' If any, returns the percentage of nas en each column of a data frame

#' @param x A data frame
#'
#' @param p The function estimates the percentage of NAs in each column.
#' The function will show the percentage of NAs per column.
#' "p" is a percentage, indicating that we want only columns with NA´s above p.
#'
#' @return A Data Frame
#'
#' @examples
#' dfna<-data.frame(col1 = c("cat1", "cat2", "cat3", "cat2", "cat1", NA), col2 = c(1, NA, 3, 4, NA, 6))
#' summaryna(dfna, 0.02)
#'
summaryna <- function(x,p=FALSE) {
  dim <- dim(x)
  prov <- c()
  co <- c()
  for (i in 1:dim[2]) {
    su <- sum(is.na(x[,i]))/dim[1]
    prov <- c(prov,su)
    ind <- c()
    co <- c(co,colnames(x[,i]))
  }

  me <- data.frame(prov)
  rownames(me) <- colnames(x)
  dim <- dim(me)
  se <- c(1:dim[1])
  me <- cbind(me,se)
  cole <- c()
  colem <- c()
  cole_name <- c()

  for (i in 1:dim[1]) {
    me[i,1] <- ifelse(is.na(me[i,1]) == TRUE,0,me[i,1])
  }

  for (i in 1:dim[1]) {

    if (me[i,1] > p) {
      cole <- c(cole,me[i,1])
      colem <- c(colem,me[i,2])
      cole_name <- c(cole_name,rownames(me)[i])
    }
  }
  col2 <- data.frame(cole)
  col2 <- cbind(col2,colem)
  rownames(col2) <- cole_name

  if (dim(col2)[1] != 0) {
    colnames(col2) <- c("Percentage of NAs","Column number")
  } else {col2 <- "There are no columns with missing values"}
  col2
}

