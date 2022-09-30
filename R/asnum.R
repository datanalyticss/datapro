#' Transforming a categorical columns of a data frame into numeric
#'
#' @param x A data frame
#'
#' @return A Data Frame.
#'
#' @examples
#' cat <- data.frame(col1 = c("cat1", "cat2", "cat3", "cat2", "cat1", NA), col2 = c(1, 2, 3, 4, 5, 6))
#' asnum(cat)

asnum <- function(x) {
  cha <- charname(x)
  le <- length(cha)
  me <- "There are no character columns"

  if (cha[1] == me) {
    y <- "There are no character columns to transform"
  } else if (le == 1) {
    y <- tonum(x, cha[1])
  } else if (le >= 1) {
    y <- tonum(x, cha[1])

    for (i in 2:le) {
      y <- tonum(y, cha[i]) }
  }
  y
}

