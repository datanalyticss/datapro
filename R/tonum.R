#' Transforming a categorical column of a data frame into numeric
#'
#' @param data A data frame
#'
#' @param col column name to be transformed
#'
#' @return A Data Frame
#'
#' @examples
#' cat <- data.frame(col1 = c("cat1", "cat2", "cat3", "cat2", "cat1", NA), col2 = c(1, 2, 3, 4, 5, 6))
#' tonum(cat, "col1")
#'

tonum <- function(data,col) {
  dim <- dim(data)
  fe <- c()
  for (i in 1:dim[1]) {
    if (is.na(data[i,col]) == TRUE) {
      next
    } else { fe <- c(fe,data[i,col])
    }
  }
  cla <- data.frame(class = table(fe))
  colnames(cla) <- c("categ","freq_cat")
  cate_var <- data.frame(cate_var = as.factor(fe))
  nu_var <- cate_var
  nu_var[,"cate_var"] <- unclass(nu_var[,"cate_var"])

  nu <- data.frame(class = table(nu_var))

  nu_2 <- nu[,1]
  cla_2 <- cla[,1]
  dim2 <- length(cla_2)

  for (j in 1:dim2[1])
  {
    for (i in 1:dim[1]) {
      if (is.na(data[i,col]) == TRUE)
      { next
      } else {
        if (data[i,col] == cla_2[j]) {
          data[i,col] <- as.numeric(nu_2[j])
        } else {
          data[i,col] <- data[i,col]
        }
      }
    }
  }
  data[, col] <- as.numeric(data[, col])
  data
}
