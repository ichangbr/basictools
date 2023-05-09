# _ ____ ___ 
# | |___ |==]
# ---
# CREATED: 05.05.2023
# LAST MODIFIED: 05.05.2023

#' Calculate the means of matrix-like objects divided into equal-sized groups
#'
#' @param x A matrix or data frame.
#' @param n An integer specifying the number of columns or rows to group together.
#' @param over A character string indicating whether to group by columns ("c" or "col") or rows ("r" or "row").
#' @param fun An optional function to apply to each group. Defaults to \code{mean}.
#'
#' @return A matrix or data frame with the means of every n columns or rows.
#'
#' @details
#' The function takes in a matrix or data frame \code{x} and groups the data by every \code{n} columns or rows, depending on whether \code{over} is set to "c"/"col" or "r"/"row", respectively. 
#' 
#' If \code{over} is set to "c"/"col", the selected columns and all rows will be used to calculate the means, while if \code{over} is set to "r"/"row", the selected rows and all columns will be used.
#' 
#' If \code{x} is a tibble, the resulting data will be returned as a tibble; otherwise, it will be returned as a matrix or data frame depending on the class of the input.
#'
#' @examples
#' nMeans(matrix(1:9, ncol = 3), 2, over = "col")
#' nMeans(data.frame(matrix(1:9, ncol = 3)), 2, over = "row")
#'
#' @export

nMeans <- function(x, n, over = "c", fun = NULL) {
  # Error Messages
  if(!class(x)[1] %in% c("tbl_df", "matrix", "data.frame")) stop("Input x must be a matrix, data.frame or tibble")
  if(!over %in% c("c", "r", "col", "row")) stop("over must one of \"c\", \"r\", \"col\", \"row\".")
  over_cols <- over %in% c("c", "col")
  mx <- ifelse(over_cols, ncol(x), nrow(x))
  err_str <- ifelse(over_cols, "columns", "rows")
  marg <- ifelse(over_cols, 1, 2) # Margin
  bind <- ifelse(over_cols, cbind, rbind)
  if(mx%%n != 0) stop(sprintf("number of %s is not multiple of n", err_str))
  f <- ifelse(is.null(fun), mean, fun)
  idxs <- seq(1,mx,n)
  idxs <- Map(c, idxs, idxs + (n - 1))
  if (over_cols) {
    do.call(bind, Map(function(c) apply(x[, c[1]:c[2]], marg, f, na.rm = T), idxs))
  } else {
    do.call(bind, Map(function(c) apply(x[c[1]:c[2], ], marg, f, na.rm = T), idxs))
  }
}
