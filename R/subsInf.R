# _ ____ ___ 
# | |___ |==]
# ---
# CREATED: 09.05.2023
# LAST MODIFIED: 09.05.2023

#' Replace infinite values in a matrix with a specified value
#'
#' This function takes in a matrix-like object and replaces any infinite values in the matrix with a specified value, which defaults to \code{NA}.
#'
#' @param x A matrix, data frame, or tibble
#' @param replace_with An optional value to replace infinite values with. Defaults to \code{NA}.
#'
#' @return A modified matrix with infinite values replaced by \code{replace_with}.
#'
#' @examples
#' subsInf(matrix(c(1, 2, -Inf, 4, 5, 6, Inf, Inf, 9), nrow = 3), replace_with = -999)
#' subsInf(matrix(c(1, 2, Inf, 4, 5, Inf), nrow = 2))
#'
#' @export

subsInf <- function(x, replace_with = NA) {
  # Error Messages
  if(!class(x)[1] %in% c("tbl_df", "matrix", "data.frame")) stop("Input x must be a matrix, data.frame or tibble")
  
  return(
    apply(x,2,function(x) {x[is.infinite(x)] <- replace_with; return(x)})
  )
}
