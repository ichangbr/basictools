# _ ____ ___ 
# | |___ |==]
# ---
# CREATED: 28.04.2023
# LAST MODIFIED: 28.04.2023

#' Split a data frame or matrix into two or more horizontal sections
#'
#' This function splits a data frame or matrix into two sections based on the
#' column number specified in the \code{cnum} argument. If \code{multisplit} is
#' set to \code{TRUE}, the data frame or matrix is split into multiple sections
#' based on the number of columns specified in \code{cnum}.
#'
#' @param df A data frame, tibble, or matrix to be split.
#' @param cnum The column number to split the data frame or matrix on.
#' @param multisplit A logical value indicating whether to split the data frame
#'   or matrix into multiple sections. The default is \code{FALSE}.
#'
#' @return If \code{multisplit} is \code{FALSE}, this function returns a list
#'   containing two data frames: the first containing columns 1 through \code{cnum},
#'   and the second containing columns (\code{cnum}+1) through the end. If
#'   \code{multisplit} is \code{TRUE}, this function returns a list of data frames,
#'   with each data frame containing the number of columns specified in \code{cnum}.
#'
#' @examples
#' # Split a data frame into two sections
#' hsplit(iris, 3)
#'
#' # Split a matrix into two sections
#' hsplit(matrix(1:20, nrow = 4), 2)
#'
#' # Split a data frame into multiple sections
#' hsplit(iris, 2, multisplit = TRUE)
#'
#' @seealso \code{\link{vsplit}}
#'
#' @importFrom base class stop ncol lapply seq as.data.frame
#'
#' @export

hsplit <- function(df, cnum, multisplit = F) {
  
  # error conditions
  cls <- class(df)
  if (!any(class(df) %in% c("data.frame", "tbl_df", "matrix"))) stop("df not a data.frame, tibble, or matrix type")
  if (!(class(cnum) == "numeric" & length(cnum) == 1)) stop("cnum not a number")
  if (!(class(multisplit) == "logical" & length(multisplit) == 1)) stop("multisplit not TRUE or FALSE")
  len <- ncol(df)
  if (!multisplit) {
    df1 <- df[, 1:cnum, drop = F]
    df2 <- df[, (cnum+1):len, drop = F]
    
    return(list(df1 = as.data.frame(df1), df2 = as.data.frame(df2)))
  } else {
    idxs <- lapply(seq(1, len, by = cnum), function(i){i:min(i+(cnum - 1), len)})
    res <- list()
    n = 1
    print(idxs)
    for (idx_vec in idxs){
      str_name <- paste0("df", n)
      res[[str_name]] <- df[, idx_vec, drop = F]
      n = n + 1
    }
    return(res)
  }
}
