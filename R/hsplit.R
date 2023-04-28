# _ ____ ___ 
# | |___ |==]
# ---
# CREATED: 28.04.2023
# LAST MODIFIED: 28.04.2023

#' Horizontal Split
#' 
#' Splits `data.frame`, `tibble`, or `matrix` by column number.
#' @param df Object to split
#' @param cnum number of column to split. If `multisplit` `TRUE` it is the number of columns per resulting dataframe (the last `data.frame` is the remaining columns).
#' @param multisplit if `FALSE` split `df` in two. If `TRUE` split `df` in multiple data.frames with `cnum` column number, with the last one being the remainder columns.
#' @examples
#' dataframe <- data.frame(a = 1:5, b = 1:5, c = 1:5, d = 1:5, e = 1:5, f = 1:5)
#' 
#' ## split in two
#' hsplit(dataframe, cnum = 3)
#' 
#' ## split in multiple
#' hsplit(dataframe, cnum = 2, multisplit = T)
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
