# _ ____ ___ 
# | |___ |==]
# ---
# CREATED: 11.05.2023
# LAST MODIFIED: 11.05.2023

#' String Formatting Function
#'
#' This function replaces placeholders in a character string with values from a vector or from variables in the calling environment.
#'
#' @param pat A character string with placeholders in the form "{i}", where i is the index of the value to insert or the name of a variable in the calling environment.
#' @param vect A character vector of values to insert into the placeholders. If \code{NULL}, the function looks for variables in the calling environment with the same name as the placeholders.
#' @return A character string with the placeholders replaced by values from \code{vect} or the calling environment.
#' @keywords character
#' @export
#' @examples
#' fstring("The value of x is {1} and the value of y is {2}", c(3, 4))
#' fstring("The value of x is {x} and the value of y is {y}")

fstring <- function(pat, vect = NULL) {
  idxs <- regmatches(pat, gregexpr("(?<=\\{).+?(?=\\})", pat, perl = T))[[1]]
  if (!is.null(vect)) {
    idxs <- as.numeric(idxs)
    invisible(lapply(idxs, function(i) { pat <<- gsub(paste0("\\{",i,"\\}"), vect[i], pat) }))
  } else {
    invisible(lapply(idxs, function(i) { pat <<- gsub(paste0("\\{",i,"\\}"), get(i), pat) }))
  }
  return(pat)
}