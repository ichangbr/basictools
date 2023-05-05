# _ ____ ___ 
# | |___ |==]
# ---
# CREATED: 24.4.2023
# LAST MODIFIED: 3.5.2023


#' Free up memory by removing objects from the R environment
#'
#' This function can be used to remove one or more objects from the R environment
#' in order to free up memory. By default, all objects in the global environment
#' will be removed.
#'
#' @param objs A character vector of the names of objects to remove from the R
#'   environment. If \code{NULL}, all objects in the global environment will be
#'   removed.
#' @param exclude A logical value indicating whether to exclude the objects named
#'   in \code{objs} from removal instead of removing them. The default is
#'   \code{FALSE}.
#' @param echo A logical value indicating whether to print the amount of memory
#'   freed after removing the objects. The default is \code{FALSE}.
#'
#' @return This function is called for its side effects, and does not return a
#'   value.
#'
#' @examples
#' # Remove all objects from the global environment
#' freeMem()
#'
#' # Remove specific objects from the global environment
#' freeMem(c("myData", "myModel"))
#'
#' # Exclude specific objects from removal
#' freeMem(c("myData", "myModel"), exclude = TRUE)
#'
#' # Print the amount of memory freed by removing objects
#' freeMem(echo = TRUE)
#'
#' @seealso \code{\link{rm}}, \code{\link{gc}}
#'
#' @importFrom base ls rm setdiff
#'
#' @export

freeMem <- function(objs = NULL, exclude = F, echo = F){
  if(is.null(objs)){
    objs <- ls(envir = .GlobalEnv)
  } else if (exclude) {
    objs <- setdiff(ls(envir = .GlobalEnv), objs)
  }
  if(!is.vector(objs)) stop("objs must be a vector")
  if(!is.character(objs)) stop("objs must be a character vector")
  if(!length(objs) > 0) stop("objs must be length 1 or higher")
  if(!is.logical(echo) | !length(echo) == 1) stop("objs must be logical value")
  
  rm(list = intersect(objs, ls(envir = .GlobalEnv)), envir = .GlobalEnv)
  if(!echo) invisible(gc()) else gc()
}

