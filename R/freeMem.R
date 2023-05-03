# _ ____ ___ 
# | |___ |==]
# ---
# CREATED: 24.4.2023
# LAST MODIFIED: 3.5.2023


#' Free Memory
#' 
#' Deletes selected files from the environment
#' @param objs Character vector of the names of objects to delete. If left empty all objects will be deleted
#' @param exclude Boolean to determine whether to keep or discard the selected objects
#' @param echo If true, output for gc() is shown. F by default
#' @examples
#' a <- c(1, 2)
#' b <- c(1, 2, 3)
#' freeMem(c("a", "b"), echo = T)
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

