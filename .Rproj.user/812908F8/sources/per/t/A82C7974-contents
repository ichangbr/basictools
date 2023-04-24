

freeMem <- function(objs, echo = F){
  if(!is.vector(objs)) stop("objs must be a vector")
  if(!is.character(objs)) stop("objs must be a character vector")
  if(!length(objs) > 0) stop("objs must be length 1 or higher")
  if(!is.logical(echo) | !length(echo) == 1) stop("objs must be logical value")
  rm(list = objs, envir = .GlobalEnv)
  if(!echo) invisible(gc()) else gc()
}

