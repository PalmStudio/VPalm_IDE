#' Update progress
#' 
#' @description Callback function to update the Shiny progress bar
#'
#' @param value  The value to increment
#' @param detail A character string about the event
#' 
#' @details If `value` is NULL, the function will move the progress bar by 1/steps 
#' of the remaining distance.
#' If non-NULL, it will set the progress to that value.
#'
#' @export
updateProgress= function(value = NULL, detail = NULL,progress_obj,steps) {
  if (is.null(value)) {
    value <- progress_obj$getValue()
    value <- value + (progress_obj$getMax() - value) / steps
  }
  progress_obj$set(value = value, detail = detail)
}



xy_str <- function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("x=", round(e$x, 2), " y=", round(e$y, 2), "\n")
}
xy_range_str <- function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("xmin=", round(e$xmin, 2), " xmax=", round(e$xmax, 2), 
         " ymin=", round(e$ymin, 2), " ymax=", round(e$ymax, 2),
         "\nArea= ",round((e$xmax-e$xmin)*(e$ymax-e$ymin),2))
}