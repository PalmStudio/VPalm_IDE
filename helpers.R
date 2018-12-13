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