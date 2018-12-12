#' Update progress
#' 
#' @description Callback function to update the Shiny progress bar
#'
#' @param value  The value to increment
#' @param detail A character string about the event
#' 
#' @details If `value` is NULL, the function will move the progress bar 1/21 
#' of the remaining distance. It is 1/21 because there are 21 calls of updateProgress
#' in [Vpalmr::mod_all()].
#' If non-NULL, it will set the progress to that value.
#'
#' @export
updateProgress= function(value = NULL, detail = NULL,progress_obj) {
  if (is.null(value)) {
    value <- progress_obj$getValue()
    value <- value + (progress_obj$getMax() - value) / 21
  }
  progress_obj$set(value = value, detail = detail)
}