load_vpalmr <- function() {
  
  # Test if Vpalmr is up-to-date compared to the Github version, and install it if not:
  remote= remotes:::github_remote(repo = "VEZY/Vpalmr", ref = "master", 
                                  auth_token = "71c9b59d68594c61acb7250813ef6098a381d4c4")
  
  package_name <- remotes:::remote_package_name(remote)
  local_sha <- remotes:::local_sha(package_name)
  test= 
    tryCatch(expr = {
      remote_sha <- remotes:::remote_sha(remote, local_sha)
      if (!isTRUE(force) && !remotes:::different_sha(remote_sha = remote_sha, 
                                                     local_sha = local_sha)) {
        message("Vpalmr is already up-to-date compared to the Github version,", 
                "the SHA1 (", substr(remote_sha, 1L, 8L), ") has not changed since last install.\n")
        0
      }else{
        remotes::install_github("VEZY/Vpalmr", auth_token = "71c9b59d68594c61acb7250813ef6098a381d4c4")
        1
      }
      
    }, error= function(cond){
      message("Could not fetch Vpalmr from Github.com at VEZY/Vpalmr. ",
              "Check your internet connection. ",
              "VPALM-IDE will use the current local version of Vpalmr. \n",
              "The original error is as follows:")
      message(cond)
      2
    }, finally = {
      require(Vpalmr)
    })
  
  return(test)
}


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
xy_length <- function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("Diagonal length=",
         round(sqrt((e$xmax-e$xmin)^2+(e$ymax-e$ymin)^2),3))
}


polygon_coord= function(data){
  data%>%
    mutate(group= paste0('x:',Col,", y:",Row))%>%
    group_by(group)%>%
    summarise(coords= 
                list(expand.grid(x= unique(c(xmin,xmax)),
                                 y= unique(c(ymin,ymax))))
    )%>%
    mutate(v_id= as.factor(1:n()))%>%
    tidyr::unnest()%>%  
    group_by(v_id)%>%
    mutate(pos= ifelse(x==min(x)&y==min(y),1,
                       ifelse(x==min(x)&y==max(y),2,
                              ifelse(x==max(x)&y==min(y),4,3))))%>%
    arrange(v_id,pos)
}
