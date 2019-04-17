load_vpalmr <- function() {
  
  # Test if Vpalmr is up-to-date compared to the Github version, and install it if not:
  remote= remotes:::github_remote(repo = "VEZY/Vpalmr", ref = "master", 
                                  auth_token = "71c9b59d68594c61acb7250813ef6098a381d4c4")
  
  package_name <- remotes:::remote_package_name(remote)
  
  # Checking for dependencies:
  deps= remotes::dev_package_deps(pkgdir = file.path(.libPaths(),"Vpalmr")[1])
  # And unloading them if they need a re-install:
  pkg_to_install= deps$package[deps$diff!=0]
  if(length(pkg_to_install)>0){
    lapply(pkg_to_install, function(x){
      detach(paste0("package:",x), unload=TRUE)
    })
  }
    
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
        remotes::install_github("VEZY/Vpalmr", auth_token = "71c9b59d68594c61acb7250813ef6098a381d4c4",
                                upgrade = "always")
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
  
  # Reload detached packages
  if(length(pkg_to_install)>0){
    lapply(pkg_to_install, function(x){
      require(x)
    })
  }
  
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


default_params= function(){
  data.frame(
    Parameter= c(
      'nbLeafEmitted', 'rachisLengthRank1', 'rachisLengthSlope', 'Nbmax', 'nbSlope', 
      'nbInfl', 'B_length_Rank1', 'leafletLengthAtBSlope', 'B_width_Rank1',
      'bWidthSlope', 'xm_intercept', 'xm_slope', 'ym_intercept', 'ym_slope', 
      'petioleRachisRatio_M', 'nbFronds_M', 'decMaxA', 'decSlopeA', 'decInflA',
      'decliCintercept', 'decliCslope'),
    Description= c(
      'Total cumulative number of emitted leaves from planting',
      'Rachis length of the leaf at rank 1',
      'Slope of the rachis length equation',
      'Maximum number of leaflets per leaf',
      'Slope of the number of leaflets per leaf ~ rachis length',
      'Inflexion point of the number of leaflets per leaf ~ rachis length', 
      'Leaflet length at B point for the leaf at rank 1', 
      'Slope of the allometry between leaflet length at B point and rachis length', 
      'Leaflet width at B point for the leaf at rank 1', 
      'Slope of the leaflet width at B point ~ rachis length', 
      'Intercept of the relative position of maximum width on leaflet ~ position on rachis',
      'Slope of the relative position of maximum width on leaflet ~ position on rachis',
      'Intercept of leaflet shape ~ position on rachis', 
      'Slope of the leaflet shape ~ position on rachis',
      'Ratio of the petiole length to rachis length', 
      'Number of green leaves on the palm', 
      'Maximum declination angle (i.e leaf curvature) at point A', 
      'Slope at inflexion point of the declination angle (i.e leaf curvature) at A point ~ at C point', 
      'Inflexion point of the declination angle at A point ~ at C point', 
      'Intercept of the declination angle (i.e leaf curvature) at C point ~ leaf rank', 
      'Slope of the declination angle (i.e leaf curvature) at C point ~ leaf rank'
    ),
    Unit=
      c(
        'leaves',
        'cm',
        'cm leaves<sup>-1</sup>',
        'leaflets', 
        'leaflets cm<sup>-1</sup>',
        'cm',
        'cm',
        'cm cm<sup>-1</sup>',
        'cm',
        'cm cm<sup>-1</sup>',
        '-',
        '-',
        '-',
        '-',
        'cm cm<sup>-1</sup>',
        'leaves',
        'degrees',
        'degrees degrees<sup>-1</sup>',
        'degrees',
        'degrees',
        'degrees rank<sup>-1</sup>'
      )
  )
}


make_scene_custom= function(x, path, AMAPStudio, planting_design= NULL, 
                            plant_dist= 9.2, name= NULL, progress= NULL){
  # As make_scene but for already-formated VPalm outputs for one tree only, comming
  # from user-input. 
  if(is.null(name)){
    name= "custom"
  }
  VPalm_in = format_tree(data = x)
  up_progress(progress, "format_tree")
  params = write_tree(data = VPalm_in, path = file.path(path,"VPalm_inputs"),
                      name= name, verbose = F, overwrite = TRUE)
  up_progress(progress, "write_tree")
  if(params) {
    message("VPalm parameters file was successfully written in: ", 
            file.path(path, "1-VPalm_inputs"))
  }else{
    stop("Error during VPalm parameter file writing")
  }
  
  MAP= VPalm_in$value[grep("Modelled Months After Planting", VPalm_in$name)]
  OPFs = make_opf(parameter = file.path(path, "VPalm_inputs", paste0(name,"_MAP_",MAP,".txt")), 
                  opf = file.path(path, "scenes","opf",paste0(name,"_Average_MAP_",MAP,".opf")),
                  AMAPStudio = AMAPStudio, overwrite = TRUE)
  up_progress(progress, "make_opf")
  
  if(is.null(planting_design)){
    planting_design = design_plot(rows = 1, cols = 1, x0 = 0, 
                                  x_dist = plant_dist)$design
  }
  up_progress(progress, "design_plot")
  
  format_ops(design = planting_design, Progeny = name, map = MAP,
             average = TRUE) %>% 
    write_ops(file.path(path, "scenes",paste0(name, "_MAP_",MAP, ".ops")), 
              overwrite = TRUE)
  up_progress(progress, "make_ops_all")
  
  list(plot_design= planting_design)
}
