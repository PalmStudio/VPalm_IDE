server <- function(input, output, session) {
  
  isloaded= try(load_vpalmr())
  
  output$loading= renderText({
    if(isloaded==0){
      "Vpalmr is up-to-date"
    }else if(isloaded==1){
      "Vpalmr was successfully updated"
    }else if(isloaded==2|inherits(isloaded,"try-error")){
      paste("Could not install Vpalmr (could not  fetch from Github.com at VEZY/Vpalmr., or could not install).",
            "VPALM-IDE will use the current local version of Vpalmr. \n")
    }
  })
  
  shinyjs::toggle("loading_page")
  shinyjs::toggle("loaded")
  
  map= reactive({
    paste("Month after planting=", input$map)
  })
  
  output$architecture1= renderText({map()})
  output$architecture2= renderText({map()})
  
  # Architecture parameter fit: ---------------------------------------------
  # Either read from file: 
  Palm_Param_previous= 
    eventReactive(input$params_previous, {
      message("Load previous parameters fit\n")
      readRDS(input$params_previous$datapath)
    })
  
  
  # Or read the inputs local folder or files:
  
  # Find the right Palm_data_* according to the choices of the user  ---------------
  # NB: choices are either loading from default folder, loading from user-chosen folder,
  # or by uploading each files separately.
  
  Palm_data= reactive({
    switch(input$load,
           "Load data from default folder" = Palm_data_folder(),
           "Load data from folder" = Palm_data_folder(),
           "Load each file separately" = Palm_data_files())
  }) 
  
  volumes_data= c(Home = fs::path_home(), getVolumes()(), WD= getwd(),
                  "R Installation" = R.home())
  shinyDirChoose(input, "dirdata", roots = volumes_data, session = session,
                 restrictions = system.file(package = "base"))
  dir_data= reactive(input$dirdata)
  
  # Files in the data folder:
  output$files_data= renderPrint(list.files(parseDirPath(volumes_data,dir_data())))
  output$dir_data= renderPrint({
    parseDirPath(volumes_data, dir_data())
  })
  
  # Display info about the user-defined path to the data:
  observeEvent(input$dirdata, {
    output$data_path_info1= renderText({
      paste("Here is the folder you chose:",
            parseDirPath(volumes_data, dir_data()))
    })
    output$data_path_info2= renderText({
      "Here is a list of the data in the folder:"
    })
    output$data_path_info3= renderPrint({
      list.files(parseDirPath(volumes_data,dir_data()))
    })
  })
  
  # If import from folder, either use the default path, or one specified by the user:
  folder_path=
    reactive({
      if(input$load == "Load data from default folder"){
        file.path(getwd(), "1-Data/Archi")  
      }else{
        parseDirPath(volumes_data, dir_data())
      }
    }) 
  
  Palm_data_folder=
    eventReactive(input$load_data_folder, {
      dir_path= folder_path()
      data_path=
        list(
          parameter= file.path(dir_path,'ParameterSimu.csv'),
          development= file.path(dir_path,'Development_Rep4_SMSE.csv'),
          phylotaxy= file.path(dir_path,'Stem_SMSE14.csv'),
          declination= file.path(dir_path,'AnglesC&A_SMSE_Nov14.csv'),
          curvature= file.path(dir_path,'LeafCurvature_SMSE14.csv'),
          leaf_area= file.path(dir_path,'LeafArea_monitoring_SMSE.csv'),
          axial_angle= file.path(dir_path,'LeafDispositionComp_SMSE14.csv'),
          petiole_width= file.path(dir_path,'Petiole_SMSE14.csv'),
          twist= file.path(dir_path,'Torsion_SMSE14.csv')
        )
      map_num= as.numeric(input$map)
      if(!is.na(map_num)){
        data_path$map= map_num
      }else{
        return('Error')
      }
      Inputs=
        tryCatch(
          do.call(Vpalmr::import_data, data_path),
          error = function(out){
            return('Error')
          })
      if(all(Inputs!="Error")){
        message("Data successfully imported")
      }
      Inputs
    })
  
  # Or read the inputs from user-inputed files:
  Palm_data_files=
    eventReactive(
      input$submit_upload,
      {
        cat("\nImporting data for ", input$map, " months after planting\n")
        tryCatch(
          do.call(Vpalmr::import_data, list(parameter= input$param_file$datapath,
                                            development= input$param_dev$datapath,
                                            phylotaxy= input$param_phylotaxy$datapath,
                                            declination= input$param_dec$datapath,
                                            curvature= input$param_curv$datapath,
                                            leaf_area= input$param_la$datapath,
                                            axial_angle= input$param_axial_angle$datapath,
                                            petiole_width= input$param_petiole_width$datapath,
                                            twist= input$param_twist$datapath, map= input$map)),
          error = function(out){
            message("Error during Vpalmr::import_data execution")
            message("Original function error:")
            message(out)
          })
      })
  
  # trigger the display of the Parameter data.frame from the data just read (for control):
  output$data_trigger= 
    renderText({
      if(isTruthy(Palm_data())){
        if(all(Palm_data()=="Error")){
          showNotification("Given MAP does not exist in the data")
          'notok'
        }else if(is.data.frame(Palm_data()$Parameter)&&
                 nrow(Palm_data()$Area)>0){
          showNotification("Data successfully imported")
          'ok'
        }else{
          showNotification("Given MAP does not yield enough data")
          'notok'  
        }
      }else{
        'notyet'
      }
    })
  
  
  outputOptions(output, "data_trigger", suspendWhenHidden = FALSE)  
  
  output$progeny_filt = renderUI({
    checkboxGroupInput(inputId = 'prog_filt', label = 'Filter progenies:', 
                       choices = unique(Palm_data()$Parameter$Progeny),
                       selected= unique(Palm_data()$Parameter$Progeny))
  })
  
  # Filter the data according to prog_filt (output$progeny_filt):
  Palm_data_filt= reactive({
    lapply(Palm_data(), function(x){
      if(is.data.frame(x)){
        x[x$Progeny%in%input$prog_filt,]
      }else{
        x
      }
    })
  }) 
  
  output$datashow=
    DT::renderDataTable({
      Palm_data_filt()$Parameter
    }, options = list(pageLength = 5))
  
  output$data_filt_trigger= 
    renderText({
      if(!is.null(Palm_data_filt())){
        is_data= 
          lapply(Palm_data_filt()[-grep("MAP_requested",names(Palm_data_filt()))],nrow)%>%
          unlist
      }else{
        is_data=1
      }
      if(any(is_data<1)){
        nodata= names(is_data[is_data<1])
        # showNotification(paste("Missing data for selected Progenies"))
        # "notok"
        paste(nodata, collapse= ", ")
      }else{
        "ok"
      }
    })
  outputOptions(output, "data_filt_trigger", suspendWhenHidden = FALSE)
  
  
  mods= 
    eventReactive(input$updatearchi, {
      # updateSliderInput(session, "map", value = input$map,
      #                   min = min(Palm_data_filt()$Parameter$MAP),
      #                   max = max(Palm_data_filt()$Parameter$MAP))
      if(!is.null(Palm_data_filt()$Parameter)){
        # Create a Progress object
        progress_obj <- shiny::Progress$new()
        progress_obj$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress_obj$close())
        
        # Fit the models on data:
        models= mod_all(x= Palm_data_filt(), progress = 
                          function(x){
                            updateProgress(detail = x, progress_obj = progress_obj,
                                           steps= 21)})
      }else{
        models= NULL
      }
      models
    })
  
  output$modout=
    renderText({
      is(mods()) # trigger when mods is created
      paste("Architectural parameters successfully computed !")
    })
  
  Palm_Param_computed= reactive({
    list(input= isolate(Palm_data_filt()), model= mods())
  })
  
  
  output$parameters_description= 
    renderDataTable({default_params()},escape = FALSE)
  
  # Reading the parameters template:
  template= reactive(readRDS("1-Data/VPalm_list/vpalm_template.rds"))
  
  Palm_Param_custom= 
    reactive({
      param_list= template()
      # param_list$nbFronds_M= input$nbleaves_custom
      param_list$MAP_requested= input$MAP_custom
      param_list$nbLeafEmitted= round(input$MAP_custom*2.5)
      param_list$rachisLengthIntercept= 
        input$rachisLengthRank1 - input$rachisLengthSlope * param_list$nbLeafEmitted
      param_list$rachisLengthSlope= input$rachisLengthSlope
      param_list$nbMax= input$nbMax
      param_list$nbSlope= input$nbSlope
      param_list$nbInfl= input$nbInfl
      param_list$lenfletLengthAtBIntercept= 
        input$B_length_Rank1 - input$rachisLengthRank1 * input$leafletLengthAtBSlope
      param_list$leafletLengthAtBSlope= input$leafletLengthAtBSlope
      param_list$bWidthIntercept=
        input$B_width_Rank1 - input$rachisLengthRank1 * input$bWidthSlope
      param_list$bWidthSlope= input$bWidthSlope
      param_list$xm_intercept= input$xm_intercept
      param_list$xm_slope= input$xm_slope
      param_list$ym_intercept= input$ym_intercept
      param_list$ym_slope= input$ym_slope
      param_list$petioleRachisRatio_M= input$petioleRachisRatio_M
      param_list$decMaxA= input$decMaxA
      param_list$decSlopeA= input$decSlopeA
      param_list$decInflA= input$decInflA
      param_list$decliCintercept= input$decliCintercept
      param_list$decliCslope= input$decliCslope
      
      list(custom= param_list)
    })
  
  # Find the right Palm_Param_* according to the choices of the user  ---------
  # NB: either load previous, compute it or user-defined values
  
  Palm_Param= reactive({
    switch(input$previous,
           "Load previous computation" = Palm_Param_previous(),
           "Compute new parameters" = Palm_Param_computed(),
           "User-defined parameters (exploration)" = Palm_Param_custom())
  })
  
  observeEvent(Palm_Param_computed(), {
    showNotification("Parameters successfully computed")
  })
  
  observeEvent(Palm_Param_previous(), {
    showNotification("Parameters successfully loaded")
  })
  
  # Return names of input and models from loaded model.Rdata for checking:
  observeEvent(input$params_previous,{
    output$title_data_archi <- renderText({
      "Architectural data:"
    })
    output$data_archi <- renderText({
      paste(names(Palm_Param()$input), collapse= ', ')
    })
    
    output$title_data_models <- renderText({
      "Models:"
    })
    output$data_models <- renderText({
      paste(names(Palm_Param()$model), collapse= ', ')
    })
    
  })
  
  # trigger the display of the download button when parameters are available:
  output$param_trigger= 
    renderText({
      ifelse(length(Palm_Param())==2|input$save_vpalm>0,'ok','notok')
    })
  outputOptions(output, "param_trigger", suspendWhenHidden = FALSE)  
  
  # Make the data downloadable:
  output$downloadData= downloadHandler(
    filename = function() {
      paste0("models_MAP_",input$map,"_Prog_",
             paste(unique(Palm_Param()$input$Parameter$Progeny), collapse = "_"),
             ".RData")
    },
    content = function(file) {
      saveRDS(Palm_Param(), file)
    }
  )
  
  # Page 2: Call VPalm and build OPF/OPS files ------------------------------
  
  output$progeny = renderUI({
    
    if(length(Palm_Param())==2){
      Progs= unique(Palm_Param()$input$Parameter$Progeny)  
    }else if(input$save_vpalm>0){
      Progs= "custom"
    }
    
    Progs_choices= 
      if(length(Progs)>1){
        c("All progenies","Average progeny",Progs)  
      }else{
        list(Progs)  
      }
    selectInput(inputId = 'prog', 'Progeny', Progs_choices)
  })
  
  # directory where to write the outputs:
  volumes= c(Home = fs::path_home(), getVolumes()(), WD= getwd(), "R Installation" = R.home())
  
  shinyDirChoose(input, "dir", roots = volumes, session = session, restrictions = system.file(package = "base"))
  dir= reactive(input$dir)
  
  output$dir= renderPrint({
    parseDirPath(volumes, input$dir)
  })
  
  output$dirtrigger= renderText({
    if(isTruthy(dir())){"Here is the folder you chose for the outputs:"}else{""}
  })
  
  output$dirtrigger2= 
    renderText({
      if(isTruthy(dir())){"Now you can set the parameters:"}else{""}
    })
  
  # Make the output paths available for ui for display as information:
  output$dir_vpalm_inputs= renderText({
    paste("Vpalm input files written in:",
          file.path(parseDirPath(volumes, input$dir),"VPalm_inputs"))
  })
  output$dir_scenes= renderText({
    paste("Vpalm output files (OPS and OPF files) written in:",
          file.path(parseDirPath(volumes, input$dir),"scenes"))
  })
  
  # Update the initial (default) seed value according to the number of trees.
  
  observeEvent(input$nbtrees, {
    updateTextInput(session, "seed", 
                    value = 
                      if(input$nbtrees==0|!isTruthy(input$nbtrees)){
                        NULL
                      }else{
                        paste(1:input$nbtrees, collapse = ",")
                      }
    )
  })
  
  seeds= reactive(
    if(isTruthy(input$seed)){
      as.numeric(unlist(strsplit(input$seed,",")))  
    }else{
      NULL
    }
  )
  
  
  output$design_ex=
    renderTable(
      Vpalmr::design_plot(rows = 2, cols = 1, x_dist = 9.21)$design%>%
        dplyr::select(.data$x,.data$y,.data$z,.data$xmin, .data$ymin,
                      .data$xmax, .data$ymax,.data$scale,
                      .data$inclinationAzimut,.data$inclinationAngle,
                      .data$stemTwist)
    )
  
  custom_design= reactive(
    if(isTruthy(input$planting_design)){
      data.table::fread(file = input$planting_design$datapath, data.table = FALSE)
    }else{
      NULL
    }
  )
  
  scenes= 
    eventReactive(
      input$makescene,
      {
        progress_obj <- shiny::Progress$new()
        progress_obj$set(message = "Making scene:", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress_obj$close())
        if(length(Palm_Param())==2){
          scene= 
            Vpalmr::make_scene(data = Palm_Param(),
                               nleaves = input$nleaves,
                               path = parseDirPath(volumes, dir()), 
                               Progeny = 
                                 if(input$prog=="All progenies"){
                                   NULL
                                 }else if(input$prog=="Average progeny"){
                                   "Average"
                                 }else{
                                   input$prog
                                 },
                               AMAPStudio = getwd(),
                               ntrees = ifelse(is.na(input$nbtrees),0,input$nbtrees),
                               plant_dist = input$plant_dist,
                               plot_design= 
                                 if(!isTruthy(input$planting_design)){
                                   NULL
                                 }else{
                                   custom_design()
                                 },
                               seed= if(is.na(input$nbtrees)){NULL}else{seeds()},
                               progress = function(x){
                                 updateProgress(detail = x, progress_obj = progress_obj,
                                                steps = 7) 
                               })
          message("Vpalmr::extract_progeny() ran successfully")
        }else if(input$save_vpalm>0){
          scene= 
            make_scene_custom(x = Palm_Param()$custom, 
                              path = parseDirPath(volumes, dir()),
                              AMAPStudio = getwd(),
                              planting_design= 
                                if(!isTruthy(input$planting_design)){
                                  NULL
                                }else{
                                  custom_design()
                                },  
                              plant_dist= input$plant_dist,
                              name= if(isTruthy(input$custom_name)){
                                input$custom_name  
                              }else{
                                NULL
                              },
                              progress=function(x){
                                updateProgress(detail = x, progress_obj = progress_obj,
                                               steps = 7) 
                              })
        }else{
          scene= NULL
        }
        scene
      })
  
  # trigger the display of the paths where the outputs were written:
  output$scene_trigger= 
    renderText({
      ifelse(!is.null(scenes()),'ok','notok')
    })
  outputOptions(output, "scene_trigger", suspendWhenHidden = FALSE) 
  
  
  observeEvent(scenes(), {
    showNotification("Scene successfully created and written")
  })
  
  output$plot_design <- renderPlot({
    plot_design= scenes()$plot_design
    ranges= range(plot_design$xmax,plot_design$ymax,plot_design$xmin,plot_design$ymin)
    
    plot_design%>%
      mutate(image= 'www/palm_tiny.png')%>%
      ggplot2::ggplot(ggplot2::aes(x= x, y= y))+
      ggplot2::coord_fixed()+
      ggplot2::geom_point()+
      ggimage::geom_image(ggplot2::aes(image= image), size= input$palm_size)+
      ggplot2::geom_point(ggplot2::aes(color= "Palm tree center"))+
      ggplot2::geom_polygon(data= polygon_coord(plot_design), 
                            ggplot2::aes(x= x, y= y, fill= "Plot limits", 
                                         color= "Plot limits"), alpha= 0.2)+
      ggplot2::ylim(low= ranges[1], high= ranges[2])+
      ggplot2::xlim(low= ranges[1], high= ranges[2])+
      ggplot2::labs(colour = "", fill= "")+
      ggplot2::theme(legend.position="bottom")
  })
  
  output$plot_info <- renderText({
    paste0(
      "Value on click: ", xy_str(input$plot_click),
      "Value on hover: ", xy_str(input$plot_hover),
      "Diagonal length: ", xy_length(input$plot_brush),
      "Selection: ", xy_range_str(input$plot_brush)
    )
  })
  
  
  output$plot_design_rep <- renderPlot({
    plot_design= scenes()$plot_design
    
    # Matrix of the design (each cell is a Voronoi):
    mat_plot= expand.grid(Row= 1:input$voronois, 
                          Col= 1:input$voronois)
    
    # Full design:
    design=
      mapply(function(Row,Col){
        plot_design%>%
          select(x,y,xmax,ymax,xmin,ymin)%>%
          mutate(xmin= xmax*(Col-1), ymin= ymax*(Row-1),
                 x= x+xmin, y= y+ymin,
                 xmax= xmax*Col, ymax= ymax*Row,
                 Col= Col, Row= Row)
      }, Row= mat_plot$Row, Col= mat_plot$Col)%>%t()%>%as_tibble()%>%
      tidyr::unnest()
    
    ranges_full= range(design$xmax,design$ymax,design$xmin,design$ymin)
    
    voronoi_stands=
      design%>%
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
    
    design%>%
      mutate(image= 'www/palm_tiny.png')%>%
      ggplot2::ggplot(ggplot2::aes(x= .data$x, y= .data$y))+
      ggimage::geom_image(ggplot2::aes(image= image), size= input$palm_size/3)+
      ggplot2::geom_point(ggplot2::aes(color= "Palm tree center"))+
      ggplot2::ylim(low= ranges_full[1], high= ranges_full[2])+
      ggplot2::xlim(low= ranges_full[1], high= ranges_full[2])+
      ggplot2::theme(legend.position="bottom")+
      ggplot2::labs(fill= "Voronoï index", x= "x coordinate (m)", y= "y coordinate (m)")+
      ggplot2::geom_polygon(data= voronoi_stands, 
                            ggplot2::aes(x= x, y= y, fill= v_id, color= v_id), alpha= 0.2)+
      ggplot2::guides(color= FALSE)+
      ggplot2::coord_fixed()
  })
  
  
  output$plot_info_rep <- renderText({
    paste0(
      "Value on click: ", xy_str(input$plot_click_rep),
      "Value on hover: ", xy_str(input$plot_hover_rep),
      "Diagonal length: ", xy_length(input$plot_brush),
      "Selection: ", xy_range_str(input$plot_brush_rep)
    )
  })
}

# Run the application 
# shinyApp(ui = ui, server = server)

