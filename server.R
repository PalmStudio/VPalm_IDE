server <- function(input, output, session) {
  
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
  
  output$data=
    renderDataTable({
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
  
  # Find the right Palm_Param_* according to the choices of the user  ---------
  # NB: either load previous or compute it
  
  Palm_Param= reactive({
    switch(input$previous,
           "Load previous computation" = Palm_Param_previous(),
           "Compute new parameters" = Palm_Param_computed())
  })
  
  observeEvent(Palm_Param(), {
    showNotification("Parameters successfully computed")
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
      ifelse(length(Palm_Param())==2,'ok','notok')
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
    Progs= unique(Palm_Param()$input$Parameter$Progeny)
    Progs_choices= 
      if(length(Progs)>1){
        c("All progenies","Average progeny",Progs)  
      }else{
        Progs  
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
      Vpalmr::design_plot(rows = 2, cols = 1, x_dist = 9.2)$design%>%
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
        if(!is.null(Palm_Param())){
          scene= 
            Vpalmr::make_scene(data = Palm_Param(),
                               nleaves = input$nleaves,
                               path = 
                                 if(length(parseDirPath(volumes, dir())>0)){
                                   parseDirPath(volumes, dir())
                                 }else{
                                   parseDirPath(volumes, dir())
                                 }, 
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
      ggplot(aes(x= x, y= y))+
      coord_fixed()+
      geom_point()+
      geom_image(aes(image= image), size= input$palm_size)+
      geom_point(aes(color= "Palm tree center"))+
      geom_polygon(data= polygon_coord(plot_design), 
                   aes(x= x, y= y, fill= "Plot limits", 
                       color= "Plot limits"), alpha= 0.2)+
      ylim(low= ranges[1], high= ranges[2])+
      xlim(low= ranges[1], high= ranges[2])+
      labs(colour = "", fill= "")+
      theme(legend.position="bottom")
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
      ggplot(aes(x= .data$x, y= .data$y))+
      geom_image(aes(image= image), size= input$palm_size/3)+
      geom_point(aes(color= "Palm tree center"))+
      ylim(low= ranges_full[1], high= ranges_full[2])+
      xlim(low= ranges_full[1], high= ranges_full[2])+
      theme(legend.position="bottom")+
      labs(fill= "Voronoï index", x= "x coordinate (m)", y= "y coordinate (m)")+
      geom_polygon(data= voronoi_stands, aes(x= x, y= y, fill= v_id, color= v_id), alpha= 0.2)+
      guides(color= FALSE)+
      coord_fixed()
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

