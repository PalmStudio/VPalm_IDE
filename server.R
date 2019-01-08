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
  
  
  # Or read the inputs from local folder:
  Palm_data_default=
    eventReactive(input$load_data_default, {
      data_path= file.path(getwd(), "1-Data/Archi")
      data_path=
        list(
          parameter= file.path(data_path,'ParameterSimu.csv'),
          development= file.path(data_path,'Development_Rep4_SMSE.csv'),
          phylotaxy= file.path(data_path,'Stem_SMSE14.csv'),
          declination= file.path(data_path,'AnglesC&A_SMSE_Nov14.csv'),
          curvature= file.path(data_path,'LeafCurvature_SMSE14.csv'),
          leaf_area= file.path(data_path,'LeafArea_monitoring_SMSE.csv'),
          axial_angle= file.path(data_path,'LeafDispositionComp_SMSE14.csv'),
          petiole_width= file.path(data_path,'Petiole_SMSE14.csv'),
          twist= file.path(data_path,'Torsion_SMSE14.csv')
        )
      data_path$map= input$map
      Inputs=
        tryCatch(
          do.call(Vpalmr::import_data, data_path),                 
          error = function(out){
            message("Error during Vpalmr::import_data execution")
            message("Original function error:")
            message(out)
          })
      message("Data successfully imported")
      Inputs
    })
  
  
  Palm_data_import=
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
  
  # Find the right Palm_data_* according to the choices of the user  ---------------
  # NB: choices are either loading from default folder or by uploading custom files
  Palm_data= reactive({
    switch(input$load,
           "Load data from default folder" = Palm_data_default(),
           "Load each file separately" = Palm_data_import())
  }) 
  
  observeEvent(Palm_data(), {
    showNotification("Data successfully imported")
  })
  
  # trigger the display of the Parameter data.frame from the data just read (for control):
  output$data_trigger= 
    renderText({
      ifelse(is.data.frame(Palm_data()$Parameter),'ok','notok')
    })
  outputOptions(output, "data_trigger", suspendWhenHidden = FALSE)  
  
  output$data=
    renderDataTable({
      Palm_data()$Parameter
    }, options = list(pageLength = 5))
  
  mods= 
    eventReactive(input$updatearchi, {
      # updateSliderInput(session, "map", value = input$map, min = min(Palm_data()$Parameter$MAP),
      #                   max = max(Palm_data()$Parameter$MAP))
      if(!is.null(Palm_data()$Parameter)){
        # Create a Progress object
        progress_obj <- shiny::Progress$new()
        progress_obj$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress_obj$close())
        
        # Fit the models on data:
        models= mod_all(x= Palm_data(), progress = 
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
    list(input= isolate(Palm_data()), model= mods())
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
      paste0("models_MAP_",input$map,".RData")
      # "model.RData"
    },
    content = function(file) {
      saveRDS(Palm_Param(), file)
      # message("Data and models were successfully written in: ", file)
    }
  )
  
  
  
  # Page 2: Call VPalm and build OPF/OPS files ------------------------------
  
  output$progeny = renderUI({
    selectInput(inputId = 'prog', 'Progeny', 
                c("All progenies","Average progeny",unique(Palm_Param()$input$Parameter$Progeny)))
  })
  
  output$scenepar=
    reactive({
      paste(c(names(Palm_Param()), ifelse(is.na(input$nbtrees),0,input$nbtrees),
              input$nleaves,
              input$prog,input$plant_dist,ifelse(is.null(input$planting_design),"NULL","other")))
    })
  
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
                               path = file.path(getwd(), "3-Outputs"), 
                               Progeny = 
                                 if(input$prog=="All progenies"){
                                   NULL
                                 }else if(input$prog=="Average progeny"){
                                   "Average"
                                 }else{
                                   input$prog
                                 },
                               AMAPStudio = file.path(getwd(), "2-VPalm_exe"),
                               ntrees = ifelse(is.na(input$nbtrees),0,input$nbtrees),
                               plant_dist = input$plant_dist,
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
  
  observeEvent(scenes(), {
    showNotification("Scene successfully created and written")
  })
  
  
  output$plot_design <- renderPlot({
    plot_design= scenes()$plot_design
    plot_design%>%
      mutate(image= 'www/palm_tiny.png')%>%
      ggplot(aes(x= x, y= y))+
      geom_image(aes(image= image), size= input$palm_size)+
      geom_point(aes(color= "Palm tree center"))+
      ylim(low= unique(plot_design$ymin),
           high= unique(plot_design$ymax))+
      xlim(low= unique(plot_design$xmin),
           high= unique(plot_design$xmax))+
      labs(colour = "")+
      theme(legend.position="bottom")
  })
  
  output$plot_info <- renderText({
    paste0(
      "Value on click: ", xy_str(input$plot_click),
      "Value on hover: ", xy_str(input$plot_hover),
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
      ylim(low= min(unique(design$ymin)),
           high= max(unique(design$ymax)))+
      xlim(low= min(unique(design$xmin)),
           high= max(unique(design$xmax)))+
      theme(legend.position="bottom")+
      labs(fill= "Voronoï index", x= "x coordinate (m)", y= "y coordinate (m)")+
      geom_polygon(data= voronoi_stands, aes(x= x, y= y, fill= v_id, color= v_id), alpha= 0.2)+
      guides(color= FALSE)
  })
  
  
  output$plot_info_rep <- renderText({
    paste0(
      "Value on click: ", xy_str(input$plot_click_rep),
      "Value on hover: ", xy_str(input$plot_hover_rep),
      "Selection: ", xy_range_str(input$plot_brush_rep)
    )
  })
  
}

# Run the application 
# shinyApp(ui = ui, server = server)

