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
                          function(x)updateProgress(detail = x, progress_obj = progress_obj))
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
                c("All progenies",unique(Palm_Param()$input$Parameter$Progeny)))
  })
  
  output$scenepar=
    reactive({
      paste(c(names(Palm_Param()), ifelse(is.na(input$nbtrees),0,input$nbtrees),
              input$nleaves,
              input$prog,input$plant_dist,ifelse(is.null(input$planting_design),"NULL","other")))
    })
  
  
  observeEvent(
    input$makescene,
    {
      Vpalmr::make_scene(data = Palm_Param(),
                         nleaves = input$nleaves,
                         path = file.path(getwd(), "3-Outputs"), 
                         Progeny = ifelse(input$prog=="All progenies",NULL,input$prog),
                         AMAPStudio = file.path(getwd(), "2-VPalm_exe"),
                         ntrees = ifelse(is.na(input$nbtrees),0,input$nbtrees),
                         plant_dist = input$plant_dist)
      message("Vpalmr::extract_progeny() ran successfully")
    })
  
  
}

# Run the application 
# shinyApp(ui = ui, server = server)

