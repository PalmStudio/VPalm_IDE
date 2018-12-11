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
      # cat("\nImporting data from '1-Data/Archi' for MAP ", input$map, " months after planting\n")
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
          },
          warning= function(out){
            message("warning(s) during Vpalmr::import_data execution")
            message("Original function warning:")
            message(out)
          })
      message("Data successfully imported")
      Inputs
    })
  
  
  Palm_data_import=
    eventReactive(input$submit_upload,
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
                      },
                      warning= function(out){
                        message("warning(s) during Vpalmr::import_data execution")
                        message("Original function warning:")
                        message(out)
                      })
                    
                  })
  
  # Palm_data= reactive(ifelse(is.null(Palm_data_import()),Palm_data_default(),Palm_data_import()))
  

  output$data <- renderDataTable({
    Palm_data_default()$Parameter
  }, options = list(pageLength = 5))
  
  mods= 
    eventReactive(input$archi, {
      updateSliderInput(session, "map", value = input$map, min = min(Palm_data()$input$Parameter$MAP),
                        max = max(Palm_data()$input$Parameter$MAP))
      cat("Computing parameters for ", input$map, " months after planting\n")
      if(!is.null(Palm_data())){
        # Fit the models on data:
        mod_all(x= Palm_data())
      }
    })
  
  output$downloadData= downloadHandler(
    filename = function() {
      paste0("models_MAP_",input$map,".csv")
    },
    content = function(file) {
      write_models(data = list(input= Palm_data(), model= mods()), path = file)
      message("Data and models were successfully written in: ", file)
    }
  )
  
  
  
  
  
  # Find the right Palm_Param_* according to the choices of the user  ---------
  # NB: either load previous or compute it
  
  Palm_Param= reactive({
    switch(input$previous,
           "Load previous computation" = Palm_Param_previous(),
           "Compute new parameters" = Palm_Param_computed())
  })
  
  
  # Return names of input and models from loaded model.Rdata for checking:
  observeEvent(c( 
    input$params_previous
  ),{
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
    
    output$contents <- renderTable({
      if(is.null(Palm_Param)){return(NULL)}
    })
    
  })
}

# Run the application 
# shinyApp(ui = ui, server = server)

