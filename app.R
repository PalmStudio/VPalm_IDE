pack= c('shiny', 'shinyFiles')
lapply(pack, function(x){
  if(!require(x, character.only = T)){
    install.packages(x)  
  }
  require(x, character.only = T)
})

if(!require(Vpalmr)){
  if(!require(remotes)){
    install.packages("remotes")  
  }
  remotes::install_github("VEZY/Vpalmr")
}
library(Vpalmr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Virtual Palm plants from field data"),
  
  hr(),
  
  sidebarLayout(
    mainPanel(  
      h2("Import parameters from previous session:"),
      p("If you already did the computation of the parameters for the map you",
        "need on the previous session, you can import the results instead of",
        " re-computing them"),
      em("Hint: by default, they are written in '2-Outputs/models.Rdata'"),
      tableOutput("contents")
    ),
    sidebarPanel(
      fileInput("params_previous",
                label= "Previous session parameters"),
      actionButton("action", label = "Submit"),
      h4(textOutput("title_data_archi")),
      textOutput("data_archi"),
      h4(textOutput("title_data_models")),
      textOutput("data_models")
    )
  ),
  sidebarLayout(
    mainPanel(  
      h2("Import parameters from local default file location (1-Data/Archi):"),
      dataTableOutput("data")
    ),
    sidebarPanel(
      actionButton("load_data_default", label = "Load data")
    )
  ),
  sidebarLayout(
    mainPanel(
      h2("Or compute parameters from new data or different age:"),
      textOutput("architecture"),
      
      h3("Import the input files:"),
      fileInput("param_file", width = '50%',
                label= "Parameter file (e.g. ParameterSimu.csv):"),
      fileInput("param_dev", width = '50%',
                label= "Development file (e.g. Development_Rep4_SMSE.csv):"),
      fileInput("param_phylotaxy", width = '50%',
                label= "Phylotaxy file (e.g. Stem_SMSE14.csv):"),
      fileInput("param_dec", width = '50%',
                label= "Declination and torsion file (e.g. AnglesC&A_SMSE_Nov14.csv):"),
      fileInput("param_curv", width = '50%',
                label= "Curvature file (e.g. LeafCurvature_SMSE14.csv):"),
      fileInput("param_la", width = '50%',
                label= "Leaf area file (e.g. LeafArea_monitoring_SMSE.csv):"),
      fileInput("param_axial_angle", width = '50%',
                label= "Leaflet axial angle file (e.g. LeafDispositionComp_SMSE14.csv):"),
      fileInput("param_petiole_width", width = '50%',
                label= "Petiole width file (e.g. Petiole_SMSE14.csv):"),
      fileInput("param_twist", width = '50%',
                label= "Leaf twist (torsion) file (e.g. Torsion_SMSE14.csv):"),
      p("Submit the files:"),
      actionButton(inputId = "submit_upload",label = "Submit")
    ),
    sidebarPanel(
      sliderInput("map",
                  "Desired palm age in months after planting:",
                  min = 1,
                  max = 40,
                  value = 30),
      actionButton("archi", "Update Architectural parameters"),
      downloadButton("downloadData", "Download")
    )
  )
  # sidebarLayout(
  #   sidebarPanel(
  #     numericInput("nleaves",
  #                  "Number of leaves for the virtual palm:",
  #                  value= 45, min= 1, step= 1)
  #   ),
  #   mainPanel(
  #     # plotOutput("distPlot")
  #   )
  # )
  
)

server <- function(input, output, session) {
  
  
  output$architecture <- renderText({
    paste("Month after planting=", input$map)
  })
  
  # Architecture parameter fit: ---------------------------------------------
  # Either read from file: 
  Palm_Param= 
    eventReactive(input$params_previous, {
      cat("Computing parameters for ", input$map, " months after planting\n")
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
    head(Palm_data_default()$Parameter)
  })
  
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
  
  
  
  
  # input$archi # observe here
  
  # Palm_Param= reactive({
  #   ifelse(!is.null(Palm_Param_read),Palm_Param_read(),Palm_Param_computed())  
  # })
  
  observeEvent(c( 
    # input$archi,
    input$params_previous
  ),{
    
    
    output$title_data_archi <- renderText({
      "Architectural data:"
    })
    output$data_archi <- renderText({
      paste(names(Palm_Param_read()$input), collapse= ', ')
    })
    
    output$title_data_models <- renderText({
      "Models:"
    })
    output$data_models <- renderText({
      paste(names(Palm_Param_read()$model), collapse= ', ')
    })
    
    output$contents <- renderTable({
      if(is.null(Palm_Param)){return(NULL)}
      # Palm_Param_read()[[1]][[1]]
    })
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

