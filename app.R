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
  # Ensuring that the background is always the same: 
  tags$head(tags$style(HTML("
    .shiny-text-output {
                            background-color:#fff;
                            }
                            "))),
  h1("Import data", span("And Compute Parameters", style = "font-weight: 300"), 
     style = "color: #fff; text-align: center;
              background-image: url('texturebg.png');
              padding: 20px"),
  br(),
  p("There are two main ways to get parameters for the construction of 3D palms:",
    tags$ul(
      tags$li("By loading a previous computation of the parameters that was made using this
              application or the",code("Vpalmr"),"package"), 
      tags$li("By loading the data files first, and then by computing the parameters in the
              app using",code("VPalmr::mod_all()"),". Loading the data can also be done in two ways:"),
      tags$ul(
        tags$li("From the default folder in the Shiny application (in 1-Data/Archi)"), 
        tags$li("Or by loading each file independently")
      )
    )
  ),
  
  br(),
  
  tags$ul(
    tags$li( 
      h2("Import parameters from previous session:"),
      p("If you already did the computation of the parameters for the map you",
        "need on the previous session, you can import the results instead of",
        " re-computing them."),
      em("Hint: by default, they are written in '2-Outputs/models.Rdata'")
    )),  
  
  fluidRow(  
    column(6,
           wellPanel(
             fileInput("params_previous",
                       label= "Parameters from a previous session "),
             actionButton("action", label = "Submit"),
             h4(textOutput("title_data_archi")),
             textOutput("data_archi"),
             h4(textOutput("title_data_models")),
             textOutput("data_models")
           )
    )
  ),
  tags$ul(
    tags$li( 
      h2("Load the data and compute the parameters"),
      p("If you need a new computation for the parameters, import the data",
        "and compute the new parameter values using",code("VPalmr::mod_all()"),".",
        "Two ways are provided to import the",
        "data: import them from the default location in the Shiny app (1-Data/Archi)",
        strong("or"),"load each file separately")
    )),
  fluidRow(
    column(6,
           wellPanel(  
             h2("Import parameters from local default file location"),
             p("The default file location is '1-Data/Archi' in the Shiny application."),
             dataTableOutput("data"),
             actionButton("load_data_default", label = "Load data")
           )
    )
  ),
  wellPanel(
    h2("Import data from files:"),
    textOutput("architecture", container = p),
    
    h3("Choose all the input files:"),
    
    fluidRow(
      column(2,fileInput("param_file", width = '50%',
                         label= "Parameter file (e.g. ParameterSimu):")),
      column(2,fileInput("param_dev", width = '50%',
                         label= "Development file (e.g. Development_Rep4_SMSE):")),
      column(2,fileInput("param_phylotaxy", width = '50%',
                         label= "Phylotaxy file (e.g. Stem_SMSE14):")),
      column(2,fileInput("param_dec", width = '50%',
                         label= "Declination and torsion file (e.g. AnglesC&A_SMSE_Nov14):")),
      column(2,fileInput("param_curv", width = '50%',
                         label= "Curvature file (e.g. LeafCurvature_SMSE14):")),
      # ),
      # fluidRow(
      column(2,fileInput("param_la", width = '50%',
                         label= "Leaf area file (e.g. LeafArea_monitoring_SMSE):")),
      column(2,fileInput("param_axial_angle", width = '50%',
                         label= "Leaflet axial angle file (e.g. LeafDispositionComp_SMSE14):")),
      column(2,fileInput("param_petiole_width", width = '50%',
                         label= "Petiole width file (e.g. Petiole_SMSE14):")),
      column(2,fileInput("param_twist", width = '50%',
                         label= "Leaf twist (torsion) file (e.g. Torsion_SMSE14):"))
    ),
    fluidRow(
      p("Import the files using",code("Vpalmr::import_data"),":"),
      actionButton(inputId = "submit_upload",label = "Submit"),
      br(),
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

