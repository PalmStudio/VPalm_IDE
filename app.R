library(shiny)
library(Vpalmr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Virtual Palm plants from field data"),
  
  hr(),
 
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    mainPanel(  
      h2("Import parameters from previous session:"),
      p("If you already did the computation of the parameters for the map you",
         "need on the previous session, you can import the results instead of",
         " re-computing them"),
      em("Hint: by default, they are written in '2-Outputs/models.Rdata'")
    ),
    sidebarPanel(
      fileInput("params_previous",
                label= "Previous session parameters"),
      actionButton("action", label = "Submit")
    )
  ),
  sidebarLayout(
    mainPanel(  
      h2("Or compute parameters from new data or different age:"),
      textOutput("architecture")
    ),
    sidebarPanel(
      sliderInput("map",
                  "Desired palm age in months after planting:",
                  min = 1,
                  max = 45,
                  value = 30),
      actionButton("archi",
                   "Update Architectural parameters")
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  # updateSliderInput()
  # output$architecture <- renderText({ 
  #   paste("Month after planting=", input$map)
  # })
  output$architecture <- 
    eventReactive(input$archi, {
      cat("Computing parameters for ", input$map, " months after planting\n")
      tryCatch(Vpalmr::compute_archi(map = input$map,
                                     data_path = "1-Data/Archi",
                                     write_path = "2-Outputs"),
               error = function(out){
                 message("Error during Vpalmr::compute_archi execution")
                 message("Original function error:")
                 message(out)
               })
      # session$sendCustomMessage(type = 'testmessage',
      #                           message = 'Architectural parameters successfully computed')
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

