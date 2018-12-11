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
                            .shiny-text-output {background-color:#fff;}"))),
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
  p("Load previous computation or make a new one ?"),
  selectInput("previous", "Choose here:", c(" ","Load previous computation",
                                            'Compute new parameters'), selected = " ",
              multiple = FALSE),
  
  conditionalPanel(
    condition = 'input.previous == "Load previous computation"',
    h2("Import parameters from previous session:"),
    p("If you already did the computation of the parameters for the map you",
      "need on the previous session, you can import the results instead of",
      " re-computing them."),
    em("Hint: they are often written as 'models.Rdata'"),
    fluidRow(  
      column(6,
             wellPanel(
               fileInput("params_previous",
                         label= "Parameters from a previous session "),
               h4(textOutput("title_data_archi")),
               textOutput("data_archi"),
               h4(textOutput("title_data_models")),
               textOutput("data_models")
             )
      )
    )
  ),  
  conditionalPanel(
    condition = 'input.previous == "Compute new parameters"',
    
    h2("Load the data and compute the parameters"),
    p("If you need a new computation for the parameters, choose a palm age, import the data",
      "and compute the new parameter values using",code("VPalmr::mod_all()"),"by following",
      "each following step:"),
    hr(),
    tags$ul(
      tags$li(
        h3("Please choose the desired palm age:"),
        sliderInput("map",
                    "Desired palm age in months after planting:",
                    min = 1,
                    max = 47,
                    value = 47)
      ),
      tags$li(
        hr(),
        h3("Load the data:"),
        p("Two ways are provided to import the",
          "data: import them from the default location in the Shiny app (left)",
          strong("or"),"load each file separately (right)."),
        selectInput("load", "Choose here:", c(" ","Load data from default folder",
                                              'Load each file separately'),
                    selected = " ",
                    multiple = FALSE),
        fluidRow(
          wellPanel(
            conditionalPanel(
              condition = 'input.load == "Load data from default folder"',
              
              h2("Load data from local default file location"),
              p("The default file location is '1-Data/Archi' in the Shiny application."),
              p(textOutput("architecture1", container = span)),
              br(),
              p("Import the files using",code("Vpalmr::import_data"),":"),
              actionButton("load_data_default", label = "Load data")
            ),
            conditionalPanel(
              condition = 'input.load == "Load each file separately"',
              h2("Load data from files:"),
              p(textOutput("architecture2", container = span)),
              
              h3("Choose each input files:"),
              
              fluidRow(
                column(4,p("Parameter file (e.g. ParameterSimu):"),
                       fileInput("param_file",NULL)),
                column(4,p("Development file (e.g. Development_Rep4_SMSE):"),
                       fileInput("param_dev", label= NULL)),
                column(4,p("Phylotaxy file (e.g. Stem_SMSE14):"),
                       fileInput("param_phylotaxy", label= NULL))
              ),
              fluidRow(
                column(4,p("Declination/torsion (e.g. AnglesC&A_SMSE_Nov14):"),
                       fileInput("param_dec", label= NULL)),
                column(4,p("Curvature file (e.g. LeafCurvature_SMSE14):"),
                       fileInput("param_curv", label= NULL)),
                column(4,p("Leaf area file (e.g. LeafArea_monitoring_SMSE):"),
                       fileInput("param_la", label= NULL))
              ),
              fluidRow(
                column(4,p("Leaflet axial angle (e.g. LeafDispositionComp):"),
                       fileInput("param_axial_angle", label= NULL)),
                column(4,p("Petiole width file (e.g. Petiole_SMSE14):"),
                       fileInput("param_petiole_width", label= NULL)),
                column(4,p("Leaf twist (torsion) file (e.g. Torsion_SMSE14):"),
                       fileInput("param_twist", label= NULL))
              ),
              fluidRow(
                p("Import the files using",code("Vpalmr::import_data"),":"),
                actionButton(inputId = "submit_upload",label = "Load data")
              )
            ),
            conditionalPanel(
              condition = 'output.data_trigger == "ok"',
              br(),
              p("Data successfully imported !")
            )
          )
        )
      ),
      conditionalPanel(
        condition = 'output.data_trigger == "ok"',
        # textOutput("data_trigger"),
        tags$li(
          h3("Glimpse of the imported data"),
          p("Here is a preview of the parameter file from the data for you to control:"),
          dataTableOutput("data")
        )
      ),
      tags$li(
        hr(),
        h3("Compute the parameters (can take some time)"),
        p("Now that you have imported the data files, compute the parameters",
          "by clicking the following button:"),
        actionButton("updatearchi", "Update Architectural parameters"),
        p(textOutput("modout"))
      )
    )
  ),
  conditionalPanel(
    condition = 'output.param_trigger == "ok"',
    h3("Download the parameters for VPalm"),
    p("Now that the parameters are fitted (or imported), you can download the results for",
      "further usage:"),
    downloadButton("downloadData", "Download")
  )
)

