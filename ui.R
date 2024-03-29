# Mandatory to load the packages per session because VPalmr installation
# can potentially have to re-install them, so we need them to not be loaded before
pack= c('shiny','ggplot2','ggimage','dplyr','shinyFiles','shinyjs','magrittr',"DT")
lapply(pack, function(x){
  if(!require(x, character.only = T)){
    install.packages(x, repos= "https://cloud.r-project.org",)
    require(x, character.only = T)
  }
})
isloaded= try(load_vpalmr())

source("helpers.R")

# Define UI for application that draws a histogram
ui <- navbarPage(
  "VPALM-IDE",
  # Page 1: Importing / Computing VPalm parameter files ---------------------
  tabPanel("Initialize",
           shinyjs::useShinyjs(),
           div(
             id = "loading_page",
             h1("Downloading Vpalmr package (this may take a while)")
           ),
           shinyjs::hidden(
             div(
               id = "loaded",
               h2("All packages were imported, please proceed to next page."),
               textOutput("loading")
             )
           )
  ),
  tabPanel("Importing / computing VPalm parameters",

           # Application title
           # titlePanel("Virtual Palm plants from field data"),
           # hr(),
           # Ensuring that the background is always the same:
           tags$head(tags$style(HTML("
                            .shiny-text-output {background-color:#fff;}"))),
           tags$head(HTML('<link rel="shortcut icon" href="favicon.png">
                          <link rel="icon" type="image/png" sizes="16x16" href="favicon.png">
                          <link rel="icon" type="image/png" sizes="32x32" href="favicon.png">')),
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
                                app using",code("VPalmr::mod_all()"),
                       ". Loading the data can also be done in two ways:"),
               tags$ul(
                 tags$li("From the default folder in the Shiny application (in 0-data/Archi)"),
                 tags$li("Or by loading each file independently")
               )
             )
           ),

           br(),
           p("Load previous computation or make a new one ?"),
           selectInput("previous", "Choose here:",
                       c(" ","Load previous computation",
                         'Compute new parameters',
                         "User-defined parameters (exploration)"), selected = " ",
                       multiple = FALSE),

           conditionalPanel(
             condition = 'input.previous == "Load previous computation"',
             h2("Import parameters from previous session:"),
             p("If you already did the computation of the parameters for the map you",
               "need on the previous session, you can import the results instead of",
               " re-computing them."),
             em("Hint: they are written as 'models_MAP_{}_Prog_{}.Rdata'"),
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
                 h3("Please choose the desired palm age for the data import:"),
                 textInput('map', 'Desired palm age in months after planting:'),
                 p("Data will be prepared with the desired palm age +/- 20 months")
               ),
               tags$li(
                 hr(),
                 h3("Load the data:"),
                 p("Three ways are provided to import the",
                   "data: import them from the default data in VPALM-IDE,",
                   "importh them from a custom folder,",
                   strong("or"),"load each file separately."),
                 selectInput("load", "Choose here:", c(" ",
                                                       "Load data from folder",
                                                       "Load data from default folder",
                                                       'Load each file separately'),
                             selected = " ",
                             multiple = FALSE),
                 fluidRow(
                   wellPanel(
                     conditionalPanel(
                       condition = 'input.load == "Load data from folder"',
                       h2("Load data from local folder"),
                       p("Please choose the folder were the files resides:"),
                       shinyDirButton("dirdata", "Choose data folder", "Data folder"),
                       textOutput("data_path_info1"),
                       textOutput("data_path_info2"),
                       verbatimTextOutput("data_path_info3")
                     ),

                     conditionalPanel(
                       condition = 'input.load == "Load data from default folder"',
                       h2("Load data from local default file location"),
                       p("The default file location is located into the VPALM-IDE application."),
                       p(textOutput("architecture1", container = span))
                     ),
                     # Button to trigger data load from folder (default or user-specified):
                     conditionalPanel(
                       condition = 'input.load == "Load data from folder" | input.load == "Load data from default folder"',
                       br(),
                       p("Import the files using",code("Vpalmr::import_data"),":"),
                       actionButton("load_data_folder", label = "Load data"),
                       textOutput("test")
                     ),
                     # Load each file separately:
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
                     ),
                     conditionalPanel(
                       condition = 'output.data_trigger == "notok"',
                       br(),
                       p("Data could not be imported: please change the desired palm age.")
                     )
                   )
                 )
               ),
               conditionalPanel(
                 condition = 'output.data_trigger == "ok"',
                 tags$li(
                   hr(),
                   h3("Filter the progenies"),
                   # Option to filter the progenies from the imported data:
                   p("You can compute the parameters for all progenies in the data, or ",
                     "for some progenies only."),
                   uiOutput('progeny_filt'),
                   p("By default, VPalm will compute the parameters for all progenies at once.")
                 )
               ),
               conditionalPanel(
                 condition = 'output.data_trigger == "ok"',
                 # textOutput("data_trigger"),
                 tags$li(
                   h3("Glimpse of the imported data"),
                   p("Here is a preview of the parameter file from the data for you to control:"),
                   DT::dataTableOutput("datashow")
                 )
               ),
               conditionalPanel(
                 condition = 'output.data_filt_trigger != "ok" & output.data_filt_trigger != null',
                 wellPanel(
                   h3("Warning: missing data for selected progenies", style= 'color:red'),
                   p('Unable to compute architectural parameters.'),
                   p("Missing data for:", textOutput("data_filt_trigger")),
                   # p(textOutput("data_filt_trigger")),
                   p("Parameters can be fitted using another progeny data by adding it to the selection.")
                 )
               ),
               conditionalPanel(
                 condition = 'output.data_trigger == "ok" & output.data_filt_trigger == "ok"',
                 tags$li(
                   hr(),
                   h3("Compute the parameters (can take some time)"),
                   p("Now that you have imported the data files and possibly filtered the progenies,",
                     " you can compute the parameters by clicking the following button:"),
                   actionButton("updatearchi", "Update Architectural parameters"),
                   p(textOutput("modout"))
                 )
               )
             )
           ),
           conditionalPanel(
             condition = 'input.previous == "User-defined parameters (exploration)"',
             h2("User-defined values for the main parameters:"),
             p("You can explore different architectures based on custom values for the",
               "main input parameters for VPalm:"),
             fluidRow(
               column(3,numericInput(inputId = "MAP_custom",
                                     label="MAP",
                                     value = 60, min = 30, max = 100, step = 1)),
               column(3,numericInput(inputId = "rachisLengthRank1",
                                     label= "rachisLengthRank1",
                                     value = 489.3226, min = 1, max = 1000, step = 0.01)),
               column(3,numericInput(inputId = "rachisLengthSlope",
                                     label= "rachisLengthSlope",
                                     value = 2.245567, min = 0, max = 10, step = 0.000001)),
               column(3,numericInput(inputId = "nbMax",
                                     label= "nbMax",
                                     value = 139.53, min = 1, max = 1000, step = 0.01)),
               column(3,numericInput(inputId = "nbSlope",
                                     label= "nbSlope",
                                     value = 0.002938597, min = 0, max = 1, step = 0.000000001)),
               column(3,numericInput(inputId = "nbInfl",
                                     label= "nbInfl",
                                     value = 158.1657, min = 1, max = 100, step = 0.01)),
               column(3,numericInput(inputId = "B_length_Rank1",
                                     label= "B length Rank1",
                                     value = 95.68059, min = 1, max = 500, step = 0.00001)),
               column(3,numericInput(inputId = "leafletLengthAtBSlope",
                                     label=  "B length slope",
                                     value = 0.1407511, min = 0, max = 1, step = 0.0000001)),
               column(3,numericInput(inputId = "B_width_Rank1",
                                     label= "B width Rank 1",
                                     value = 5.331514, min = 1, max = 40, step = 0.001)),
               column(3,numericInput(inputId = "bWidthSlope",
                                     label= "B width slope",
                                     value = 0.005703363, min = 0, max = 1, step = 0.00000001)),
               column(3,numericInput(inputId = "xm_intercept",
                                     label= "xm_intercept",
                                     value = 0.175777, min = 0, max = 1, step = 0.0000001)),
               column(3,numericInput(inputId = "xm_slope",
                                     label= "xm_slope",
                                     value = 0.07976941, min = 0, max = 1, step = 0.0000001)),
               column(3,numericInput(inputId = "ym_intercept",
                                     label= "ym_intercept",
                                     value = 0.5057222, min = 0, max = 1, step = 0.0000001)),
               column(3,numericInput(inputId = "ym_slope",
                                     label= "ym_slope",
                                     value = -0.02517757, min = -1, max = 1, step = 0.00000001)),
               column(3,numericInput(inputId = "petioleRachisRatio_M",
                                     label= "petioleRachisRatio_M",
                                     value = 0.3068787, min = 0, max = 1, step = 0.0000001)),
               column(3,numericInput(inputId = "decMaxA",
                                     label= "decMaxA",
                                     value = 180, min = 0, max = 260, step = 0.1)),
               column(3,numericInput(inputId = "decSlopeA",
                                     label= "decSlopeA",
                                     value = 0.01, min = 0, max = 1, step = 0.01)),
               column(3,numericInput(inputId = "decInflA",
                                     label= "decInflA",
                                     value = 41.58479, min = 1, max = 100, step = 0.00001)),
               column(3,numericInput(inputId = "decliCintercept",
                                     label= "decliCintercept",
                                     value = 9.072, min = 0, max = 100, step = 0.001)),
               column(3,numericInput(inputId = "decliCslope",
                                     label= "decliCslope",
                                     value = 1.485, min = 0, max = 100, step = 0.001))
             ),
             p("Set the name of your output here:"),
             textInput(inputId = "custom_name", label= "name"),
             hr(),
             p("Save the VPalm inputs files:"),
             actionButton(inputId = "save_vpalm",label = "Save VPalm parameters"),
             hr(),
             p("To get the description and units of the variable, search in the following table:"),
             dataTableOutput('parameters_description')
           ),
           conditionalPanel(
             condition = 'output.param_trigger == "ok"',
             h3("Download the parameters for VPalm"),
             p("Now that the parameters are fitted (or imported), you can download the results for",
               "further usage:"),
             downloadButton("downloadData", "Download")
           )
  ),


  # Page 2: Calling VPalm, design the plot and export OPS/OPF files ---------

  tabPanel("OPS/OPF files making",
           tags$head(tags$style(HTML("
                            .shiny-text-output {background-color:#fff;}"))),
           h1("Call VPalm to ", span("compute OPF and OPS files", style = "font-weight: 300"),
              style = "color: #fff; text-align: center;
                             background-image: url('texturebg.png');
                             padding: 20px"),
           br(),
           p("A full 3D palm scene is constructed using",code("Vpalmr::make_scene()"),"which is applying several steps:",
             tags$ul(
               tags$li("VPalm parameter files are written on disk for each progeny and each tree required in the scene",
                       "from the parameters that were computed or imported beforehand in the first page of this application.",
                       "See",code("Vpalmr::extract_progeny()"),",",code("Vpalmr::format_progeny()"), "and",
                       code("Vpalmr::write_progeny()"),"for further information."),
               tags$li("The VPalm automaton is called to construct 3D palm mock-ups in OPFs format using the parameter files, see",
                       code("Vpalmr::make_opf_all()"),"for further information."),
               tags$li("The plot design is constructed according to the number of trees desired, see",code("Vpalmr::design_plot()"),
                       "for more details."),
               tags$li("And finally make the OPS (Open Plant Scene) file to link all elements of the scene using",
                       code("Vpalmr::make_ops_all()"),".")

             )
           ),
           p("Some parameters are needed to compute the 3D palm scenes:",
             tags$ul(
               tags$li("The directory where to write the outputs. Two folders are created on this diretory:",
                       "The Vpalm inputs folder, which contains the parameters values for each palm tree input",
                       "of VPalm ; and the scene folder, where the OPS and OPF files are written."),
               tags$li("The number of leaves of the 3D mock-up. Palms in the field have generally
                       45 leaves because leaves with rank greater than 45 are selectively pruned
                       as a consequence of the harvest."),
               tags$li("The progeny to use. A scene is made of palms from one progeny only for simplicity,
                       so the user can choose the progeny to use. An option is available to use the average
                       traits of all progenies for users that don't mind using a specific progeny. Choose
                        'All' if you want to use this option."),
               tags$li("The distance between palm trees in the scene design.")
             )
           ),
           p("Advanced parameters are also available for experienced users:",
             tags$ul(
               tags$li("The planting design of the scene. The default design is a quincunx disposition because
                        it is thought to be the optimum for light capture. Only two trees are used by default in the design
                        because it represents the Voronoï tessellation of the quincunx design, so
                        the scene can be duplicated afterwards to form a bigger scene, or even an infinte scene using toricity"),
               tags$li("The number of trees to be sampled for each progeny if genetic variability is required on the palm trees
                       of the scene. This option is very particular and should be used by advanced users only")
             ),
             "Note that these parameters should be used only by experienced users."
           ),
           conditionalPanel(
             condition = 'output.param_trigger == "notok"',
             wellPanel(
               h3("Warning: you must compute or import data first (see previous page) ", style= 'color:red'),
               p('Unable to find architectural parameters. Please import/compute them using the previous page.')
             )
           ),
           conditionalPanel(
             condition = 'output.param_trigger == "ok"',
             # Folder where to write the outputs:
             p("Folder of destination:"),
             shinyDirButton("dir", "Choose destination folder", "VPalm inputs and outputs folder"),
             textOutput("dirtrigger"),
             conditionalPanel(
               condition = 'output.dirtrigger == "Here is the folder you chose for the outputs:"',
               verbatimTextOutput("dir")
             ),
             br(),
             textOutput("dirtrigger2"),
             conditionalPanel(
               condition = 'output.dirtrigger2 == "Now you can set the parameters:"',

               # Number of leaves for the mock-up:
               numericInput(inputId = "nleaves", label = "Number of leaves in the OPFs",
                            value = 45, min = 3, max = 100, step = 1),
               # Progeny:
               uiOutput('progeny'),
               # Plant distance:
               numericInput(inputId = "plant_dist", label = "Distance between palm trees in the scene",
                            value = 9.2, min = 1, max = 100, step = 0.01),

               # Advanced parameters:
               p("You can trigger advanced parameters if you need them, but please remember they should be
             used by advanced users only."),
               # actionButton(inputId = "advanced_param_button",label = "Trigger advanced parameters"),
               p(strong("Trigger advanced parameters")),
               actionButton(inputId = "advanced_param_button",label = icon("exclamation-triangle"),
                            style= "color:white;
                       background-color: #E31F1F"),
               conditionalPanel(
                 condition = 'input.advanced_param_button%2==1',
                 wellPanel(
                   p("You seem to be an advanced user. Do you need to use a custom planting design ? Or maybe
               use randomly generated palm trees for each progeny instead of the average one ?"),
                   fluidRow(

                     numericInput(inputId = "nbtrees",
                                  label = "Number of random trees",
                                  value = 0, min = 1, max = 100, step = 1)
                   ),
                   br(),
                   fluidRow(
                     p("If you need to set a random seed, set a comma separated vector of length equal to the",
                       "number of trees required."),
                     textInput('seed', 'Enter a vector of seeds (comma delimited)')
                   ),
                   br(),
                   fluidRow(
                     p("To add a custom design into the VPalm app, you should follow the format of `design_plot()`. For example for 2 rows of 2 palm trees in quincunx (*i.e.* 4 palms), we
                      call", code("Vpalmr::design_plot()"), "as follows:"),
                     column(6,
                            code(
                              p("library(Vpalmr)"),
                              p("design_plot(rows = 2, cols = 1, x_dist = 9.2)")
                            )
                     )
                   ),
                   fluidRow(
                     p("The mandatory columns needed are as follows:"),
                     tableOutput("design_ex"),
                     p("So if you need a new design, make sure your design table follows exactly this format."),
                     fileInput("planting_design", label= "Custom planting design"),
                     em("Hint: Always check if the design of the plots below correspond to the one you inputed")
                   )
                 )
               ),
               h3("Compute the scene"),
               actionButton("makescene", "Make the scene",style = "color: white;
                       background-color: #088A08"),
               # Output the folders paths where the scene and Vpal inputs were written:
               conditionalPanel(
                 condition = 'output.scene_trigger == "ok"',
                 hr(),
                 h4("Scene successfully created and written"),
                 textOutput("dir_vpalm_inputs"),
                 br(),
                 textOutput("dir_scenes"), br(),
                 hr()
               ),
               h3("Plot of the stand design"),
               actionButton(inputId = "plot_button",label = "Show/Hide plots"),
               conditionalPanel(
                 condition = 'input.plot_button%2==1',
                 p("Here is a plot of the planting design. Note that this plot is informative only and do not represent
              the scene precisely because the size of the palms is arbitrary. Please open the scene in Xplot to get
              the true dimensions."),
                 p(strong("Choose the size of the palm trees in the scene (rendering purpose only):")),
                 numericInput(inputId = "palm_size",
                              label = "",
                              value = 0.4, min = 0.01, max = 10, step = 0.01),
                 plotOutput("plot_design", width= 600, height = 600,
                            click = "plot_click",
                            hover = "plot_hover",
                            brush = "plot_brush"
                 ),
                 verbatimTextOutput("plot_info"),
                 p("Here is a depiction of the plot that would be constructed using the previous voronoï:"),
                 numericInput(inputId = "voronois",
                              label = "How many voronoïs in col and rows ?",
                              value = 3, min = 1, max = 20, step = 1),
                 plotOutput("plot_design_rep", width= 1200, height = 1200,
                            click = "plot_click_rep",
                            hover = "plot_hover_rep",
                            brush = "plot_brush_rep"
                 ),
                 verbatimTextOutput("plot_info_rep")
               )
             )
           )
  )
)
