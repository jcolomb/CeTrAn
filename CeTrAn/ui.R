require(shiny)

require (rhandsontable)
require (shinyFiles)
# Define UI for dataset viewer application
fluidPage(
  titlePanel("Centroid Trajectory Analysis (made for Multiple Buridan Experiments)"),
  tags$div("CeTrAn was mainly developed by Dr. Julien Colomb. Issues should be reported on",
           tags$a(href="https://github.com/jcolomb/Cetran/issues", "the github page")
  ),
  helpText("This app creates some general statistical plots on multiple flies tracked in the
           Buridan Experiment. Please use the butons on the left to set the path to the folder containing the data and 
           and choose the file containing the grouping information (2 tab separated columns with the path to the 
           xml file and the goup name for that fly). Then set the variables and click the GO button. Follow the progress
           of the analysis on the R console."),
  

  sidebarLayout(position = "left",
                
                sidebarPanel(
                  textInput("outputfile", "Name of output file:", "output"),
                  shinyDirButton("g_outputdir", "Output Directory", 
                                 "Choose the output directory:"),
                  h1("Input"),
                  shinyDirButton("g_inputdir", "Input Directory", 
                                 "Choose the input directory containing all the xml and data files:"),
                  shinyFilesButton("g_filetablename", "File Table",
                                   "Select a tab seperated file containing the matrix:", FALSE),
                  checkboxInput("g_open_pdf", "Open plot in default pdf-viewer", FALSE),
                  h1("Options"),
                  sliderInput("g_duration_slider", label = h3("Min duration for pause (*dt),
                                                              and of activities in (*0.4s)"),
                              min = 0, max = 20, value = 10),
                  helpText("1/10 s"),
                  h2("Options for histograms"),
                  sliderInput("g_bin_size", label = h3("Bin size"),
                              min = 0, max = 20, value = 1),
                  helpText("0 = 0.5 in s"),
                  h2("Options for occupancy plot"),
                  checkboxInput("g_supress_paints", "Supress paints, if the fly is not moving", TRUE),
                  sliderInput("g_treshold", label = h3("Threshold set to"),
                              min = 0, max = 20, value = 8),
                  helpText("treshold for movt*10, 10 in arbitrary values"),
                  h2("Analysis done:"),
                  checkboxInput("g_general", "general", TRUE),
                  checkboxInput("g_roundarena", "thigmotaxis in round arena", TRUE),
                  checkboxInput("g_stripes_deviation", "stripe deviation calculation", TRUE),
                  br(),
                  checkboxInput("g_occupancy", "occupancy", TRUE),
                  checkboxInput("g_angledev", "angle deviation", TRUE),
                  checkboxInput("g_outputtext", "output text files", TRUE),
                  checkboxInput("g_log", "activity time threshold", TRUE),
                  br(),
                  checkboxInput("g_thigmo", "thigmotaxis", TRUE),
                  checkboxInput("g_pca", "PCA", TRUE),
                  checkboxInput("g_individual", "do individual analyses (other pdf file)", TRUE)
                  
                , width = 4),
                mainPanel(
                  
                  #textOutput("filepaths"),
                  #textOutput("directorypath"),
                  helpText("the first column must show the path to the xml file from th folder entered as input folder, the second column is the group names"),
                  dataTableOutput("group"),
                 actionButton("goButton", "Go!"),
                 h2("you will find the analysis in a pdf here:"),
                 textOutput("test"),
                 
                   downloadLink("pdflink")
                )
  
))

