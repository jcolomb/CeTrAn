require(shiny)

require (rhandsontable)
require (shinyFiles)
# Define UI for dataset viewer application
fluidPage(
  titlePanel("Trajectory Analysis for Multiple Buridan Experiments"),
  helpText("Creates some general statistical plots on multiple flies tracked in the
           Buridan Experiment. Includes speed, activity, distance and walks between the
           stripes, occupancy plots and activity time, pauses, as well as walking
           direction deviation from going to the stripe (histogram and mean of medians)"),
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
                  
                ),
                mainPanel(
                  #textOutput("filepaths"),
                  #textOutput("directorypath"),
                  h2("rgghome"),
                  textOutput("test")#,
                 # dataTableOutput("group"),
                  # downloadLink("pdflink")
                )
  
))

