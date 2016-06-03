require(shiny)
require(rhandsontable)
require(tidyr)
rgghome <-  paste(getwd(),"CeTrAn", sep = "/")

shinyServer(function(input, output, session) {
  
  volumes <- c(wd=".")
  shinyDirChoose(input, 'g_inputdir', session=session,
                 roots=volumes)
  
  shinyDirChoose(input, 'g_outputdir', session=session,
                 roots=volumes)
  
  shinyFileChoose(input, "g_filetablename", session=session, roots=volumes)
  # shinyFileChoose(input, "g_filetablename", session=session, roots=getVolumes(), filetypes=c("", ".txt"))
  
  DataOutput <- reactive({
    g_filetablename <- as.character(parseFilePaths(volumes, input$g_filetablename)$datapath)
    g_filetable <- read.csv(g_filetablename,sep = "\t", header=FALSE)
    g_duration_slider <- input$g_duration_slider
    g_bin_size <- input$g_bin_size
    g_suppress_paints <- input$g_supress_paints
    g_threshold <- input$g_treshold
    g_general <- input$g_general
    g_roundarena <- input$g_roundarena
    g_stripes_deviation <- input$g_stripes_deviation
    g_occupancy <- input$g_occupancy
    g_angledev <- input$g_angledev
    g_log <- input$g_log
    g_outputtext <- input$g_outputtext
    g_thigmo <- input$g_thigmo
    g_pca <- input$g_pca
    g_individual <- input$g_individual
    g_open_pdf <- input$g_open_pdf
    

    # call main program
    source("CeTrAn_norgg_xml.r")
  })
  
  
  output$pdflink <- downloadHandler(
    filename <- input$outputfile,
    content <- function(file) {
      DataOutput()
      dev.copy2pdf(file = file, width=12, height=8, out.type="pdf")
      # file.copy("plot.pdf", file)
    }
  )
    
    

  output$filepaths <- renderPrint({parseFilePaths(volumes, input$g_filetablename)$datapath})
  output$directorypath <- renderPrint({parseDirPath(volumes, input$g_inputdir)})
  
 
})