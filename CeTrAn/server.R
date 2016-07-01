require(shiny)
require(rhandsontable)
require(tidyr)

rgghome <-  getwd()

shinyServer(function(input, output, session) {
  
  volumes <- getVolumes() #c(wd=".") #
  #origin <- rgghome #to set compared to volumes
  
  #shinyDirChoose(input, 'g_inputdir', roots=volumes, session=session)

  
   #shinyDirChoose(input, 'g_outputdir', session=session,
    #             roots=volumes,restrictions=system.file(package='base') )
  
   
  
  # shinyFileChoose(input, "g_filetablename", session=session, roots=getVolumes(), filetypes=c("", ".txt"))
  
  
  #outputfolder <- reactive ( parseDirPath( root,input$outputdir))
  
  #output$test <- renderPrint({rgghome}) #ok
  #output$test <-  renderPrint({as.character(parseFilePaths(volumes, input$g_filetablename)$datapath)}) #ok
  #output$test <- renderPrint({as.character(volumes)})
   shinyDirChoose(input, 'g_inputdir', roots=volumes, session=session, restrictions=system.file(package='base'))
   
   fileInput <- reactive({
     filepath= (parseDirPath(volumes, input$g_inputdir))
     filepath
   })
   shinyFileChoose(input, "g_filetablename", session=session, roots=volumes)
   
   shinyDirChoose(input, 'g_outputdir', roots=volumes, session=session, restrictions=system.file(package='base'))
   
   fileOutput <- reactive({
     filepath= (parseDirPath(volumes, input$g_outputdir))
     
     if (length (filepath) <1) {
       if (!dir.exists (paste(c(fileInput(),"/output"),collapse=""))){
         filepath= dir.create  (paste(c(fileInput(),"/output"),collapse=""))
       }
       
       filepath= paste(c(fileInput(),"/output"),collapse="")
              #shinyfile app not tested
       # #       outputpath = paste(c(datapath,"/output"),collapse="")	
       # #     }
      
     }
     #filepath = paste0(origin,"/",filepath)
     filepath
   })
   
   output$test <- renderPrint({ fileOutput()})
 
  # output$test <- renderPrint({parseFilePaths(volumes,input$g_inputdir)})
  
  #output$group <- renderDataTable( input$g_inputdir)
  
  
  
  DataOutput <- reactive({
    g_filetablename <- as.character(parseFilePaths(volumes, input$g_filetablename)$datapath) 
    g_inputdir <- fileInput()
    g_outputdir <- fileOutput()
    g_filetable <- read.csv(g_filetablename,sep = "\t", header=FALSE)
    g_duration_slider <- input$g_duration_slider
    g_bin_size <- input$g_bin_size
    g_suppress_paints <- input$g_supress_paints
    g_treshold <- input$g_treshold
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
    outputfile <- input$outputfile
    
    
    
    # call main program
    source("CeTrAn_norgg_xml.r", local=TRUE)
    

  })

  output$pdflink <- downloadHandler(
    filename <- input$outputfile,
    content <- function(file) {
      DataOutput()
      file.copy(bla, file)
    }
  )
    
  

 
})