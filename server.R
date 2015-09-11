# This is the peritonial analysis application.

library(shiny)
shinyServer(function(input, output) {
  
  patientdata <- reactive({   
    
    inFile <- input$file1
  
 if (is.null(inFile)){
  
   return(NULL)
  
  }else{
  # pp <- read.csv("C:/xampp/htdocs/InfectionStatisticalAnalysis/nephrology_patients.csv", header = TRUE, na.strings="NA", sep = ";")
  read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, na.strings = c("NA", "NULL", ""), encoding = 'UTF-8', fileEncoding = 'CP1253')
    
  
  }
  })
  
  
  peritonitis_rate <- eventReactive(input$goButton,{
    
    # inFile <- input$file1

      if (is.null(patientdata)){
  
          return(NULL)
  
      }else{
  
            
            #read file and return data frame
            #patient <- read.csv(patientdata,header=input$header, sep=input$sep, quote=input$quote, na.strings="NA")
            #patient <- read.csv(inFile$datapath, header = TRUE, na.strings="NA", sep = ";")
            #patient <- read.table("C:/Users/MelinaG/Documents/peritonitisrate/peritonitisrate2013.csv", header = TRUE, na.strings = c("NA", "NULL", ""), sep = ";")
        
            #library(foreign)
            #sppssfile <- read.spss("C:/Users/MelinaG/Documents/InfectionStatisticalAnalysis/pdr2013.sav", to.data.frame=TRUE) 
            
            
            patient <- patientdata()
            #name the columns of patient data frame
            names(patient) <- c("patient", "dateofstart", "dateofdeath", "infection", "dateofinfection")
            
            #subset of patients are having therapy
            activepatient <- subset(patient, is.na(dateofdeath))
            activepatientstart <- as.Date(activepatient$dateofstart,  format="%d/%m/%Y")
            
            #subset of patients has stopped therapy
            expatient <- subset(patient, !is.na(dateofdeath))
            expatient_enddate <- as.Date(expatient$dateofdeath,  format="%d/%m/%Y")
            expatient_startdate <- as.Date(expatient$dateofstart,  format="%d/%m/%Y")
            #total days of therapy per expatient
            expatient_totaldays <- (expatient_enddate - expatient_startdate)
            
            #set the current year end date
            
            if (is.null(input$year)){
              
              currentyear <- format(Sys.Date(), "%Y")
            }
            else{
              currentyear <- input$year
            }
            #currentyear = "2013"
            enddate <- c("31 12")
            currentenddate <- as.Date(paste(enddate, currentyear), "%d %m %Y")
            startdate <- c("01 01")
            currentstartdate <- as.Date(paste(startdate, currentyear), "%d %m %Y")
            
            # total patient days for current year
            activedays<- (currentenddate - activepatientstart)
            # total therapy days for active and ex patients for current year
            totaltherapydays <- c(activedays, expatient_totaldays)
            totalpatientdays <- as.numeric(sum(totaltherapydays))
            
            #subset of current year patients with episode  
            infectionDate <- as.Date(as.character(patient$dateofinfection),  format="%d/%m/%Y")
            currentyearInfections <- subset(patient, infectionDate >= currentstartdate )
            currentyearInfections <- subset(currentyearInfections, currentyearInfections$infection > 0)
            # sum of current year infections
            totalinfections <- sum(currentyearInfections$infection)
            #patient years 
            patientyears <- totalpatientdays / 365
            #how many peritonitis per patient year
            rate <- totalinfections / patientyears
            #should also show the percentage of patients are free peritonitis
    }
  })
  
  #dispaly  file data in table format
  output$contents <- renderDataTable({
      data.frame(patientdata())
  })
  
  #display the peritonitis rate
  output$sum <- renderText({
    peritonitis_result <- peritonitis_rate()
    str <- paste0('Ο ρυθμός εμφάνισης περιτονίτιδας του τρέχοντος έτους είναι: "', peritonitis_result, '"')
    
  })
  
  
  examplefile <- reactive({
    localfile <- read.csv("peritonitisrate2015.csv", header = TRUE, na.strings = c("NA", "NULL", ""), sep = ";")
   
    })
  
  output$downloadData <- downloadHandler(
       filename = function() {
         paste('examplefile', '.csv')
       },
       
      content = function(file) {
        write.table(examplefile(), file, sep=";")
        
       }
    
     )
  
  output$downloadReport <- downloadHandler(
    
    filename = function() {
      paste('AnnualReport_PeritonitisRate', sep = '.', 'docx')
    },
    
    content = function(file) {
      src <- normalizePath('peritonitisrate_report.Rmd')
    
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
     # owd <- setwd(tempdir())
    #  on.exit(setwd(owd))
    #  file.copy(src, 'peritonitisrate_report.Rmd')
      
      library(rmarkdown)
      
      #set the author name
      theauthor <- input$author
      #set the clinic name
      theclinic <- input$clinic
      #set the calculated peritonial analysis rate
      therate <- peritonitis_rate()
      #set the year
      theyear <- input$year
      
      # report 
      
    #  out <- render('prd6.Rmd', params = params, switch(
     #   input$format,
    #    PDF = pdf_document(), HTML = html_document(), Word = word_document()
     # ), encoding = 'UTF-8')
      out <- render('peritonitisrate_report.Rmd', output_format = word_document(), encoding = 'UTF-8')
      
      file.rename(out, file)
    })
  
   
  
  
})