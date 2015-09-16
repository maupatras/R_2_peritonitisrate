# This is the peritonial analysis application.

library(shiny)
shinyServer(function(input, output) {
  
  patientdata <- reactive({   
    
    inFile <- input$file1
  
 if (is.null(inFile)){
  
   return(NULL)
  
  }else{
 
     read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, na.strings = c(" NA", "NULL", "", " ", "NA"), encoding = 'UTF-8')
    
  
  }
  })
  
  
  peritonitis_rate <- eventReactive(input$goButton,{
   
      if (is.null(patientdata)){
  
          return(NULL)
  
      }else{
  
            
            #read file and return data frame
            patient <- patientdata()
          
            #name the columns of patient data frame
            names(patient) <- c("patient", "dateofstart", "dateofdeath", "episode", "dateofinfection")
            #subset data for selected columns
            selectedcolumns <- c( "dateofstart", "dateofdeath", "episode", "dateofinfection")
            patient <- patient[selectedcolumns]
            #subset of patients are having therapy
            activepatient <- subset(patient, is.na(dateofdeath) )
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
            
            enddate <- c("31 12")
            currentenddate <- as.Date(paste(enddate, currentyear), "%d %m %Y")
            startdate <- c("01 01")
            currentstartdate <- as.Date(paste(startdate, currentyear), "%d %m %Y")
            
            # total patient days for current year
            activedays<- (currentenddate - activepatientstart)
          #  activedays<- (activedays, !is.na(activedays))
            # total therapy days for active and ex patients for current year
            totaltherapydays <- c(activedays, expatient_totaldays)
            totaltherapydays<- subset(totaltherapydays, !is.na(totaltherapydays))
            totalpatientdays <- as.numeric(sum(totaltherapydays))
            
            #subset of current year patients with episode  
            infectionDate <- as.Date(as.character(patient$dateofinfection),  format="%d/%m/%Y")
            currentyearInfections <- subset(patient, infectionDate >= currentstartdate )
            currentyearInfections <- subset(currentyearInfections, currentyearInfections$episode > 0)
            # sum of current year infections
            totalinfections <- sum(currentyearInfections$episode)
            #patient years 
            patientyears <- totalpatientdays / 365
            #how many peritonitis per patient year
            rate <- totalinfections / patientyears
            #should also show the percentage of patients are free peritonitis
    }
  })
  
  #display file data in table format
  output$contents <- renderDataTable({
      data.frame(patientdata())
  })
  
  #display the peritonitis rate
  output$sum <- renderText({
    peritonitis_result <- peritonitis_rate()
    str <- paste0('Ο ρυθμός εμφάνισης περιτονίτιδας του τρέχοντος έτους είναι: "', peritonitis_result, '"')
    
  })
  
  
  examplefile <- reactive({
    localfile <- read.csv("peritonitisrate.csv", header = TRUE, na.strings = c("NA", "NULL", ""), sep = ";", encoding = 'UTF-8', fileEncoding = 'CP1253')
   
    })
  
  output$downloadData <- downloadHandler(
       filename = function() {
         paste('examplefile', '.csv')
       },
       
      content = function(file) {
        write.table(examplefile(), file, sep=";", fileEncoding = 'CP1253')
        
       }
    
     )
  
  output$downloadReport <- downloadHandler(
    
    filename = function() {
      paste('AnnualReport_PeritonitisRate', sep = '.', 'docx')
    },
    
    content = function(file) {
      src <- normalizePath('peritonitisrate_report.Rmd')
    
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
  
      out <- render('peritonitisrate_report.Rmd', output_format = word_document(), encoding = 'UTF-8')
      
      file.rename(out, file)
    })
  
   
  
  
})