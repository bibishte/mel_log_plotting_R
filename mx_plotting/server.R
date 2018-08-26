#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(psych)
library(qcc)

options(shiny.maxRequestSize = 300*1024^2)
my_data<-data.frame()
min <- c()
max <- c()
units <- c()
xCoord<-c()
yCoord<-c()
die<-c()
addToInput<-c()
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$file1,{
    
    withProgress({
      setProgress(message = "Locating the file...")
      Sys.sleep(0.1)
      incProgress(1/7)
      
      
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      
      
      setProgress(message = "Reading the file...")
      Sys.sleep(0.1)
      incProgress(1/6)
      
      
      my_data<<-read.csv(inFile$datapath, sep = ",", stringsAsFactors = FALSE)
     
      
      setProgress(message = "Deleating unnecessary columns...")
      Sys.sleep(0.1)
      incProgress(1/5)
      
      
      if(my_data[1,1]=="PROMPT")
      {
        min<<-c(my_data[4,])
        max<<-c(my_data[8,])
        units<<-c(my_data[9,])
        delete<-c("MEAS", "TESTER", "INFDATETIME", "MESSAGE", "BinDescription", "DieReady", "DieEnable", "SHT", "SLHT", "SIT", "STT", "SST", "SLT",
                  "SLLT", "ELT", "LogTime", "HandleTime", "IdleTime", "TestTime", "StatTime", "CycleTime")
        my_data <<- my_data[, !(colnames(my_data) %in% delete), drop=TRUE] 
        
        
        setProgress(message = "Deleating FAILS...")
        Sys.sleep(0.1)
        incProgress(1/5)
        
        
        my_data<<-subset(my_data, my_data$"BinType"=="PASS")
        my_data<<- my_data[,colSums(is.na(my_data))<nrow(my_data)]
        
        
        setProgress(message = "Getting cordinates !")
        Sys.sleep(0.1)
        incProgress(1/4)
        
        
        xCoord <<- c(as.numeric(paste(as.character(my_data[,"XPos"])), collapse=''))
        yCoord <<- c(as.numeric(paste(as.character(my_data[,"YPos"])), collapse=''))
        die <<- c(as.numeric(paste(as.character(my_data[,"DieNr"])), collapse=''))
      }
      
      else
      {
        min<<-c(my_data[1,])
        max<<-c(my_data[2,])
        units<<-c(my_data[3,])
        delete<-c( "setup.time", "mode.code", "part.type", "facility.id", "bincode description",
                  "DieReady", "DieEnable", "SHT", "SLHT", "SIT", "STT", "SST",
                  "SLT", "SLLT", "ELT", "LogTime", "HandleTime", "IdleTime",
                  "TestTime", "StatTime", "CycleTime", "Date", "Time", "MN",
                  "Temperature", "Humidity", "Device", "LotNr", "Condition", "SpecVersion", "ProgVersion", 
                  "Message", "Author", "HostName", "UserName", "SetupID", "DutBoardID",
                  "FamilyBoardID", "TesterID", "DiePack", "STS", "ETS","TestMode",
                  "ScreenID", "GnG_CC", "D_CC_Cable1", "D_CC_Cable2", "D_CC_Cable3",
                  "D_CC_Cable4", "D_CC_Cable5", "D_CC_Cable6", "D_CC_Cable7", "D_CC_Cable8",
                  "D_CC_Cable9", "D_CC_Cable10", "D_CC_Cable11", "D_CC_Cable12", "D_CC_Cable13",
                  "D_CC_Cable14", "D_CC_Cable15", "D_CC_Cable16", "V_CC_Cable1", "V_CC_Cable2",
                  "V_CC_Cable3", "V_CC_Cable4", "V_CC_Cable5", "V_CC_Cable6", "V_CC_Cable7",
                  "V_CC_Cable8","V_CC_Cable9", "V_CC_Cable10", "V_CC_Cable11", "V_CC_Cable12", 
                  "V_CC_Cable13", "V_CC_Cable14", "V_CC_Cable15", "V_CC_Cable16",
                  "msa_programName", "msa_step", "msa_programVersion","msa_testSpecVersion",
                  "msa_temperature","msa_testType", "msa_tag", "msa_machineID", "msa_machineType",
                  "msa_testerID", "msa_testerType", "msa_loadboardID", "msa_handlerBoardID",
                  "msa_probeCardID", "msa_cableID", "msa_otherEquID", "msa_facility",
                  "msa_lotID", "msa_motherLotName", "msa_splitName", "msa_waferID", "RgndCompensation",
                  "K_TC_FOsc_HT2RT_OSC", "D_TEMP_OFFSET_OTP","D_TEMP_SLOPE_OTP", "D_TEMP_SLOPE_N_OTP",
                  "K_TC_HT2RT_IBIAS", "Idd_IA2_OFFSUB_IC", "K_TC_HT2RT_Gain_SIGPATH", 
                  "K_TC_BistSensitivity_Z_HT2RT_IC")
        my_data <<- my_data[, !(colnames(my_data) %in% delete), drop=TRUE] 
        
        
        setProgress(message = "Deleating FAILS...")
        Sys.sleep(0.1)
        incProgress(1/5)
        
        
        my_data<<-subset(my_data, my_data$"bincode.type"=="PASS_BIN")
        my_data<<- my_data[,colSums(is.na(my_data))<nrow(my_data)]
      
      
        setProgress(message = "Getting cordinates...")
        Sys.sleep(0.1)
        incProgress(1/4)
        
      
        xCoord <<- c(my_data[,"x.coord"])
        yCoord <<- c(my_data[,"y.coord"])
        die <<- c(my_data[,"site.number"])
      }
      
      output$dataSelector <- renderUI({
        addToInput<<-c("", colnames(my_data))
        selectInput("select", label = h3("Select parameter"), 
                    choices=addToInput, 
                    selected = 1)
        })
      
      setProgress(message = "Uploading parameters into selector...")
      Sys.sleep(0.1)
      incProgress(1/3)
        
    })
     
  })
  
  
  
  
  
  observeEvent(input$bins,{
      observeEvent(input$select,{
        if(input$select=="")
        {
          return(NULL)
        }
        
        
        withProgress({
          setProgress(message = "Getting parameters...")
          Sys.sleep(0.1)
          incProgress(1/7)
          
          
          x<-as.numeric(paste(as.character(my_data[,input$select])), collapse='')
          minx <- as.numeric(paste(min[[input$select]],collapse = ''))
          maxx <- as.numeric(paste(max[[input$select]],collapse = ''))
          bins <- seq(minx, maxx, length.out = input$bins + 1)
          inputsbin<-input$bins
          dd<-paste(units[[inputsbin]] ,collapse = '')
          coordinates_plots<-data.frame(x, xCoord, yCoord)
          ordred_coordinates_plots <- coordinates_plots[order(xCoord, yCoord),]
          wid<-900+75*((inputsbin/66)+1)
          describe_output<-describeBy(x)
          describe_output$vars<-NULL
          
          output$text1 <- renderText({paste("You have selected:", input$select)})
          output$stat <- renderTable({
            describe_values<-cbind( minx, maxx, describe_output, describe_output[["mean"]] - 3*describe_output[["sd"]], describe_output[["mean"]] + 3*describe_output[["sd"]],
                              describe_output[["mean"]] - 6*describe_output[["sd"]], describe_output[["mean"]] + 6*describe_output[["sd"]], 
                              (describe_output[["mean"]]-minx)/(3*describe_output[["sd"]]),
                              (maxx-describe_output[["mean"]])/(3*describe_output[["sd"]]), (describe_output[["mean"]]-minx)/(6*describe_output[["sd"]]), 
                              (maxx-describe_output[["mean"]])/(6*describe_output[["sd"]]))
            describe_units<-c( "Low limit", "High limit", "measurment count", "arithmetic mean", "standart deviation", "median quadratic", "trimmed mean",
                                "mean absolute deviation", "min measured value", "max measured value", "range=max-min", "skew", "kurtosis", 
                                "standart error", "3S Low", "3S Hi", "6S Low", "6S Hi", "3S CPKL", "3S CPKH", "6S CPKL", "6S CPKH")
            describe_table<-rbind(describe_units, describe_values)
            describe_table<-t(describe_table)
            colnames(describe_table)<-c("Param", "value")
            describe_table
          })
        
          
          setProgress(message = "Rendering histogram...")
          Sys.sleep(0.1)
          incProgress(1/6)
          
          
          output$distPlot <- renderPlot({
            ggplot(coordinates_plots, mapping=aes(x=x)) +
              xlab(paste(units[[inputsbin]] ,collapse = '')) +
              geom_histogram(aes(y=..density..), binwidth=(maxx-minx)/input$bins, fill="#00fc4f", alpha=0.7)+
              geom_vline(aes(xintercept=minx), colour="#fc0000", linetype = "dashed") +
              geom_vline(aes(xintercept=maxx), colour="#fc0000", linetype = "dashed") +
              geom_vline(aes(xintercept=mean(x)), colour="#000000") +
              geom_vline(aes(xintercept=describe_output[["mean"]]), colour="#f904c4", linetype = "dashed") +
              geom_vline(aes(xintercept=describe_output[["median"]]), colour="#03e8f9", linetype = "dashed") +
              geom_density(fill="#b2aeae",alpha=0.1)
          }, width = wid)
      
          
          setProgress(message = "Rendering waffer map...")
          Sys.sleep(0.1)
          incProgress(1/5)
          
          
          output$wafferPlot <- renderPlot({
            ggplot(ordred_coordinates_plots, aes(xCoord, yCoord)) +
              coord_cartesian(xlim = c(min(xCoord), max(xCoord)), ylim = c(min(yCoord), max(yCoord))) +
              scale_x_continuous(breaks = seq(min(xCoord),max(xCoord))) +
              scale_y_continuous(breaks = seq(min(yCoord), max(yCoord)))+
              geom_tile(aes(fill=x))+
              guides(fill=guide_legend(keywidth = 2,keyheight = 2,nrow = 30 ))+
              theme(
                panel.background = element_rect(fill= 'white', color = 'white'),
                panel.grid.major = element_line(color='#E0E0E0'),
                panel.grid.minor = element_line(color='#E0E0E0'),
                axis.text.x  = element_text(angle=90)
              )+
              ggtitle('Wafer Map')+
              scale_fill_gradientn(name ="Legend",colors = rainbow(input$bins + 1,start = 0.1,end = 0.8), breaks=bins)
        
          }, width = wid)
      
          
          setProgress(message = "Rendering violin plot...")
          Sys.sleep(0.1)
          incProgress(1/4)
       
          
          output$violinPlot <- renderPlot({
            ggplot(coordinates_plots, aes(factor(die), x, color=die)) + 
              geom_violin(trim=FALSE) +
              geom_jitter(shape=16, position=position_jitter(0.02))
          }, width=wid)
          
          output$spcPlot <- renderPlot({
            #qcc.options(run.length=10000)    Remove violating runs
            qcc(x, type = "xbar.one", limits=c(mean(x)-3*sd(x), mean(x)+3*sd(x)), plot=TRUE, title="3S Outlayers",
                ylab=dd, xlab=" ")
          }, width = wid)
          
          output$spcPlot_sixSigma <- renderPlot({
            #qcc.options(run.length=10000)     Remove violating runs
            qcc(x, type = "xbar.one", limits=c(mean(x)-6*sd(x), mean(x)+6*sd(x)), plot=TRUE, title="6S Outlayers", 
                ylab=dd, xlab=" ")
          }, width = wid)
          
          
          setProgress(message = "Finishing...")
          Sys.sleep(0.1)
          incProgress(1/3)
            
        
        })
    })
    
  })

}