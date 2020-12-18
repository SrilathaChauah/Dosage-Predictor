############################################
# Srilatha Chauah                          #
# Dosage Predictor                         #
############################################

# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]


####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("cosmo"),
                      navbarPage("Dosage Predictor",
                                 
                                 tabPanel("Main",
                                          # Input values
                                          sidebarPanel(
                                            HTML("<h3>Input parameters</h4>"),
                                            sliderInput("Dmd", label = "Dmd", value = 624.20,
                                                        min = min(TrainSet$Dmd),
                                                        max = max(TrainSet$Dmd)),
                                            sliderInput("Fbxo32", label = "Fbxo32", value = 285.18,
                                                        min = min(TrainSet$Fbxo32),
                                                        max = max(TrainSet$Fbxo32)),
                                            sliderInput("Klf15", label = "Klf15", value = 30.63,
                                                        min = min(TrainSet$Klf15),
                                                        max = max(TrainSet$Klf15)),
                                            sliderInput("Mef2a", label = "Mef2a", value = 100.00,
                                                        min = min(TrainSet$Mef2a),
                                                        max = max(TrainSet$Mef2a)),
                                            sliderInput("Anxa6", label = "Anxa6", value = 200.43,
                                                        min = min(TrainSet$Anxa6),
                                                        max = max(TrainSet$Anxa6)),
                                            sliderInput("Cxcl12", label = "Cxcl12", value = 92.90,
                                                        min = min(TrainSet$Cxcl12),
                                                        max = max(TrainSet$Cxcl12)),
                                            sliderInput("Ccl7", label = "Ccl7", value = 2.54,
                                                        min = min(TrainSet$Ccl7),
                                                        max = max(TrainSet$Ccl7)),
                                            
                                            actionButton("submitbutton", "Submit", class = "btn btn-primary")
                                          ),
                                          
                                          mainPanel(
                                            tags$label(h3('Status/Output')), # Status/Output Text Box
                                            verbatimTextOutput('contents'),
                                            tableOutput('tabledata') # Prediction results table
                                            
                                          )
                                  ),
                                 
                                 tabPanel("About",
                                          titlePanel("About"),
                                          div(includeMarkdown("about.md"),
                                              align="justify")
                                          )
                      )
  
  
  
  
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Dmd",
               "Fbxo32",
               "Klf15",
               "Mef2a",
               "Anxa6",
               "Cxcl12",
               "Ccl7"),
      Value = as.character(c(input$Dmd,
                             input$Fbxo32,
                             input$Klf15,
                             input$Mef2a,
                             input$Anxa6,
                             input$Cxcl12,
                             input$Ccl7)),
      stringsAsFactors = FALSE)
    
    Treatment <- 0
    df <- rbind(df, Treatment)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Prediction complete.") 
    } else {
      return("Server is ready for prediction.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
