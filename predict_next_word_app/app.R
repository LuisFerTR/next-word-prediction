#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("sbo")
source("./clean_data.R")

# Define UI for application that draws a histogram
ui <- fillPage(

    # Application title
    navbarPage("Next word prediction"),
  
    theme = bslib::bs_theme(bootswatch = "sandstone"),
    
    
    titlePanel(
      h1("Type a phrase", align = "center")
    ),
    

    fluidRow(column(4, offset = 4, textInput("text", label = "", 
                                             value = "", width = "100%", 
                                             placeholder = "Enter text..."))),
    
    
    uiOutput("predictions"),
    
    # fluidRow(column(3, textOutput("value")))
    
    
    # tagList(
    #   tags$head(
    #     tags$style(HTML(
    #       "html {
    #          position: relative;
    #          min-height: 100%;
    #        }
    #        body {
    #          margin-bottom: 60px; /* Margin bottom by footer height */
    #        }
    #        .footer {
    #          position: absolute;
    #          bottom: 0;
    #          width: 100%;
    #          height: 60px; /* Set the fixed height of the footer here */
    #          background-color: #f5f5f5;
    #        }"))),
    #   tags$footer("The footer.", class = "footer")
    # )
)

# Define server logic 
server <- function(input, output) {
    # Load prediction table
    load("./t4.RData")
  
    # Create prediction model
    p4 <- sbo_predictor(t4)
    
    
    predict_next_words <- reactive({
      words <- predict(p4, input$text)
      
      if ("<EOS>" %in% words) {
        words <- words[words != "<EOS>"]
      }
      
      words
    })
    
    output$value <- renderText({ input$text == "" })
  
    
    output$predictions <- renderUI(
      {
        words <- predict_next_words()
        fluidRow(column(4, align = "center",
                        div(words[1], class = "btn btn-success")),
                 column(4, align = "center",
                        div(words[2], class = "btn btn-info")),
                 column(4, align = "center", 
                        div(words[3], class = "btn btn-warning")))
        }
      )
}

# Run the application 
shinyApp(ui = ui, server = server)
