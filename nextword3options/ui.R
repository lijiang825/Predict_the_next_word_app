library(shiny)

shinyUI(fluidPage( 
  titlePanel("Predict the next word app"), 
  
  sidebarLayout(
    sidebarPanel(
      helpText("Input some phrases and this app will predict a possible next word."),
      
      textInput(inputId="text1", label = "Input phrases", value = NULL)
      
      #submitButton('Submit')
    ),
    
    mainPanel(
      h3('Results of prediction'),
      br(),
      h4('You entered'), 
      verbatimTextOutput("inputText"), 
      br(),
      h4('The first predicted next word is '), 
      verbatimTextOutput("prediction1"),
      
      h4('The second predicted next word is '), 
      verbatimTextOutput("prediction2"),
      
      h4('The third predicted next word is '), 
      verbatimTextOutput("prediction3")
    )
  )
))
