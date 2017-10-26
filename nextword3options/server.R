source("prediction_three_choices.R")

# Define server logic required to draw a histogram
shinyServer(
  function(input, output) {
    dataInput <- reactive({
      prediction(input$text1,ngram_table_list)
    })
    
    output$inputText <- renderPrint({input$text1})
    output$prediction1 <- renderPrint({dataInput()[1]}) 
    output$prediction2 <- renderPrint({dataInput()[2]}) 
    output$prediction3 <- renderPrint({dataInput()[3]}) 
  })
