library(shiny)

shinyServer(function(input, output) {
  output$Graf <- renderPlot({
    ggplot(zdr %>% filter(leto == input$izberi, bruto/povrsina > 50),
           aes_string(x = "kraj", y = input$izb, fill = "mesec")) +
      geom_bar(stat="identity",position = "dodge",size=10) +
      coord_flip()
  })
})