library(shiny)

shinyServer(function(input, output) {
  output$Graf <- renderPlot({
    ggplot(zdr %>% filter(leto == input$izberi, povrsina > 200),
           aes_string(x = "kraj", y = input$izb)) +
      geom_bar(stat="identity",fill="deeppink3",size=10) +
      coord_flip()
  })
})
