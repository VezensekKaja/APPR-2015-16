library(shiny)

shinyServer(function(input, output) {
  output$Graf <- renderPlot({
    zdr.leto <- zdr %>% filter(leto == input$izberi)
    ggplot(zdr.leto %>% group_by(kraj) %>%
             summarise(razmerje = max(bruto/povrsina)) %>%
             filter(razmerje > 50) %>% inner_join(zdr.leto, by = "kraj"),
           aes_string(x = "kraj", y = input$izb, fill = "mesec")) +
      geom_bar(stat="identity",position = "dodge",size=10) +
      coord_flip()
  })
})