library(shiny)

ui <- fluidPage(
  headerPanel('Jeu de Picross'),
  sidebarPanel(
    selectInput('5x5', '10x10', '20x20'),
    selectInput('ycol', 'Y Variable', names(iris),selected = names(iris)[2]),
    numericInput('clusters', 'Cluster count', 3,min = 1, max = 9)),
  mainPanel(plotOutput('plot1'))
)

server <-function(input, output) {
  datatocluster <- reactive(iris[,c(input$xcol,input$ycol)])
  clustering <- reactive(kmeans(datatocluster(),centers=input$clusters))
  output$plot1 <- renderPlot(
    {plot(datatocluster(),col=clustering()$cluster+1,pch=20) ;
      points(clustering()$centers,pch="X",cex=4)})
}

shinyApp(ui = ui, server = server)