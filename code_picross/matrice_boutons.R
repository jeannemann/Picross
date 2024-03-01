library(shiny)

ui <- fluidPage(
  titlePanel("Matrice de Boutons Carrés"),
  uiOutput("buttonMatrix"),
  tags$head(
    tags$style(HTML("
      .square-button {
        width: 50px;
        height: 50px;
        margin: 2px;
      }
    "))
  )
)

server <- function(input, output) {
  output$buttonMatrix <- renderUI({
    buttons <- lapply(1:25, function(i) {
      actionButton(inputId = paste0("button", i), "", class = "btn btn-default square-button")
    })
    
    matrix_buttons <- matrix(buttons, nrow = 5, byrow = TRUE)
    
    rows <- lapply(1:5, function(i) {
      fluidRow(do.call(tagList, matrix_buttons[i, ]))
    })
    
    tagList(rows)
  })
}

shinyApp(ui, server)
