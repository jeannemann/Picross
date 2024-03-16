if (interactive()) {
  
  ui <- fluidPage(
    titlePanel("Matrice de Cases Cliquables"),
    sidebarLayout(
      sidebarPanel(
        selectInput("matrix_size",
                    "Matrix Size:",
                    choices = c("3x3" = "3x3",
                                "4x4" = "4x4",
                                "5x5" = "5x5"),
                    selected = "3x3")
      ),
      mainPanel(
        uiOutput("clickableMatrix")
      )
    )
  )
  
  server <- function(input, output) {
    output$clickableMatrix <- renderUI({
      matrix_size <- as.numeric(unlist(strsplit(input$matrix_size, "x")))
      num_boxes <- matrix_size[1] * matrix_size[2]
      
      boxes <- lapply(1:num_boxes, function(i) {
        checkboxInput(inputId = paste0("box", i), label = NULL)
      })
      
      matrix_boxes <- matrix(boxes, nrow = matrix_size[1], ncol = matrix_size[2])
      
      rows <- lapply(1:matrix_size[1], function(i) {
        fluidRow(do.call(tagList, matrix_boxes[i, ]))
      })
      cols <- lapply(1:matrix_size[2], function(j) {
        fluidRow(do.call(tagList, matrix_boxes[ ,j]))
      })
      
      tagList(rows)
    })
  }
  
  shinyApp(ui, server)
}

