library(shiny)

ui <- fluidPage(
  titlePanel("Grille de PICROSS"),
  sidebarLayout(
    sidebarPanel(
      selectInput("grid_size",
                  "Grid Size:",
                  choices = c("5x5" = "5x5",
                              "5x10" = "5x10",
                              "10x10" = "10x10",
                              "10x15" = "10x15",
                              "15x15" = "15x15",
                              "15x20" = "15x20",
                              "20x20" = "20x20"),
                  selected = "5x5")
    ),
    mainPanel(
      uiOutput("buttonMatrix"),
      tags$head(
        tags$style(HTML("
          .square-button {
            width: 50px;
            height: 50px;
            margin: 1px;
          }
        "))
      )
    )
  )
)

server <- function(input, output) {
  output$buttonMatrix <- renderUI({
    grid_size <- as.numeric(unlist(strsplit(input$grid_size, "x")))
    num_buttons <- grid_size[1] * grid_size[2]
    
    buttons <- lapply(1:num_buttons, function(i) {
      actionButton(inputId = paste0("button", i), "", class = "btn btn-default square-button")
    })
    
    matrix_buttons <- matrix(buttons, nrow = grid_size[1], byrow = TRUE)
    
    rows <- lapply(1:grid_size[1], function(i) {
      fluidRow(do.call(tagList, matrix_buttons[i, ]))
    })
    
    tagList(rows)
  })
}

shinyApp(ui, server)
