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
          width: 30px;
          height: 30px;
        }
        
        .matrix-container {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(30px, 1fr));
          gap: 1px;
          max-width: 100%; /* Ne pas dépasser la largeur du conteneur parent */
          overflow-x: auto; /* Ajoutez une barre de défilement horizontale si nécessaire */
          max-height: 150vh; /* Ajustez la hauteur maximale selon vos besoins */
          overflow-y: auto;
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
    
    button_list <- lapply(1:num_buttons, function(i) {
      actionButton(inputId = paste0("button", i), "", class = "btn btn-default square-button")
    })
    
    cols <- grid_size[2]  # Nombre de colonnes dans la grille
    col_width <- "30px"  # Ajustez la largeur des colonnes
    
    tagList(
      tags$style(HTML(sprintf("
        .matrix-container { 
          grid-template-columns: repeat(%d, %s); 
          gap: 1px; 
          max-width: 100%%; 
          overflow-x: auto; 
          max-height: 150vh; 
          overflow-y: auto;
        }
      ", cols, col_width))),
      div(id = "matrix-container", class = "matrix-container", button_list)
    )
  })
}

shinyApp(ui, server)
