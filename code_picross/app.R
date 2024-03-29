library(shiny)

ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Grille de PICROSS"),
  
  # Use fixed layout
  tags$head(tags$script(HTML(
    '
        $(document).on("click", ".grid-cell", function() {
          var cell = $(this);
          if (!cell.hasClass("darkblue")) {
            cell.addClass("darkblue");
            cell.css("background-color", "#333");
          } else {
            cell.removeClass("darkblue");
            cell.css("background-color", "white");
          }
        });
        '))),
  
  # Sidebar with a select input for grid size
  sidebarLayout(
    sidebarPanel(
      selectInput("grid_size",
                  "Grid Size:",
                  choices = c("5x5", 
                              "5x10", 
                              "10x10", 
                              "10x15", 
                              "15x15", 
                              "15x20", 
                              "20x20"),
                  selected = "10x10")
    ),
    
    # Show the grid of black and white squares
    mainPanel(
      uiOutput("grid"),
      style = "width: 40%; 
               height: 100%;
               margin: 10px;
               overflow:
               auto;"
    )
  )
)

# Définit un server logique pour générer la grille
server <- function(input, output, session) {
  
  # Fonction pour compter les cases noires dans le Picross
  consecutiveCounts <- function(vector) {
    runs <- rle(vector == 1)
    counts <- runs$lengths[runs$values == TRUE]
    return(counts)
  }
  
  # Fonction pour générer une grille de picross aléatoire
  grille_aleatoire <- function(dim) {
    grid <- matrix(sample(c(0, 1), dim[1] * dim[2], replace = TRUE, prob = c(0.6, 0.4)), nrow = dim[1], ncol = dim[2])
    return(grid)
  }
  
  # Crée la grille
  grid <- reactiveVal(NULL)
  
  observeEvent(input$grid_size, {
    dim <- as.numeric(unlist(strsplit(input$grid_size, "x")))
    grid(grille_aleatoire(dim))
  })
  
  observe({
    # Met à jour la grille grâce au clic
    if (!is.null(grid())) {
      for (i in 1:nrow(grid())) {
        for (j in 1:ncol(grid())) {
          id <- paste0("cell_", i, "_", j)
          observeEvent(input[[id]], {
            if (grid()[i, j]  == 0)
            new_grid[i, j] <- 1 
            else new_grid[i, j] <- 0  
            style <- if (cell_value == 1) "background-color: darkblue;" else "background-color: white;"
            actionButton(
              id, 
              "", 
              style = paste0(
                "width: 100%;",
                "height: 100%;",
                style
              ),
              class = "grid-cell"
            )
            grid(new_grid)
          })
        }
      }
    }
  })
  
  output$grid <- renderUI({
    if (!is.null(grid())) {
      dim <- dim(grid())
      
      div(
        style = paste0(
          "display: grid;",
          "grid-template-columns: repeat(", dim[2], ", 1fr);",
          "grid-template-rows: repeat(", dim[1], ", 1fr);",
          "grid-gap: 1px;"
        ),
        lapply(1:dim[1], function(i) {
          lapply(1:dim[2], function(j) {
            id <- paste0("cell_", i, "_", j)
            cell_value <- grid()[i, j]
            style <- if (cell_value == 1) "background-color: darkblue;" else "background-color: white;"
            actionButton(
              id, 
              "", 
              style = paste0(
                "width: 100%;",
                "height: 100%;",
                style
              ),
              class = "grid-cell"
            )
          })
        })
      )
    }
  })
}

# Run the application 
shinyApp( ui, server)


 
