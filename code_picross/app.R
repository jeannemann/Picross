library(shiny)

ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Grille de PICROSS"),
  
  # Use fixed layout
  tags$head(tags$script(HTML(
    '
        $(document).on("click", ".grid-cell", function() {
          var cell = $(this);
          if (cell.hasClass("darkblue")) {
            cell.removeClass("darkblue");
            cell.css("background-color", "white");
          } else {
            cell.addClass("darkblue");
            cell.css("background-color", "darkblue");
          }
        });
        '))),
  
  # Utilisation de fluidRow pour disposer les éléments côte à côte
  fluidRow(
    # Panneau latéral avec une entrée de sélection pour la taille de la grille
    column(
      width = 3,
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
                    selected = "10x10"),
        style = "width: 200px;"
      )
    ),
    
    # Affichage des indices au-dessus de la grille
    column(
      width = 9,
      fluidRow(
        uiOutput("indices"),
        style = "margin-top: 20px;"
      ),
      fluidRow(
        uiOutput("grid"),
        style = "margin-top: 20px;"
      )
    )
  )
)

# Define server logic to generate grid
server <- function(input, output, session) {
  
  # Function to count black squares in Picross
  consecutiveCounts <- function(vector) {
    runs <- rle(vector == 1)
    counts <- runs$lengths[runs$values == TRUE]
    return(counts)
  }
  
  # Function to generate random Picross grid
  grille_aleatoire <- function(dim) {
    grid <- matrix(sample(c(0, 1), dim[1] * dim[2], replace = TRUE, prob = c(0.6, 0.4)), nrow = dim[1], ncol = dim[2])
    return(grid)
  }
  
  # Create grid
  grid <- reactiveVal(NULL)
  counts <- reactiveVal(NULL)  # Stocker les indices pour chaque colonne
  
  observeEvent(input$grid_size, {
    dim <- as.numeric(unlist(strsplit(input$grid_size, "x")))
    grid(grille_aleatoire(dim))
  })
  
  observe({
    # Calculer les indices pour chaque colonne et chaque ligne de la grille
    if (!is.null(grid())) {
      rows <- nrow(grid())
      cols <- ncol(grid())
      
      # Calculer les indices pour les colonnes
      counts_cols <- lapply(1:cols, function(j) {
        consecutiveCounts(grid()[, j])
      })
      
      # Calculer les indices pour les lignes
      counts_rows <- lapply(1:rows, function(i) {
        consecutiveCounts(grid()[i, ])
      })
      
      # Mettre à jour les réactifs
      counts_list <- list(cols = counts_cols, rows = counts_rows)
      counts(counts_list)
    }
  })
  
  # Afficher les indices
  output$indices <- renderUI({
    if (!is.null(counts())) {
      cols <- length(counts()$cols)
      rows <- length(counts()$rows)
      
      div(
        style = paste0(
          "display: grid;",
          "grid-template-columns: repeat(", cols, ", 30px);",
          "grid-gap: 1px;"
        ),
        lapply(1:cols, function(j) {
          div(
            style = paste0(
              "display: grid;",
              "grid-template-rows: repeat(", length(counts()$cols[[j]]), ", 30px);",
              "grid-gap: 1px;"
            ),
            lapply(counts()$cols[[j]], function(count) {
              div(
                count,
                style = "text-align: center;"
              )
            })
          )
        }),
        div(
          style = paste0(
            "display: grid;",
            "grid-template-rows: repeat(", rows, ", 30px);",
            "grid-gap: 1px;"
          ),
          lapply(1:rows, function(i) {
            div(
              style = paste0(
                "display: grid;",
                "grid-template-columns: repeat(", length(counts()$rows[[i]]), ", 30px);",
                "grid-gap: 1px;"
              ),
              lapply(counts()$rows[[i]], function(count) {
                div(
                  count,
                  style = "text-align: center;"
                )
              })
            )
          })
        )
      )
    }
  })
  
  # Afficher la grille
  output$grid <- renderUI({
    if (!is.null(grid())) {
      dim <- dim(grid())
      
      div(
        style = paste0(
          "display: grid;",
          "grid-template-columns: repeat(", dim[2], ", 30px);",  # Utiliser une largeur fixe en pixels pour chaque colonne de la grille
          "grid-template-rows: repeat(", dim[1], ", 30px);",     # Utiliser une hauteur fixe en pixels pour chaque ligne de la grille
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
shinyApp(ui, server)