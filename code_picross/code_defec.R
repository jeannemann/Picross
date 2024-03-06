library(shiny)
library(shinyjs)

# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Grille de PICROSS"),
  tags$head(tags$style(HTML("
        body, .container-fluid {
            max-width: 950px;
            width: 950px !important;
        }
        .grid-button {
            width: 40px;
            height: 40px;
            margin: 2px;
        }
        .white-button {
            background-color: white;
            color: white; /* Assurez-vous que la couleur du texte est blanche pour le bouton blanc */
        }
        .blue-button {
            background-color: darkblue;
            color: white;
        }
        .grey-button {
            background-color: grey;
            color: white;
        }
  "))),
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
                  selected = "10x10")
    ),
    mainPanel(
      uiOutput("gridUI")
    )
  )
)

# Définir la logique du serveur
server <- function(input, output, session) {
  rv <- reactiveVal()
  current_value <- rv()
  
  observe({
    dim <- as.numeric(unlist(strsplit(input$grid_size, "x")))
    rv(matrix(0, nrow = dim[1], ncol = dim[2]))
  })
  
  output$gridUI <- renderUI({
    dim <- as.numeric(unlist(strsplit(input$grid_size, "x")))
    
    buttons <- matrix(
      lapply(seq_len(prod(dim)), function(i) {
        buttonId <- sprintf("button_%d_%d", (i - 1) %/% dim[2] + 1, (i - 1) %% dim[2] + 1)
        actionButton(buttonId, label = "", style = "width: 40px; height: 40px; margin: 2px;", class = "btn-white")
      }),
      nrow = dim[1],
      ncol = dim[2],
      byrow = TRUE
    )
    
    do.call(tagList, buttons)
  })
  
  clickCount <- reactiveVal(0)
  
  observe({
    btnId <- names(input)[grepl("button", names(input))]
    if (!is.null(btnId)) {
      btnRowCol <- as.numeric(unlist(strsplit(sub("button_", "", btnId), "_")))
      btnValue <- input[[btnId]]
      btnColor <- c("white", "darkblue", "grey")[btnValue %% 3 + 1]
      
      updateActionButton(session, btnId, label = "", style = sprintf("width: 40px; height: 40px; margin: 2px; background-color: %s; color: white;", btnColor))
      
      rv()[btnRowCol[1], btnRowCol[2]] <- btnValue
    }
  })
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)
