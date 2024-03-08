library(shiny)
library(shinyjs)

ui <- fluidPage(
  shinyjs::useShinyjs(),  # Utiliser l'extension ShinyJS
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
      tags$style(HTML("
          /* Ajoutez les styles CSS ici */
          .square-button {
            width: 30px;
            height: 30px;
            margin: 1px;
            background-color: white;
          }

          .matrix-container {
            display: grid;
            gap: 1px;
            max-width: 100%;
            overflow-x: auto;
            max-height: 150vh;
            overflow-y: auto;
          }
      ")),
      tags$script(HTML('
       $(document).on("shiny:connected", function() {
        shinyjs.onClickButton = function(index) {
          Shiny.setInputValue("clickedButton", index);
        };
      });

      '))
    )
  )
)

server <- function(input, output) {
  # Initialiser une matrice de statuts pour chaque bouton
  button_status <- reactiveVal(matrix(0, nrow = 1, ncol = 1))
  
  observeEvent(input$grid_size, {
    # Mettre à jour la taille de la grille lorsqu'elle change
    updateMatrix()
  })
  
  observeEvent(input$clickedButton, {
    # Mettre à jour la classe "clicked" des boutons côté serveur
    btn_id <- paste0("button", input$clickedButton)
    shinyjs::runjs(sprintf('$(".square-button").removeClass("clicked"); $("#%s").click();', btn_id))
    
    # Mettre à jour le statut du bouton dans la matrice
    button_status(matrix(1, nrow = 1, ncol = 1, dimnames = list(NULL, btn_id)))
    
    # Afficher le statut dans la console JavaScript du navigateur
    shinyjs::runjs(sprintf('console.log("Matrice de statuts mise à jour : %s");', as.character(button_status())))
  })
  
  updateMatrix <- function() {
    grid_size <- as.numeric(unlist(strsplit(input$grid_size, "x")))
    num_buttons <- grid_size[1] * grid_size[2]
    
    # Initialiser la matrice de statuts avec des zéros
    button_status(matrix(0, nrow = 1, ncol = num_buttons))
    
    button_list <- lapply(1:num_buttons, function(i) {
      actionButton(
        inputId = paste0("button", i),
        "",
        class = "btn btn-default square-button"
      )
    })
    
    cols <- grid_size[2]  # Nombre de colonnes dans la grille
    
    output$buttonMatrix <- renderUI({
      tagList(
        tags$style(HTML(sprintf("
          .matrix-container { 
            display: grid;
            grid-template-columns: repeat(%d, 30px); 
            gap: 1px; 
            max-width: 100%%; 
          }
        ", cols))),
        div(id = "matrix-container", class = "matrix-container", button_list)
      )
    })
  }
}

shinyApp(ui, server)
