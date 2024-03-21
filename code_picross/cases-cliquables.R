library(shiny)


ui <- fluidPage(
  shinyjs::useShinyjs(),  # Utiliser l'extension ShinyJS
  titlePanel("Grille de PICROSS"),
  sidebarLayout(
    sidebarPanel(
      selectInput("m_size",
                  "Grid Size:",
                  choices = c("5x5" = "5x5",
                              "10x10" = "10x10",
                              "15x15" = "15x15",
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



# Définit une fonction pour créer une matrice de 0 et de 1

creatematrix <- function(i, j){
  matrix(sample(c(0, 1), i*j, replace = TRUE, prob = c(0.8,0.2)), nrow = i, ncol = j)
}
creatematrix(m_size[1],m_size[2])

shinyApp(ui,server)
