library(shiny)

ui <- fluidPage(
  titlePanel("Matrice de Boutons avec Shiny"),
  uiOutput("matrice_boutons")
)

server <- function(input, output) {
  output$matrice_boutons <- renderUI({
    nRows <- 5    # Définir le nombre de lignes
    nCols <- 5    # Définir le nombre de colonnes
    
    # Créer une matrice de boutons
    boutons <- lapply(1:(nRows * nCols), function(i) {
      actionButton(inputId = paste("bouton", i, sep = "_"),label = " ", style='padding:20px; font-size:80%')
    })
    
    # Matrice de variables réactives pour stocker le statut de chaque bouton
    statuts <- reactiveVal(matrix(0, nrow = 5, ncol = 5))
    
    # Organiser les boutons en grille
    tagList(div(class = "btn-grid",
                lapply(split(boutons, ceiling(seq_along(boutons) / nCols)), div, class = "button-row")))
    
    # Mettre à jour le statut lorsque chaque bouton est pressé
    observeEvent({
      lapply(1:5, function(i) {
        lapply(1:5, function(j) {
          input[[paste0("bouton_", (i-1)*5+j)]]
        })
      })
    }, {
      statut_temp <- statuts()
      lapply(1:5, function(i) {
        lapply(1:5, function(j) {
          if (input[[paste0("bouton_", (i-1)*5+j)]] > statut_temp[i, j]) {
            statut_temp[i, j] <- input[[paste0("bouton_", (i-1)*5+j)]]
          }
        })
      })
      statuts(statut_temp)
    })
    
    # Affichage du statut des boutons
    output$statut_boutons <- renderText({
      paste("Statut des boutons : ", as.vector(statuts()), collapse = ", ")
    })
  })
}

shinyApp(ui = ui, server = server)