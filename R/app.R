
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
    '
  ))),

  # Utilisation de fluidRow pour disposer les éléments côte à côte
  fluidRow(
    # Règles du Picross
    column(
      width = 12,
      h3("Règles du Picross"),
      p("Le Picross est un jeu de puzzle où vous devez remplir certaines cases d'une grille selon les indices fournis. Les nombres sur les bords de la grille indiquent le nombre de cases bleu marine consécutives dans la ligne ou la colonne correspondante."),
      p("Utilisez les indices pour déterminer quelles cases doivent être remplies de couleur. Pour remplir une case, cliquez simplement dessus. Pour la vider, cliquez à nouveau."),
      p("Quand vous pensez avoir réussi cliquez sur le bouton 'Check' pour vérifier votre solution. Bonne chance !"),
      style = "margin-top: 20px;"
    ),

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
        selectInput("difficulty",
                    "Difficulté:",
                    choices = c("Easy", "Medium", "Hard"),
                    selected = "Medium"),
        actionButton("check_button", "Check"),
        style = "width: 200px;"
      )
    ),

    # Affichage de la matrice et des indices
    column(
      width = 9,
      fluidRow(
        # Espace pour les indices des colonnes
        column(
          width = 3, # Ajout de la taille des colonnes d'indices supplémentaires
          offset = 1, # Ajout d'une marge à gauche pour les aligner
          uiOutput("col_indices"),
          style = "margin-top: 20px; text-align: center;"
        )
      ),
      fluidRow(
        # Espace pour les indices des lignes
        column(
          width = 2,
          uiOutput("row_indices"),
          style = "margin-top: 20px;"
        ),
        # Grille
        column(
          width = 10,
          uiOutput("grid"),
          style = "margin-top: 20px;"
        )
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
  # Fonction pour définir la difficulté
  set_difficulty <- function(difficulty) {
    if (input$difficulty == "Easy") {
      return(0.9)
    } else if (difficulty == "Medium") {
      return(0.7)
    } else if (difficulty == "Hard") {
      return(0.5)
    }
  }
  # Function to generate random Picross grid
  grille_aleatoire <- function(dim, density) {
    grid <- matrix(sample(c(0, 1), dim[1] * dim[2], replace = TRUE, prob = c(1 - density, density)), nrow = dim[1], ncol = dim[2])
    return(grid)
  }

  # Create grid
  grid <- reactiveVal(NULL)
  counts <- reactiveVal(NULL)  # Stocker les indices pour chaque colonne

  observeEvent(input$grid_size, {
    dim <- as.numeric(unlist(strsplit(input$grid_size, "x")))
    grid(grille_aleatoire(dim, set_difficulty(difficulty=input$difficulty)))  # Changer la densité selon vos préférences
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

  # Afficher les indices des colonnes
  output$col_indices <- renderUI({
    if (!is.null(counts())) {
      cols <- length(counts()$cols)

      div(
        style = paste0(
          "display: grid;",
          "grid-template-columns: repeat(", cols + 1, ", 30px);", # Ajout d'un pour l'espace supplémentaire
          "grid-gap: 1px;"
        ),
        lapply(1:(cols + 1), function(j) { # Ajout d'un élément dans la boucle
          if (j == 1) {
            div() # Ajout d'une chaine vide pour le premier élément
          } else {
            div(
              style = paste0(
                "display: grid;",
                "grid-template-rows: repeat(", length(counts()$cols[[j - 1]]), ", 30px);",
                "grid-gap: 1px;"
              ),
              lapply(counts()$cols[[j - 1]], function(count) {
                div(
                  count,
                  style = "text-align: center;"
                )
              })
            )
          }
        })
      )
    }
  })

  # Afficher les indices des lignes
  output$row_indices <- renderUI({
    if (!is.null(counts())) {
      rows <- length(counts()$rows)

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
    }
  })

  # Afficher la grille
  output$grid <- renderUI({
    if (!is.null(grid())) {
      dim <- dim(grid())

      div(
        style = paste0(
          "display: grid;",
          "grid-template-columns: repeat(", dim[2], ", 30px);",
          "grid-template-rows: repeat(", dim[1], ", 30px);",
          "grid-gap: 1px;"
        ),
        lapply(1:dim[1], function(i) {
          lapply(1:dim[2], function(j) {
            id <- paste0("cell_", i, "_", j)
            style <- "background-color: white;"
            button <- actionButton(
              id,
              "",
              style = paste0(
                "width: 100%;",
                "height: 100%;",
                style
              ),
              class = "grid-cell"
            )
            if (grid()[i, j] == 1) {
              button$children <- tags$style(".grid-cell { background-color: darkblue; }")
            }
            button
          })
        })
      )
    }
  })

  verification <- function() {
    if (!is.null(grid())) {
      dim <- dim(grid())
      user_grid <- matrix(0, nrow = dim[1], ncol = dim[2])

      # Remplir la grille de l'utilisateur
      for (i in 1:dim[1]) {
        for (j in 1:dim[2]) {
          user_grid[i, j] <- ifelse(class(input[[paste0("cell_", i, "_", j)]]) %in% "action-button-darkblue", 1, 0)
        }
      }

      # Vérifier les lignes
      for (i in 1:dim[1]) {
        if (!identical(user_grid[i, ], grid()[i, ])) {
          return(FALSE)
        }
      }

      # Vérifier les colonnes
      for (j in 1:dim[2]) {
        if (!identical(user_grid[, j], grid()[, j])) {
          return(FALSE)
        }
      }

      return(TRUE)
    }
  }

  # Réaction au bouton de vérification
  observeEvent(input$check_button, {
    if (verification()) {
      showModal(modalDialog(
        title = "Félicitations!",
        "Vous avez résolu le puzzle avec succès!"
      ))
    } else {
      showModal(modalDialog(
        title = "Désolé!",
        "La solution que vous avez fournie est incorrecte. Ou correct. En fait on sait pas parce que cette fonction ne marche pas. Veuillez réessayer."
      ))
    }
  })
}

#' @title Jouer
#' @description Fonction pour lancer l'application blablabla ecrire ce que ca fait
#' @author Lapi - Mannequin
#' @import shiny
#' @export

play_MJ <- function() {
  # Run the application
  shinyApp(ui, server)
}
