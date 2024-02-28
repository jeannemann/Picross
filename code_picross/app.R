library(shiny)

# Define UI for application that draws a grid of black and white squares
ui <- fluidPage(
  
  # Application title
  titlePanel("Grille de PICROSS"),
  
  # Use fixed layout
  tags$head(tags$style(HTML("
        body, .container-fluid {
            max-width: 950px; /* adjust this value to your desired maximum width */
            width: 950px !important;
        }
    "))),
  
  # Sidebar with a select input for grid size
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
    
    # Show the grid of black and white squares
    mainPanel(
      plotOutput("gridPlot", height = "600px")  # Adjust height to make the grid bigger
    )
  )
)

# Define server logic required to draw the grid
server <- function(input, output) {
  
  output$gridPlot <- renderPlot({
    # Extract grid dimensions from input$grid_size
    dim <- as.numeric(unlist(strsplit(input$grid_size, "x")))
    
    # Generate grid based on dimensions
    grid <- matrix(sample(c(0, 1), dim[1] * dim[2], replace = TRUE), nrow = dim[1], ncol = dim[2])
    
    # Calculate number of blue squares in each row and column
    row_sums <- rowSums(grid)
    col_sums <- colSums(grid)
    
    # Set up plot area
    plot_range <- max(dim) + 1
    par(mar = c(3, 3, 1, 1))
    plot(0, 0, xlim = c(-0.5, plot_range), ylim = c(-0.5, plot_range), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    
    # Plot the grid of black and white squares
    for (i in 1:dim[1]) {
      for (j in 1:dim[2]) {
        if (grid[i, j] == 1) {
          rect(j - 1, dim[1] - i, j, dim[1] - i + 1, col = "darkblue", border = "black")
        } else {
          rect(j - 1, dim[1] - i, j, dim[1] - i + 1, col = "white", border = "black")
        }
      }
    }
    
    # Add labels for consecutive filled squares in rows
    for (i in 1:dim[1]) {
      consecutive_counts <- consecutiveCounts(grid[i, ])
      text(-0.5, dim[1] - i + 0.5, paste(consecutive_counts, collapse = ' '), pos = 4)
    }
    
    # Add labels for consecutive filled squares in columns
    for (j in 1:dim[2]) {
      consecutive_counts <- consecutiveCounts(grid[, j])
      text(j - 0.5, dim[1] + 0.5, paste(consecutive_counts, collapse = ' '), pos = 3)
    }
  })
  
  # Function to count consecutive filled squares
  consecutiveCounts <- function(vector) {
    runs <- rle(vector == 1)
    counts <- runs$lengths[runs$values == TRUE]
    return(counts)
  }
}

# Run the application 
shinyApp(ui = ui, server = server)
