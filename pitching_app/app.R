# Load necessary libraries
library(shiny)
library(ggplot2)
library(catboost)

# Load the saved models
models <- readRDS("pitch_models.RDS")
ff_pitching_model <- models$ff_pitching_model
bb_pitching_model <- models$bb_pitching_model
ch_pitching_model <- models$ch_pitching_model

# Define UI
ui <- fluidPage(
  titlePanel("Pitch Grader App"),
  
  # Organize inputs in two horizontal lines
  fluidRow(
    column(4,
           selectInput("pitch_type", "Pitch Type", choices = c("Four-Seam", "Sinker", "Curveball", "Cutter", "Slider", "Changeup", "Splitter"))
    ),
    column(4,
           selectInput("batter_hand", "Batter Hand", choices = c("R", "L"))
    ),
    column(4,
           selectInput("pitcher_hand", "Pitcher Hand", choices = c("R", "L"))
    )
  ),
  
  fluidRow(
    column(3,
           numericInput("RelSpeed", "Speed", value = 90, min = 50, max = 100)
    ),
    column(3,
           numericInput("InducedVertBreak", "IVB", value = 10, min = -20, max = 20)
    ),
    column(3,
           numericInput("HorzBreak", "HB", value = 0, min = -20, max = 20)
    ),
    column(3,
           numericInput("RelSide", "Release Side", value = 0, min = -5, max = 5)
    ),
    column(3,
           numericInput("RelHeight", "Release Height", value = 6, min = 0, max = 10)
    ),
    column(3,
           numericInput("SpinRate", "Spin Rate", value = 2000, min = 0, max = 4000)
    ),
    column(3,
           numericInput("Extension", "Extension", value = 6, min = 0, max = 10)
    )
  ),
  
  mainPanel(
    plotOutput("ev_plot")
  )
)

server <- function(input, output) {
  # Reactive expression for plot data
  plot_data <- reactive({
    # Create base data frame with grid of locations
    loc <- expand.grid(PlateLocHeight = seq(2, 3, 0.05), PlateLocSide = seq(-.5, .5, 0.05))
    
    # Add numeric inputs to data frame
    loc$RelSpeed <- input$RelSpeed
    loc$InducedVertBreak <- input$InducedVertBreak
    loc$HorzBreak <- input$HorzBreak
    loc$RelSide <- input$RelSide
    loc$RelHeight <- input$RelHeight
    loc$SpinRate <- input$SpinRate
    loc$Extension <- input$Extension
    
    # Add sameHand column
    loc$sameHand <- as.factor(ifelse(input$batter_hand == input$pitcher_hand, 1, 0))
    
    # Select appropriate model based on pitch type
    pitch_model <- switch(input$pitch_type,
                          "Four-Seam" = ff_pitching_model,
                          "Sinker" = ff_pitching_model,
                          "Curveball" = bb_pitching_model,
                          "Cutter" = bb_pitching_model,
                          "Slider" = bb_pitching_model,
                          "Changeup" = ch_pitching_model,
                          "Splitter" = ch_pitching_model)
    
    # Convert data frame to pool for CatBoost prediction
    data_pool <- catboost.load_pool(data = loc)
    
    # Get prediction probabilities
    probs <- catboost.predict(pitch_model, data_pool, prediction_type = "Probability")
    
    # Calculate EV
    ev_vector <- c(.054, -.061, .103, -.039, -.056, .277, -.225, -.108)
    loc$RV <- rowSums(probs %*% ev_vector) 
    loc$Stuff <- 100 + (((loc$RV - 0.004) / 0.007)*-10)
    
    return(loc)
  })
  
  output$ev_plot <- renderPlot({
    loc <- plot_data()
    
    ggplot(loc, aes(x = PlateLocSide, y = PlateLocHeight)) +
      geom_tile(aes(fill = Stuff)) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                           midpoint = 100) +
      labs(title = "Pitch Location Heatmap",
           x = "Plate Location Side",
           y = "Plate Location Height") +
      theme_minimal() +
      coord_equal()  # This ensures the plot is square
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
