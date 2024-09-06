library(shiny)
library(gt)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Load data from CSV

df <- readRDS("final_data.RDS")

df_smry <- df$df_smry
df_tot <- df$df_tot

# Extract unique pitcher names for the dropdown
pitcher_names <- unique(df_smry$Pitcher)

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Pitcher Search"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("pitcher_name", 
                     "Select Pitcher's Name:",
                     choices = NULL,  # Initialize with no choices
                     options = list(maxOptions = 1000)  # Limit options shown
      ),
      
      hr()  # Horizontal bar separating input from the table
    ),
    
    mainPanel(
      gt_output("pitcher_table"),
      plotOutput("pitchPlot")  # Output for the combined plots
    )
  )
)

# Define the Shiny server logic
server <- function(input, output, session) {
  
  # Update the selectize input choices dynamically on the server
  updateSelectizeInput(session, "pitcher_name", choices = pitcher_names, server = TRUE)
  
  # Filter data based on user input
  filtered_data <- reactive({
    if (is.null(input$pitcher_name) || input$pitcher_name == "") {
      return(NULL)
    } else {
      df_smry %>% filter(Pitcher == input$pitcher_name)
    }
  })
  
  # Render the GT table
  output$pitcher_table <- render_gt({
    if (is.null(filtered_data())) {
      return(NULL)
    } else {
      filtered_data() %>%
        gt() %>%
        tab_header(title = md("**Pitcher Trackman Summary 2024**")) %>%
        cols_label(AutoPitchType = "Pitch Type",
                   pitches_thrown = "Count",
                   velo = "Velo",
                   x_rel = "HRel",
                   z_rel = "VRel",
                   hmov = "HMov",
                   vmov = "IVB",
                   spinrate = "SpinRate",
                   ext = "Extension",
                   vaa = "VAA",
                   haa = "HAA")
    }
  })
  
  # Filter plot data based on user input
  filtered_plot_data <- reactive({
    if (is.null(input$pitcher_name) || input$pitcher_name == "") {
      return(NULL)
    } else {
      df_tot %>% filter(Pitcher == input$pitcher_name)
    }
  })
  
  # Render the combined plots
  output$pitchPlot <- renderPlot({
    data <- filtered_plot_data()
    
    if (is.null(data)) {
      return(NULL)
    }
    
    # Velocity distribution plot
    velocity_plot <- ggplot(data, aes(x = Velo, fill = Pitch)) +
      geom_density(alpha = 0.7) +
      labs(title = "Pitch Velocity Distribution", x = "Velocity (mph)", y = "Density") +
      theme_minimal()
    
    # Horizontal and Vertical Movement Plot
    movement_plot <- ggplot(data, aes(x = HorzBreak, y = InducedVertBreak, color = Pitch)) +
      geom_point(size = 3) +
      labs(title = "Pitch Movement", x = "Horizontal Movement (in)", y = "Vertical Movement (in)") +
      xlim(c(-30,30)) + ylim(c(-30,30)) +
      geom_hline(yintercept=0) + geom_vline(xintercept=0)+
      theme_minimal()
    
    # Combine the two plots side by side
    gridExtra::grid.arrange(velocity_plot, movement_plot, ncol = 2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

