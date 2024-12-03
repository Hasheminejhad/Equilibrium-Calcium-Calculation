library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Calculate Ca_eq for Cation Data"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("Na", "Sodium (Na) in meq/L:", value = 1.0, min = 0),
      numericInput("Ca", "Calcium (Ca) in meq/L:", value = 1.0, min = 0),
      numericInput("Mg", "Magnesium (Mg) in meq/L:", value = 1.0, min = 0),
      numericInput("HCO3", "Bicarbonate (HCO₃) in meq/L:", value = 3.0, min = 0),
      actionButton("calculate", "Calculate Ca_eq")
    ),
    
    mainPanel(
      tableOutput("results")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Function to calculate SC (sum of cations in meq/L)
  calculate_SC <- function(Na, Ca, Mg) {
    SC <- Na + Ca + Mg
    return(SC)
  }
  
  # Function to calculate Is (Ionic Strength)
  calculate_Is <- function(SC) {
    Is <- (1.3477 * SC * 0.5355) / 1000
    return(Is)
  }
  
  # Function to calculate log(X)
  calculate_logX <- function(Is, Ca, HCO3) {
    logX <- (1 / 3) * (
      4.6629 +
        (0.6103 * log10(Is)) +
        (0.0844 * (log10(Is)^2)) +
        (2 * log10(Ca / (2 * HCO3)))
    )
    return(logX)
  }
  
  # Function to calculate Ca_eq
  calculate_caeq <- function(Na, Ca, Mg, HCO3, PCO2 = 0.0007) {
    # Calculate SC
    SC <- calculate_SC(Na, Ca, Mg)
    # Calculate Is
    Is <- calculate_Is(SC)
    # Calculate log(X)
    logX <- calculate_logX(Is, Ca, HCO3)
    # Calculate Ca_eq
    Ca_eq <- 2 * 10^(logX * (PCO2^(1/3)))
    return(Ca_eq)
  }
  
  # Reactive to calculate results
  results <- eventReactive(input$calculate, {
    req(input$Na, input$Ca, input$Mg, input$HCO3)
    
    # Calculate Ca_eq
    Ca_eq <- calculate_caeq(input$Na, input$Ca, input$Mg, input$HCO3)
    
    # Return results as a data frame for display
    data.frame(
      Parameter = c("Na", "Ca", "Mg", "HCO₃", "Calculated Ca_eq"),
      Value = c(input$Na, input$Ca, input$Mg, input$HCO3, round(Ca_eq, 4)),
      Unit = c("meq/L", "meq/L", "meq/L", "meq/L", "meq/L")
    )
  })
  
  # Render the results table
  output$results <- renderTable({
    req(results())
    results()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
