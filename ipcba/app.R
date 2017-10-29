library(shiny)
ipcba <- read.csv("../datasets/IPCBA_.csv", stringsAsFactors=FALSE)

# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("ipcBA"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Selector for choosing year ----
      selectInput(inputId = "year",
                  label = "Choose a year:",
                  choices = c(2013:2017)),

      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 12)
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),

      # Output: HTML table with requested number of observations ----
      tableOutput("view"),

      # Output: Histogram ----
       plotOutput("distPlot")

    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  # Return the requested dataset ----
  datasetInput <- reactive({ipcba[which(ipcba$anno==input$year),]})

  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })

  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })

  output$distPlot <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = 30)

    barplot(ipcba[which(ipcba$anno==input$year),]$mensual, col = "#75AADB",
         xlab = "% mensual",
         main = "ipcBA",
         ylim = c(-2,40),
         names = ipcba[which(ipcba$anno==input$year),]$mes)
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
1