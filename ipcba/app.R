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
                  choices = c(2013:2018,"Ultimos 12 meses")),

      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   min=1, max=12,
                   value = 3)
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
  datasetInput <- reactive({
    ipcba <- ipcba[which(ipcba$anno==input$year),]
    tt <- nrow(ipcba)
    ipcba["ipcba_ant"] <- ipcba[["ipcba"]]/(1 + ipcba[["mensual"]] / 100)
    ipcba["ipcba_ini"] <- rep(ipcba[1,"ipcba_ant"],tt)
    ipcba["ipcba_acu"] <- (ipcba[["ipcba"]] - ipcba[["ipcba_ini"]]) / ipcba[["ipcba_ini"]] * 100
    ipcba[["mensual"]]
   })

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

    #x    <- faithful$waiting
    #bins <- seq(min(x), max(x), length.out = 30)
    if(input$year == "Ultimos 12 meses"){
      ipcba <- tail(ipcba[ !is.na(ipcba[["ipcba"]]), ], n=12L)
    }else{
      ipcba <- ipcba[which(ipcba$anno==input$year),]
    }
    tt <- nrow(ipcba)
    ipcba["ipcba_ant"] <- ipcba[["ipcba"]]/(1 + ipcba[["mensual"]] / 100)
    ipcba["ipcba_ini"] <- rep(ipcba[1,"ipcba_ant"],tt)
    ipcba["ipcba_acu"] <- (ipcba[["ipcba"]] - ipcba[["ipcba_ini"]]) / ipcba[["ipcba_ini"]] * 100
    maxylim = max(c(max(ipcba$mensual, na.rm = TRUE), max(ipcba[["ipcba_acu"]], na.rm = TRUE))) * 1.3
    minylim = min(c(min(ipcba$mensual, na.rm = TRUE), min(ipcba[["ipcba_acu"]], na.rm = TRUE)))
    if (minylim > 0) minylim <- 0

    plot(ipcba$mensual, col = "#75AADB",
         xlab = "",
         ylab = "% mensual y acumulado",
         main = paste("ipcBA ", input$year, "\n"),
         ylim = c(minylim,maxylim),
         type="h", lwd=35, lend=1, axes=FALSE)
    par(new=TRUE)
    plot(ipcba$ipcba_acu,
         ylim = c(minylim,maxylim),
         ylab="", xlab="", axes=FALSE, type="l", lwd=5, col = "#FF0000")
    grid()
    text(c(1:length(ipcba$mensual)),1,ipcba$mensual, cex = 1.2)
    text(c(1:12),ipcba$ipcba_acu+2,round(ipcba$ipcba_acu, digits=1), cex = 1.2)
    axis(1, at=1:length(ipcba[["mes"]]) , labels=substr(ipcba[["mes"]],1,3))
    axis(1, at=1:length(ipcba[["anno"]]) , labels=substr(ipcba[["anno"]],1,4), outer=FALSE, tick = FALSE, line = 2)
    #axis(2, at=1:42 , labels=rep("",42), lty = 3)
    axis(3, at=1:length(ipcba[["anno"]]) , labels=rep("",length(ipcba[["anno"]])), lty = 3)
    #axis(4, at=1:42 , labels=rep("",42), lty = 3)
    par(adj = 0)
    title(sub = "Fuente: DGEyC")
    par(adj = 0.5)
    box(lty=1)
    legend(1, maxylim, c("Mensual","Acumulado"),  col=c("#75AADB","red"), lty=c(1,1), lwd = c(8,8), bg = "#FFFFFF")
  }, width=700, height= 500)

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

    
