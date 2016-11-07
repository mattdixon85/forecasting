library(shiny)
library(datasets)
library(caret)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(    
  
  # Give the page a title
  titlePanel("Telephones by region"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("period", "Period:", choices=c(7,28,365)),
      sliderInput("k", "K:", min=1, max=10, value=5),
      #hr(),
      helpText("Data from AT&T (1961) The World's Telephones.")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("plot1")  
    )
    
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  kpis <- read.csv('c:/users/matt/documents/kaggle/forecasting/kpis.tsv', sep='\t')
  kpis$dt <- strptime(kpis$date,'%Y-%m-%d')

  kpis$ordinal <- 1:20
  
  # Fill in the spot we created for a plot
  output$plot1 <- renderPlot({
    
    period <- as.numeric(input$period)
    kpis$sinx <- sin(kpis$ordinal %% period / period * 2 * pi)
    kpis$cosx <- cos(kpis$ordinal %% period / period * 2 * pi)
    
    trainx <- subset(kpis[1:10,], select=c('sinx','cosx'))
    validx <- subset(kpis[11:20,], select=c('sinx','cosx'))
    
    trainy <- kpis[1:10,]$foo
    validy <- kpis[11:20,]$foo
    
    fit <- knnreg(trainx, trainy, k = input$k)
    pred = predict(fit, validx)
    
    # Render plot
    plot(x=kpis$dt, y=kpis$foo)
    lines(x=kpis$dt[11:20], y=pred, col='red')
  })
})

shinyApp(ui = ui, server = server)