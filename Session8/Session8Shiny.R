#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Ash Sycamore Data from Anglesey (North Wales)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("species", "Species:",
                    c("All" = "all",
                      "Ash" = "1",
                      "Sycamore" = "2",
                      "Others" = "others")), 
        selectInput("variable", "Variable:",
                    c("DBH" = "dbh",
                      "Height" = "height")), 
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     dataPath <- "/Users/arng0001/Dropbox/Gwaith/Cyfrifiaduro/DropIn/Sessions/Session8/"
     myData <- read.table(paste(dataPath, "Plot1.txt", sep = ""), header = TRUE) 
     main.caption = ""
     # Species
     selectData <- myData
     if(input$species == "1") { 
       selectData <- myData[myData$Species == 1,]
     }
     else if(input$species == "2") { 
       selectData <- myData[myData$Species == 2,]
     }
     else if(input$species == "all") { 
       selectData <- myData
     }
     else if(input$species == "others") { 
       selectData <- myData[myData$Species > 2,]
     }
     # Quantitative attributes
     if(input$variable == "dbh") { 
      x <- selectData$dbh
      main.caption = "Histogram of DBH"
      x.lab = "DBH [cm]"
     }
     else {
      x <- selectData$height 
      main.caption = "Histogram of Height"
      x.lab = "Height [m]"
     }
     # Generate bins based on input$bins from ui.R
     bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # Draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white', main = main.caption, xlab = x.lab)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

