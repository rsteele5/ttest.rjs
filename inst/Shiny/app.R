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
    titlePanel("ttest.rjs Package Demo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons("type", "Choose a Test type:",
                         c("T-test" = "ttest",
                           "Welch" = "welch",
                           "Paired" = "paired")),
            sliderInput("size", "Sample Size:", 10, 40, 25, 5)
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
        set.seed(32);x=rnorm(input$size,mean=10,sd=15)

        if(input$type == "ttest"){
          set.seed(35);y=rnorm(input$size,mean=8,sd=15)
          t_test = Rttest(x,y,alpha=0.05,paired=FALSE)
          plot(t_test)
        }
        else if(input$type == "welch"){
          set.seed(35);y=rnorm(input$size,mean=8,sd=5)
          welch=Rttest(x,y,alpha=0.05,paired=FALSE)
          plot(welch)
        }
        else {
          set.seed(35);y = x+ rnorm(input$size, 5 ,4)
          paired=Rttest(x,y,alpha=0.05,paired=TRUE)
          plot(paired)
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
