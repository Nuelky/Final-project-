#
# This is a Shiny web application for the Group 4 Package

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Group4"),

    # Sidebar with a slider input for determined binary area 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Burn Area",
                        "Binary Burn Area:",
                        min = 0,
                        max = 1,
                        value = 0.5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("confusion_matrix")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$confusion_matrix <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$binary_area + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = binary_area, col = 'darkgray', border = 'white',
             main = 'Matrix of Predicted Forest Fire Incidents')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
