options(shiny.maxRequestSize = -1)
options(rgl.useNULL=TRUE)


library(shiny)
shinyUI(shinyUI(fluidPage(
    
    title=("Skeletal Inventories"),
    h1("Skeletal Inventories"),
    fluidRow(
        column(12, align="center",
               DT::dataTableOutput("mytable")
               ),
        hr(),
        column(3,h4(""),plotOutput(outputId = "distPlot")),
        column(3,h4(""),plotOutput(outputId = "distPlotMax")),
        column(6,h4(""),plotOutput(outputId = "sexPlot"))
        
    )
)
))
