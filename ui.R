options(shiny.maxRequestSize = -1)
options(rgl.useNULL=TRUE)


library(shiny)
shinyUI(shinyUI(fluidPage(
   
    title=("Skeletal Inventories"),
    h1("Skeletal Inventories",align="center"),
    fluidRow(
        column(12, align="center",
               DT::dataTableOutput("mytable")
               ),
        hr(),
        h2("Age Distribution",align="center"),
        column(6,h4("",align="center"),plotOutput(outputId = "distPlot")), 
        column(6,h4(""),plotOutput(outputId = "distPlotMax")),
        br(),
        h2("Sex Distribution",align="center"),
        column(12,h4(""),plotOutput(outputId = "sexPlot")),
        br(),
        h2("Age Distribution by Sex",align="center"),
        
        column(6,h4(""),plotOutput(outputId = "agebysexPlot")),
        column(6,h4(""),plotOutput(outputId = "maxagebysexPlot"))
    )
)
))
