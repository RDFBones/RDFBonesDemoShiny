options(rgl.useNULL=TRUE)
require(SPARQL)
library(shiny)
require(DT)
require(Morpho)
require(RDFBonesQuery)
options(shiny.maxRequestSize = -1)

runQuery <- function() {
    prefix <- RDFBonesPrefix
    query <- "SELECT ?skeletalInventoryLabel  WHERE {
 ?skeletalInventory rdf:type rdfbones:PrimarySkeletalInventoryDryBone.
 ?skeletalInventory	rdfs:label	?skeletalInventoryLabel.
 }"


    querysex<- "PREFIX rdfbones: <http://w3id.org/rdfbones/core#> 
               PREFIX frsexest: <http://w3id.org/rdfbones/extensions/FrSexEst#>
               PREFIX frageest: <http://w3id.org/rdfbones/extensions/FrAgeEst#>

               PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
               PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
               PREFIX obo: <http://purl.obolibrary.org/obo/>
               SELECT ?mylabel ?sex  WHERE {
               ?subject 	obo:BFO_0000051 ?myconc.
               ?myconc rdf:type ?mytype .
               ?subject rdfs:label ?mylabel .
               ?mytype rdfs:subClassOf obo:OBI_0000338 .
               ?myconc 	obo:OBI_0000299 ?specifiedo .
  	       ?specifiedo frsexest:HasText ?sex .
              
               

}"
    queryage <-  "PREFIX rdfbones: <http://w3id.org/rdfbones/core#> 
              PREFIX frageest: <http://w3id.org/rdfbones/extensions/FrAgeEst#>
               PREFIX frsexest: <http://w3id.org/rdfbones/extensions/FrSexEst#>
               PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
               PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
               PREFIX obo: <http://purl.obolibrary.org/obo/>
               SELECT ?mylabel  ?minage ?maxage WHERE {
               ?subject 	obo:BFO_0000051 ?myconc.
               ?myconc rdf:type ?mytype .
               ?subject rdfs:label ?mylabel .
               ?mytype rdfs:subClassOf obo:OBI_0000338 .
  	       ?myconc 	obo:OBI_0000293 ?specifiedimax .
               ?myconc 	obo:OBI_0000293 ?specifiedimin .
  	       ?specifiedimax rdf:type frageest:MaxAgeRange .
               ?specifiedimax obo:IAO_0000004 ?maxage .
               ?specifiedimin rdf:type frageest:MinAgeRange .
               ?specifiedimin obo:IAO_0000004 ?minage .
     }"

    querysex <- paste(RDFBonesPrefixString,querysex)
    mytablesex <- SPARQL(url="http://rdfbonesdemo.anthropologie.uni-freiburg.de:2020/ds/query",query=querysex,ns=RDFBonesPrefix)$results
    ## mytablesex

    queryage <- paste(RDFBonesPrefixString,queryage)
    mytableage <- SPARQL(url="http://rdfbonesdemo.anthropologie.uni-freiburg.de:2020/ds/query",query=queryage,ns=RDFBonesPrefix)$results


    mytable <- data.frame(ID=(mytablesex[[1]]),sex=mytablesex[[2]])
    mytable$minage <- NA
    mytable$maxage <- NA
    for (i in 1:length(mytableage[[1]])) {
        tab <- grep(mytableage[[1]][i],mytable$ID)
        if (length(tab)) {
            mytable$maxage[tab] <- mytableage$maxage[i]
            mytable$minage[tab] <- mytableage$minage[i]
        }
    }
    return(mytable)
}
##mytableage

##mytable <- getSkeletalInventoryLabels("http://rdfbonesdemo.anthropologie.uni-freiburg.de:2020/ds/query")
##mytable <- t(mytable)

rownames(mytable) <- NULL
##colnames(mytable) <- c("ID","Estimated Sex")
shinyServer(function(input, output) {
    mytable <- runQuery()
    output$mytable = DT::renderDataTable({
        mytable
    })
    output$distPlot <- renderPlot({
        x    <- mytable$minage
                                        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x,  col = "#75AADB", border = "white",
         xlab = "Age",
         main = "Distribution of minimum age")

    })
     output$distPlotMax <- renderPlot({
        x    <- mytable$maxage
                                        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x,  col = "#75AADB", border = "white",
         xlab = "Age",
         main = "Distribution of maximum age")

    })
    output$sexPlot <- renderPlot({
        x    <- mytable$minage
        sex <- mytable$sex
                                        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

    histGroup(x, sex,      
         main = "Distribution of Age according to Sex")

    })
})












