options(rgl.useNULL=TRUE)
require(SPARQL)
library(shiny)
require(DT)
require(Morpho)
## require(RDFBonesQuery)
require(plotrix)
options(shiny.maxRequestSize = -1)

RDFBonesPrefix <-  c("rdf"      ,"<http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
"rdfs"     ,"<http://www.w3.org/2000/01/rdf-schema#>",
"xsd"      ,"<http://www.w3.org/2001/XMLSchema#>",
"owl"      ,"<http://www.w3.org/2002/07/owl#>",
"swrl"     ,"<http://www.w3.org/2003/11/swrl#>",
"swrlb"    ,"<http://www.w3.org/2003/11/swrlb#>",
"vitro"    ,"<http://vitro.mannlib.cornell.edu/ns/vitro/0.7#>",
"bibo"     ,"<http://purl.org/ontology/bibo/>",
"cidoc-crm", "<http://www.cidoc-crm.org/cidoc-crm/>",
"c4o"      ,"<http://purl.org/spar/c4o/>",
"cito"     ,"<http://purl.org/spar/cito/>",
"dc"       ,"<http://purl.org/dc/terms/>",
"event"    ,"<http://purl.org/NET/c4dm/event.owl#>",
"fabio"    ,"<http://purl.org/spar/fabio/>",
"obo-fma"  ,"<http://purl.obolibrary.org/obo/fma#>",
"foaf"     ,"<http://xmlns.com/foaf/0.1/>",
"p1"       ,"<http://w3id.org/rdfbones/extensions/FrSexEst#>",
"geo"      ,"<http://aims.fao.org/aos/geopolitical.owl#>",
"obo"      ,"<http://purl.obolibrary.org/obo/>",
"ocrer"    ,"<http://purl.org/net/OCRe/research.owl#>",
"ocresd"   ,"<http://purl.org/net/OCRe/study_design.owl#>",
"obo-fma-patch" ,"<http://w3id.org/rdfbones/patches/obo-fma-patch#>",
"rdfbones", "<http://w3id.org/rdfbones/core#>",
"skos"     ,"<http://www.w3.org/2004/02/skos/core#>",
"vcard"    ,"<http://www.w3.org/2006/vcard/ns#>",
"vitro-public" ,"<http://vitro.mannlib.cornell.edu/ns/vitro/public#>",
"vivo"     ,"<http://vivoweb.org/ontology/core#>",
"scires"   ,"<http://vivoweb.org/ontology/scientific-research#>"
)





RDFBonesPrefixString <- "PREFIX rdf:      <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:     <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd:      <http://www.w3.org/2001/XMLSchema#>
PREFIX owl:      <http://www.w3.org/2002/07/owl#>
PREFIX swrl:     <http://www.w3.org/2003/11/swrl#>
PREFIX swrlb:    <http://www.w3.org/2003/11/swrlb#>
PREFIX vitro:    <http://vitro.mannlib.cornell.edu/ns/vitro/0.7#>
PREFIX bibo:     <http://purl.org/ontology/bibo/>
PREFIX cidoc-crm: <http://www.cidoc-crm.org/cidoc-crm/>
PREFIX c4o:      <http://purl.org/spar/c4o/>
PREFIX cito:     <http://purl.org/spar/cito/>
PREFIX dc:       <http://purl.org/dc/terms/>
PREFIX event:    <http://purl.org/NET/c4dm/event.owl#>
PREFIX fabio:    <http://purl.org/spar/fabio/>
PREFIX obo-fma:  <http://purl.obolibrary.org/obo/fma#>
PREFIX foaf:     <http://xmlns.com/foaf/0.1/>
PREFIX p1:       <http://w3id.org/rdfbones/extensions/FrSexEst#>
PREFIX geo:      <http://aims.fao.org/aos/geopolitical.owl#>
PREFIX obo:      <http://purl.obolibrary.org/obo/>
PREFIX ocrer:    <http://purl.org/net/OCRe/research.owl#>
PREFIX ocresd:   <http://purl.org/net/OCRe/study_design.owl#>
PREFIX obo-fma-patch: <http://w3id.org/rdfbones/patches/obo-fma-patch#>
PREFIX rdfbones: <http://w3id.org/rdfbones/core#>
PREFIX skos:     <http://www.w3.org/2004/02/skos/core#>
PREFIX vcard:    <http://www.w3.org/2006/vcard/ns#>
PREFIX vitro-public: <http://vitro.mannlib.cornell.edu/ns/vitro/public#>
PREFIX vivo:     <http://vivoweb.org/ontology/core#>
PREFIX scires:   <http://vivoweb.org/ontology/scientific-research#>\n"


runQuery <- function() {
   

    querySI <-  "SELECT DISTINCT ?SkeletalInventory ?SIlabel 
                WHERE{
                   ?SkeletalInventory a rdfbones:SkeletalInventory .
                   ?SkeletalInventory rdfs:label ?SIlabel .
                }"

    querysex<- "PREFIX rdfbones: <http://w3id.org/rdfbones/core#> 
               PREFIX frsexest: <http://w3id.org/rdfbones/extensions/FrSexEst#>
               PREFIX frageest: <http://w3id.org/rdfbones/extensions/FrAgeEst#>

               PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
               PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
               PREFIX obo: <http://purl.obolibrary.org/obo/>
               SELECT DISTINCT ?mylabel ?sex WHERE {
               ?subject obo:BFO_0000051 ?myconc .
               ?myconc rdf:type ?mytype .
               ?subject rdfs:label ?mylabel .
               ?mytype rdfs:subClassOf obo:OBI_0000338 .
               ##?myconc 	obo:OBI_0000299 ?specifiedo .
  	       ## ?specifiedo frsexest:HasText ?sex .
               ?myconc 	obo:OBI_0000293 ?specifiedisex .
               ?specifiedisex rdf:type frsexest:DegreeOfSexualization.
               ?specifiedisex obo:IAO_0000004 ?sex .
               }"

    queryage <- "PREFIX rdfbones: <http://w3id.org/rdfbones/core#> 
               PREFIX frageest: <http://w3id.org/rdfbones/extensions/FrAgeEst#>
               PREFIX frsexest: <http://w3id.org/rdfbones/extensions/FrSexEst#>
               PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
               PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
               PREFIX obo: <http://purl.obolibrary.org/obo/>
               SELECT DISTINCT ?mylabel  ?minage ?maxage WHERE {
               ?subject obo:BFO_0000051 ?myconc.
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

    ## run queries
    querySI <- paste(RDFBonesPrefixString,querySI)
    mytableSI <- SPARQL(url="http://rdfbonesdemo.anthropologie.uni-freiburg.de:2020/ds/query",query=querySI,ns=RDFBonesPrefix)$results
    querysex <- paste(RDFBonesPrefixString,querysex)
    mytablesex <- SPARQL(url="http://rdfbonesdemo.anthropologie.uni-freiburg.de:2020/ds/query",query=querysex,ns=RDFBonesPrefix)$results
    queryage <- paste(RDFBonesPrefixString,queryage)
    mytableage <- SPARQL(url="http://rdfbonesdemo.anthropologie.uni-freiburg.de:2020/ds/query",query=queryage,ns=RDFBonesPrefix)$results

    ## concatenate results
    mytableSI$SIlabel <- gsub("@.*","",mytableSI$SIlabel)
    mytableSI$SIlabel <- gsub("\"","",mytableSI$SIlabel)
    mytable <- data.frame(ID=(mytableSI$SIlabel))
    mytable$sex <- NA
    for (i in 1:length(mytablesex[[1]])) {
        tab <- grep(mytablesex[[1]][i],mytable$ID)
        if (length(tab)) {
            mytable$sex[tab] <- mytablesex$sex[i]
            
        }
    }
    mytable$minage <- NA
    mytable$maxage <- NA
    for (i in 1:length(mytableage[[1]])) {
        tab <- grep(mytableage[[1]][i],mytable$ID)
        if (length(tab)) {
            mytable$maxage[tab[1]] <- mytableage$maxage[i]
            mytable$minage[tab[1]] <- mytableage$minage[i]
        }
    }
    ## sort table by ID
    mytable <- mytable[order(mytable$ID),]
    
    ## format sexing from scores
    sexraw <- mytable$sex
    mytable$sex <- cut(mytable$sex,breaks=c(-Inf,-1.5,-0.5,0.5,1.5,Inf),labels=c("hypermasculine","masculine","indifferent","feminine","hyperfeminine"))
    
    return(list(mytable=mytable,sex=mytablesex,SI=mytableSI,age=mytableage,sexraw=sexraw))
}

shinyServer(function(input, output) {
    withProgress(message = 'Running SPARQL query', value = 0, {
        myresult <- runQuery()
    })
    mytable <- myresult$mytable
    output$mytable = DT::renderDataTable({
        mytable
    },rownames=F,filter = 'top')
    output$distPlot <- renderPlot({
        x    <- mytable$minage
                                        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x,  col = "#75AADB", border = "white",
         xlab = "Age",
         main = "Distribution of Minimum Age")

    })
     output$distPlotMax <- renderPlot({
        x    <- mytable$maxage
                                        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x,  col = "#75AADB", border = "white",
         xlab = "Age",
         main = "Distribution of Maximum Age")

    })
    output$sexPlot <- renderPlot({
        #x    <- mytable$minage
        sex <- mytable$sex
                                        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        ## histGroup(x, sex, main = "Distribution of Age according to Sex")
        xlen <- tapply(sex,sex,length)
        pie3D(xlen,labels=names(xlen),theta=1.2,explode = 0.1,main="Sex Distribution",labelcex=0.7)
    })
    output$agebysexPlot <- renderPlot({
        #x    <- mytable$minage
        sexraw <- myresult$sexraw
        sex <- cut(sexraw,breaks=c(-Inf,-0.5,0.5,Inf),labels=c("masculine","indifferent","feminine"))
        histGroup(mytable$minage,sex,"Minimum Age by Sex",xlab="Minimum Age")
    })
     output$maxagebysexPlot <- renderPlot({
        #x    <- mytable$minage
        sexraw <- myresult$sexraw
        sex <- cut(sexraw,breaks=c(-Inf,-0.5,0.5,Inf),labels=c("masculine","indifferent","feminine"))
        histGroup(mytable$maxage,sex,"Maximum Age by Sex",xlab="Maximum Age")
    })
})












