# server.R
library(shiny)
#library(gplots)
library(edgeR)

shinyServer(function(input, output){
    
    datasetInput <- reactive({
    filename<-input$file
    if(is.null(filename))
    return(NULL)
    dataset<-as.matrix(read.csv(filename$datapath,header=T,row.names=1))
    dataset<-dataset[,c(as.numeric(unlist(strsplit(input$control.index,","))) , as.numeric(unlist(strsplit(input$treatment.index,","))))]
    g<-c(rep(0,length(unlist(strsplit(input$control.index,",")))),rep(1,length(unlist(strsplit(input$treatment.index,",")))))
    
    libSizes<-as.vector(colSums(dataset))
    d <- DGEList(counts=dataset,group=g,lib.size=libSizes)
    d <- calcNormFactors(d)
    d <- estimateCommonDisp(d)
    d <- estimateTagwiseDisp(d)
    de.com <- exactTest(d)
    summary(de <- decideTestsDGE(de.com, adjust.method="fdr", p.value=as.numeric(input$FDR.value), lfc=as.numeric(input$logFC)))
    detags<-rownames(dataset)[as.logical(de)]
    results <- topTags(de.com,n = length(dataset[,1]))
    table.ready<-results$table
    #write.table(as.matrix(results$table),file="outputexp1.txt",sep="\t")
        })
    
    

output$caption <- renderPrint({
           input$caption
})


output$preview<-renderTable({
    filename<-input$file
    if(is.null(filename))
    return(NULL)
    dataset<-as.matrix(read.csv(filename$datapath,header=T,row.names=1))
    dataset<-dataset[,c(as.numeric(unlist(strsplit(input$control.index,","))) , as.numeric(unlist(strsplit(input$treatment.index,","))))]
    head(dataset)
    
})




output$plot<-renderPlot({
    filename<-input$file
    if(is.null(filename))
    return(NULL)
    dataset<-as.matrix(read.csv(filename$datapath,header=T,row.names=1))
    dataset<-dataset[,c(as.numeric(unlist(strsplit(input$control.index,","))) , as.numeric(unlist(strsplit(input$treatment.index,","))))]
    g<-c(rep(0,length(unlist(strsplit(input$control.index,",")))),rep(1,length(unlist(strsplit(input$treatment.index,",")))))
    
    libSizes<-as.vector(colSums(dataset))
    d <- DGEList(counts=dataset,group=g,lib.size=libSizes)
    d <- calcNormFactors(d)
    d <- estimateCommonDisp(d)
    d <- estimateTagwiseDisp(d)
    de.com <- exactTest(d)
    summary(de <- decideTestsDGE(de.com, adjust.method="fdr", p.value=as.numeric(input$FDR.value), lfc=as.numeric(input$logFC)))
    detags<-rownames(dataset)[as.logical(de)]
    plotSmear(de.com, de.tags=detags)
    abline(h=c(-1*as.numeric(input$logFC),1*as.numeric(input$logFC)), col="blue")
    text(x=5,y=2,labels=paste(summary(de <- decideTestsDGE(de.com, adjust.method="fdr", p.value=as.numeric(input$FDR.value), lfc=as.numeric(input$logFC)))[3], "genes up-regulated",sep=" "))
    text(x=5,y=-2,labels=paste(summary(de <- decideTestsDGE(de.com, adjust.method="fdr", p.value=as.numeric(input$FDR.value), lfc=as.numeric(input$logFC)))[1], "genes down-regulated", sep=" "))
    
    #results <- topTags(de.com,n = length(dataset[,1]))
    #write.table(as.matrix(results$table),file="outputexp1.txt",sep="\t")

})

#output$view<-renderTable({})


output$downloadData <- downloadHandler(


filename = function() { paste(input$outputfile, '.csv', sep='') },
content = function(file) {
    write.csv(datasetInput(), file)
}
)






})