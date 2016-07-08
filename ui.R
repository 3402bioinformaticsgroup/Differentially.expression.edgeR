# ui.R

library(shiny)
#library(edgeR)

shinyUI(fluidPage(
 titlePanel("Differentially expression analysis"),
 sidebarLayout(
   sidebarPanel(

      fileInput('file', label = h3("File input(.csv)")),
      textInput("caption", "Caption:", "Pairwise comparison"),
      textInput("control.index", "Control Index(comma delimited):", "1,2"),
      textInput("treatment.index", "Treatment Index(comma delimited):", "1,2"),
      textInput("FDR.value", "FDR cutoff value:", "0.05"),
      textInput("logFC", "LogFC cutoff value:", "1"),
      textInput("outputfile", "Output file name","edgeR.result"),
      downloadButton('downloadData', 'Download')
#textInput("logFC.down", "LogFC cutoff value(<0):", "-1")

   ),

 mainPanel(
   h3(textOutput("caption", container = span)),
   tableOutput("preview"),
   plotOutput("plot")
#tableOutput("view")

  )



)




))


