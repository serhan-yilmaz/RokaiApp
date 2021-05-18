library(shiny)
library(magrittr)
library(DT)

multiChoicePicker <- function(id, label, choices, selected = choices[1]) {
  tags$div(
    class = "inline-block",
    style = "justify-content: space-between;", 
    tags$b(label),
    shinyWidgets::pickerInput(id, "", choices, selected = selected, width = "fit", inline = T)
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  title = "RoKAI App",
  tags$head(
    tags$link(rel="shortcut icon", href="favicon.png"),
    tags$meta(name="description", content="Rokai: Robust Inference of Kinase Activity using functional networks"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  verticalLayout(
    div(
      class = "panel-heading",
      #style = "margin:0px; padding:0px;",
      style = "margin-bottom:0px; padding-bottom:0px;",
      div(
        img(src='rokai_app_logo.png', align = "left", style = "height: 150px; padding-bottom:10px;")#,
        #tags$p("v2.0.0", style = "color:#A3A3A3;")
      ),
      tags$br(),
      #tags$br(style = "display: block; content: \"\"; margin-top: 16px;")
     # tags$span("", style = "font-size: 16.5px; margin-bottom:5px; padding-bottom:0px;")
    ),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #tags$div(
        tags$div(
          class = "inline-block",
          tags$b("Sample Data: ", style = "margin-right: 10px;"),
          tags$div(
            class = "inline-block", 
          #  style = "align: center",
          fluidRow(column(12, align = "left", 
          actionButton("buttonSampleData", "Load Sample Data"),
          tags$b(style = "margin-left: 4px; margin-right: 4px;"), 
          downloadButton('buttonDownloadSampleData', 'Download')
          )
          ),
          #actionButton("buttonDownloadSampleData", "Download")
          )#, 
          # fluidRow(
          #   column(12, align="left", style = "margin: 0px;", 
          #          actionButton("buttonSampleData", "Load"),
          #   tags$b(style = "margin-left: 3px; margin-right: 3px;"), 
          #   actionButton("buttonDownloadSampleData", "Download"))
          # )
        ),
        tags$hr(style = "margin:6px 0px 4px 0px;"),
        tags$div(
          fileInput("file1", "Upload Data:", accept = c(".csv")),
          tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
          tags$style(".checkbox {margin-bottom: 0px;}"),
        ),
        multiChoicePicker("refproteome", "Reference Proteome:", c("Uniprot Human", "Uniprot Mouse")),
        tags$hr(style = "margin: 8px 0px 8px 0px;"),
      #),
      # tags$div(
      #   class = "panel panel-default",
      #   style = "margin:0px; margin-bottom: 4px;",
      #   tags$div(class = "panel-heading", "Input Data"),
      #   tags$div(
      #     class = "panel-body",
      #     style = "padding-bottom:10px; padding-top:10px; margin:0px;  height: 280px;",
      #     id = "fileInput",
      #   )
      # ),
      multiChoicePicker("datanorm", "Fold Changes:", c("Raw", "Centered", "Normalized"), "Normalized"),
      multiChoicePicker("rokaiNetwork", "RoKAI Network:", c("KinaseSubstrate", "KS+PPI", "KS+PPI+SD", "KS+PPI+SD+CoEv"), "KS+PPI+SD+CoEv"),
      checkboxInput("rokaiEnabled", "Use sites in functional neighborhood", TRUE),
      tags$hr(style = "margin: 8px 0px 8px 0px;"),
      multiChoicePicker("yaxis", "Plot Y-Axis:", c("Kinase Activity", "Z-Score"))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Plot",
          plotOutput("distPlot", height = "340px"), 
          splitLayout(
            sliderInput("minnumsubs", "Minimum number of substrates", 1, 10, 3, step = 1, width = "220px"), 
            sliderInput("minzscore", "Minimum absolute z-score", 0, 2, 0, step = 0.05, width = "220px"),
            tags$div(
              downloadButton('downloadKinasePlotPNG', 'Download PNG'),
              tags$br(), 
              downloadButton('downloadKinasePlotPDF', 'Download PDF')
            )
          ), 
        )
        ,
        # tabPanel(
        #   "Home", 
        #   tags$p("Stuff"),
        #   pre(id = "console")
        # ),
        # tabPanel(
        #   "About", 
        #   tags$p("Welcome")
        # ),
        tabPanel(
          "Kinases",
          DT::dataTableOutput("kinaseTable"),
          #div(style = 'overflow: auto; max-height:450px;', div(style = "width: 96%", dataTableOutput("kinaseTable")))
          #div(style = 'overflow: auto; max-height:450px;', div(style = "width: 96%", dataTableOutput("kinaseTable")))
        ),
        tabPanel(
          "Kinase Targets",
          DT::dataTableOutput("kinasesubsTable"),
          #div(style = 'overflow: auto; max-height:450px;', div(style = "width: 96%", dataTableOutput("kinaseTable")))
          #div(style = 'overflow: auto; max-height:450px;', div(style = "width: 96%", dataTableOutput("kinaseTable")))
        )
      )
    )
  )
  )
)