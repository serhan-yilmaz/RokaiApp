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

paper_txt <- function(authors, date, title, journal, link, misc){
  tags$div(
    class = "inline", 
    style = "margin-bottom: 6px; margin-top; 6px;", 
    tags$ul(tags$li(
      tags$text(style = "font-size: medium;", paste(authors, " (", date, ") ", sep = "")),
      tags$text(style = "font-size: medium; font-style: italic;", title),
      tags$a(style = "font-size: medium;", journal, href = link),
      tags$text(style = "font-size: medium;", misc)
    ))
  )
}

desc_text <- function(qtxt){
  tags$div(
    class = "inline", 
    style = "margin-bottom: 6px; margin-top; 6px;", 
    tags$text(style = "font-size: medium;", qtxt)
  )
}

about_question <- function(qtxt, atxt, href, actLink = FALSE){
  if(actLink){
    link <- actionLink(style = "font-size: large;", href, atxt)
  } else {
    link <- tags$a(style = "font-size: large;", atxt, href=href)
  }
  
  tags$div(
    class = "inline", 
    style = "margin-bottom: 8px; margin-top; 8px;", 
    tags$text(style = "font-size: large;", qtxt),
    link
  )
}

about_tab <- function(){
  tabsetPanel(id = "aboutTabset",
    tabPanel(
      "Welcome",
      tags$div(
        class = "panel-body",
        style = "padding-bottom:5px; padding-top:2px; margin:0px;", #  height: 78px;
        #tags$p(),
        tags$h3("Welcome!", style="font-weight:bold;"),
        tags$h4(style="font-style:italic;", "RoKAI is a computational tool for inferring kinase activities in a robust manner using functional networks."),
        
        about_question("Using it first time? To help getting started, read our ", "User Manual", "https://github.com/serhan-yilmaz/RoKAI/raw/master/rokai_user_manual.pdf"),
        about_question("Prefer to run locally? Download source code at ", "Github page", "https://github.com/serhan-yilmaz/Rokai"),
        about_question("Have a quick question or need some help?", "Contact us", "contactLink", actLink = T),
        about_question("Use RoKAI in your research?", "Please cite us", "citeLink", actLink = T),
        about_question("Thank you for using RoKAI App!", "", ""),
        #tags$text(style = "font-size: large;", "Have a quick question or need some help?"), tags$a(style = "font-size: large;", "Contact us", href="#chapter4"),
        #tags$a(name = "chapter4")
        
        tags$text(style = "font-size: small", "* For sensitive and/or confidential files, we strongly encourage you to"),
        tags$a(style = "font-size: small", "run the RoKAI App locally.", href = "https://github.com/serhan-yilmaz/RokaiApp"), 
        #tags$text(style = "font-size: small", "."),
        tags$br(), #
        tags$text(style = "font-size: small", "** This tool is intended for educational or academic purposes and it comes with no warranty. See "),
        tags$a(style = "font-size: small", "license", href = "https://github.com/serhan-yilmaz/RoKAI/blob/master/LICENSE"), 
        tags$text(style = "font-size: small", "for more information.")
      )
    ),
    tabPanel(
      "Contact",
      tags$div(
        class = "panel-body",
        style = "padding-bottom:5px; padding-top:2px; margin:0px;", #  height: 78px;
        #tags$p(),
        tags$h3("Contact", style="font-weight:bold;"),
        desc_text("RoKAI is designed by Serhan Yilmaz and Mehmet Koyuturk at Case Western Reserve University."),
        desc_text("If you have any questions or feature suggestions, please contact <serhan.yilmaz@case.edu>"),
        
        tags$h3("Acknowledgement", style="font-weight:bold;"),
        desc_text("This work was supported by National Institute of Health (NIH) grant R01-LM012980 from the National Libraries of Medicine.")
      )
    ),
    tabPanel(
      "How to cite us?",
      tags$div(
        class = "panel-body",
        style = "padding-bottom:5px; padding-top:2px; margin:0px;", #  height: 78px;
        #tags$p(),
        tags$h3("How to cite us?", style="font-weight:bold;"),
        desc_text("Please cite the following paper(s) if you use RoKAI in your research:"),
        paper_txt("Yilmaz S., Ayati M., Schlatzer D., Cicek A. E., Chance M. R., Koyuturk M.", "2021", "Robust inference of kinase activity using functional networks", "Nature Communications", "https://doi.org/10.1038/s41467-021-21211-6", "12 (1117)"),
        
        desc_text("RoKAI uses the following resources for functional networks:"),
        paper_txt("Hornbeck, P. V. et al.", "2015", "Phosphositeplus, 2014: mutations, ptms and recalibrations.", "Nucleic acids research", "https://doi.org/10.1093/nar/gku1267", "43(D1), D512-D520"),
        paper_txt("Minguez, P. et al.", "2012", "PTMcode: a database of known and predicted functional associations between post-translational modifications in proteins.", "Nucleic acids research", "https://doi.org/10.1093/nar/gks1230", "41(D1), D306-D311"),
        paper_txt("Szklarczyk, D. et al.", "2014", "STRING v10: protein–protein interaction networks, integrated over the tree of life.", "Nucleic acids research", "https://doi.org/10.1093/nar/gku1003", "43(D1), D447-D452")
        
        
      )
    )
  )
}

ga_scripts <- function (){
  tags$script(HTML(
    "$(document).on('shiny:inputchanged', function(event) {
       if (event.name === 'buttonSampleData') {
         Shiny.setInputValue('foo', 'bar', {priority: 'event'});
         //ga('send', 'event', 'input', 'updates', event.name, event.value);
       }
     });
    Shiny.addCustomMessageHandler('testmsg', function(message) {
      words = message.split('-');
      main = words[0]
      details = words[1]
      //Shiny.setInputValue('foo2', details, {priority: 'event'});
      ga('send', 'event', 'upload', 'success', main, details);
    });
    "
  ))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  title = "RoKAI App",
  tags$head(
    tags$link(rel="shortcut icon", href="favicon.png"),
    tags$meta(name="description", content="RoKAI: Robust Inference of Kinase Activity using functional networks"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    includeHTML(("www/google-analytics.html")),
    ga_scripts()
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
  fluidRow(
    column(width = 4,
           tags$form(class = "well", style = "margin-bottom:8px;",
 # sidebarLayout(
#    sidebarPanel(
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
# tags$div(
#   class = "panel panel-default",
#   style = "margin:0px; margin-bottom:5px;",
#   tags$div(
#     class = "panel-body",
#     style = "padding-bottom:10px; padding-top:10px; margin:0px;", #  height: 78px;
#     #tags$p(),
#     "For questions or feature suggestions, please contact:",
#     tags$br(),
#     tags$a("Serhan Yilmaz", href="http://www.serhanyilmaz.com"),
#     "<serhan.yilmaz@case.edu>"
#   )
# ),
tags$div(
  class = "panel panel-default",
  style = "margin:0px; margin-bottom:5px;",
  tags$div(
    class = "panel-body",
    style = "padding-bottom:10px; padding-top:10px; margin:0px;", #  height: 78px;
    #tags$p(),
    "RoKAI App is recently updated. To access the older version, please visit: ",
    #tags$br(),
    tags$a("http://legacy.rokai.io", href="http://legacy.rokai.io"),
    #"<serhan.yilmaz@case.edu>"
  )
)
    ),
   # mainPanel(
    column(width = 8,
      tabsetPanel(id = "mainTabset",
        tabPanel(
          "About",
          about_tab()
        ),
        tabPanel(
          "Plot",
          plotOutput("distPlot", height = "340px"), 
          splitLayout(
            sliderInput("minnumsubs", "Minimum number of substrates", 1, 10, 3, step = 1, width = "220px"), 
            sliderInput("minzscore", "Minimum absolute z-score", 0, 2, 1, step = 0.05, width = "220px"),
            tags$div(
              downloadButton('downloadKinasePlotPNG', 'Download PNG'),
              tags$br(), 
              downloadButton('downloadKinasePlotPDF', 'Download PDF')
            )
          ), 
        )
        ,
        tabPanel(id = "xyz", 
          "Kinases",
          DT::dataTableOutput("kinaseTable"),
          #div(style = 'overflow: auto; max-height:450px;', div(style = "width: 96%", dataTableOutput("kinaseTable")))
        ),
        tabPanel(
          "Kinase Targets",
          DT::dataTableOutput("kinasesubsTable"),
        )
      )
    )
  )
  )
)