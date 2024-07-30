library(shiny)
library(magrittr)
library(DT)
library(cicerone)

# For javascript
library(shinyjs)

# For notifications
library(shinytoastr)

# For tooltips
library(shinyBS) 
library(shinyhelper)
library(tippy)

#library(shinydashboard)

source("current_version.R")

## library(shinycssloaders) - Dependency



multiChoicePicker <- function(id, label, choices, selected = choices[1], isInline = "T") {
  switch(isInline, 
    "T" = R <- tags$div(
      class = "inline-block", id = paste(id, "_div", sep = ""), 
      style = "justify-content: space-between;", 
      tags$b(label),
      shinyWidgets::pickerInput(id, "", choices, selected = selected, width = "fit", inline = T)
    ),
    "F" = R <- tags$div(
      id = paste(id, "_div", sep = ""), 
      tags$b(label),
      #selectInput(id, label, choices, selected = selected, width = "auto")
      shinyWidgets::pickerInput(id, "", choices, selected = selected, width = "fit", inline = F)
    )
  )
  return (R)
}


foList <- function(...){
  x <- list(...)
  outList <- list()
  previous = NULL
  for(i in seq(1, length(x), 1)){
    if((i %% 2) == 0){
      outList[[previous]] <- x[[i]]
    }
    previous = x[[i]]
  }
  return(outList)
}

paper_txt <- function(authors, date, title, journal, link, misc){
  tags$div(
    class = "inline", 
    style = "margin-bottom: 6px; margin-top: 6px;", 
    tags$ul(
      style = "margin-bottom: 0px; margin-top: 0px;", 
      tags$li(
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


dataset_version_text <- function(dataset, date, link){
  id = paste(dataset, "txt", sep = "_");
  dataset <- paste(dataset, ":", sep = "")
  tags$tr(
    tags$td(tags$li(tags$a(dataset, href = link))), 
    tags$td(tags$text(date, id = id))
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

contact_question <- function(qtxt, atxt1, href1, atxt2, href2, actLink = FALSE){
  if(actLink){
    link <- actionLink(style = "font-size: large;", href1, atxt1)
  } else {
    link <- tags$a(style = "font-size: large;", atxt1, href=href1)
  }
  
  if(actLink){
    link2 <- actionLink(style = "font-size: large;", href2, atxt2)
  } else {
    link2 <- tags$a(style = "font-size: large;", atxt2, href=href2)
  }
  
  tags$div(
    class = "inline", 
    style = "margin-bottom: 8px; margin-top; 8px;", 
    tags$text(style = "font-size: large;", qtxt),
    link,
    tags$text(style = "font-size: large;", " or "), 
    link2
  )
}



on_ready <- paste(
  "$(function() {",
  "$(document).on('shiny:connected', function(e) {",
  "Shiny.setInputValue('initialized', 1);",
  "});",
  "",
  "});",
  sep = "\n"
)


#version_style <- function(){"font-size: 12px; color:#737373;"}
#version_style <- function(){"font-size: 14px; color:#A3A3A3;"}
version_style <- function(){"font-size: 14px; color:#93A3A3;"}
version_style_additional <- function(){
  "-webkit-user-select: none;
  -khtml-user-select: none;
  -moz-user-select: none;
  -ms-user-select: none;
  -o-user-select: none;
  user-select: none;"
}



about_tab <- function(){
  tabsetPanel(id = "aboutTabset",
    tabPanel(
      "Welcome",
      tags$div(
        class = "panel-body", id = "about_main_div", 
        style = "padding-bottom:5px; padding-top:2px; margin:0px;", #  height: 78px;
        #tags$p(),
        tags$h3("Welcome!", style="font-weight:bold;"),
        tags$h4(style="font-style:italic;", "RoKAI is a computational tool for inferring kinase activities in a robust manner using functional networks."),
        
        about_question("Using it first time? To help getting started, try our ", "Interactive Tutorial", "interactiveDemo", actLink = T),
        #about_question("Using it first time? To help getting started, read our ", "User Manual", "https://github.com/serhan-yilmaz/RoKAI/raw/master/rokai_user_manual.pdf"),
        about_question("Prefer to run locally? Download source code at ", "Github page", "https://github.com/serhan-yilmaz/Rokai"),
        contact_question("Have a quick question or suggestions?", "Contact us", "contactLink", "Use the feature request box", "leaveCommentLink", actLink = T),
        #about_question("Have a quick question or have suggestions?", "Contact us", "contactLink", actLink = T),
        #about_question("Have a quick question or need some help?", "Contact us", "contactLink", actLink = T),
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
      id = "Contact", 
      tags$div(
        class = "panel-body",
        style = "padding-bottom:5px; padding-top:2px; margin:0px;", #  height: 78px;
        #tags$p(),
        tags$h3("Contact", style="font-weight:bold;"),
       # desc_text("RoKAI is designed by Serhan Yilmaz and Mehmet Koyuturk at Case Western Reserve University."),
       tags$div(
         class = "inline", 
         style = "font-size: medium; margin-bottom: 6px; margin-top; 6px;", 
         "RoKAI is designed by ", 
         tags$a("Serhan Yilmaz", href = "http://www.serhanyilmaz.com/", target="_blank"), " and ", tags$a("Mehmet Koyuturk", href = "http://compbio.case.edu/koyuturk/", target="_blank"), " at Case Western Reserve University.",
       ),
        desc_text("If you have any questions, please contact <serhan.yilmaz@case.edu>"),
        
        tags$h4("Feature Suggestions & Comments", style="font-weight:bold;"),
        desc_text("To give feedback, request a new feature or to report a problem, please use the form below:"),
        tags$div(
        tags$div(style="display:inline-block; margin: 2px 0px 2px 0px;",
          textInput("textinput_name", "Name", value = "", width = 220, placeholder = "(Optional)")
        ),
        tags$div(style="display:inline-block; margin: 2px 8px 2px 8px; ",
          textInput("textinput_org", "Organization", value = "", width = 220, placeholder = "(Optional)"),
        ),
        ),
        tags$div(
        tags$div(style="display:inline-block; margin: 2px 0px 2px 0px;",
          textInput("textinput_email", "Contact Email", value = "", width = 270, placeholder = "(Optional)"),
        ), 
        tags$div(style="display:inline-block; margin: 2px 8px 2px 8px;",
           selectInput("message_type", "Category", 
                       choices = foList("Feature Request", 1, "Comment", 2, "Bug Report", 3), 
                       selected = 1, selectize = F, width = 170)    
        ),
        ),
        tags$div(style = "margin: 2px 0px 2px 0px;",
          textAreaInput("textinput_message", "Message", height = 150, value = "", width = 460),
          actionButton("buttonLeaveFeedback", "Submit", style = "margin-top: 4px;"),
        ),
        desc_text("The name, organization and email fields are optional. Please enter a contact information if you would like to be notified about future updates (e.g., if the requested feature is implemented). "),
       # bsCollapse(id = "collapseExample", open = "Panel 2",
       #            bsCollapsePanel("Panel 1", "This is a panel with just text ",
       #                            "and has the default style. You can change the style in ",
       #                            "the sidebar.", style = "info")
                 # bsCollapsePanel("Panel 2", "This panel has a generic plot. ",
                  #                "and a 'success' style.", plotOutput("genericPlot"), style = "success")
       # )
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
        tags$p("", style = "margin: 0px; padding-bottom: 2px; padding-top: 0px;"),
        desc_text("RoKAI uses the following resources for functional networks:"),
        paper_txt("Hornbeck, P. V. et al.", "2015", "Phosphositeplus, 2014: mutations, ptms and recalibrations.", "Nucleic acids research", "https://doi.org/10.1093/nar/gku1267", "43(D1), D512-D520"),
        paper_txt("Licata, L. et al.", "2020", "SIGNOR 2.0, the SIGnaling network open resource 2.0: 2019 update.", "Nucleic acids research", "https://doi.org/10.1093/nar/gkz949", "48(D1), D504-D510"),
        paper_txt("Minguez, P. et al.", "2012", "PTMcode: a database of known and predicted functional associations between post-translational modifications in proteins.", "Nucleic acids research", "https://doi.org/10.1093/nar/gks1230", "41(D1), D306-D311"),
        paper_txt("Szklarczyk, D. et al.", "2014", "STRING v10: protein–protein interaction networks, integrated over the tree of life.", "Nucleic acids research", "https://doi.org/10.1093/nar/gku1003", "43(D1), D447-D452"),
        paper_txt("Damle, N. P., & Köhn, M.", "2019", "The human DEPhOsphorylation Database DEPOD: 2019 update.", "Database", "https://doi.org/10.1093/database/baz133", ""),
        
        tags$h3("Acknowledgement", style="font-weight:bold;"),
        desc_text("This work was supported by National Institute of Health (NIH) grant R01-LM012980 from the National Libraries of Medicine.")
      )
    ),
    tabPanel(
      "Versions",
      tags$div(
        class = "panel-body",
        style = "padding-bottom:5px; padding-top:2px; margin:0px;", #  height: 78px;
        #tags$p(),
        tags$h3("Dataset Versions", style="font-weight:bold;"),
        #desc_text("Last updated dates for the datasets used are as follows:")
        tags$div(
            style = "max-width: 300px;", 
            helper(
              selectInput("dataset_version_selection", "NetworkData: ", 
                      choices = foList("v2.3.0 - Latest (August 2024)", "latest", "v2.2.0 (November 2022)", "v2.2.0", "v2.1.4 (May 2021)", "v2.1.4"), 
                      selected = "latest", selectize = F),
              type = "markdown", id = "include_networkdata_version_helper", content = "networkdata_version"
              ),
            tippy_this("include_networkdata_version_helper", "<span style='font-size:14px; margin: 0px;'>Click to learn about the NetworkData.<span>", allowHTML = TRUE), 
        ),
        desc_text("The versions (last modified dates) of the datasets used are as follows:"),
        tags$div(
          style="max-width:300px;",
        tags$table(
          style="font-size: 16px; width: 100%; margin-left: 22px;",
          dataset_version_text("Uniprot", "2022-10-04", "https://www.uniprot.org/"),
          dataset_version_text("PhosphoSitePlus", "2021-04-19", "https://www.phosphosite.org/"),
          dataset_version_text("Signor", "2021-05-21", "https://signor.uniroma2.it/"),
          dataset_version_text("STRING", "2018-12-20", "https://string-db.org/"),
          dataset_version_text("PTMcode", "2014-09-17", "https://ptmcode.embl.de/"),
          dataset_version_text("DEPOD", "2019-03-01", "http://www.depod.org/"),
          # tags$tr(
          #   tags$td(tags$li("PhosphoSitePlus:")), 
          #   tags$td("2021-04-19")
          # ),
          # tags$tr(
          #   tags$td(tags$li("Signor:")), 
          #   tags$td("2021-05-21")
          # ),
          # tags$tr(
          #   tags$td(tags$li("STRING:")), 
          #   tags$td("2018-12-20")
          # ),
          # tags$tr(
          #   tags$td(tags$li("PTMcode:")), 
          #   tags$td("2014-09-17")
          # ),
        ))
        # desc_text("PhosphoSitePlus: 2021-04-19"),
        # desc_text("Signor: 2021-05-21"),
        # desc_text("STRING: 2018-12-20"),
        # desc_text("PTMcode: 2014-09-17")
      )
    )
  )
}

ga_scripts <- function (){
  tags$script(HTML(
    "$(document).on('shiny:inputchanged', function(event) {
       if (event.name === 'buttonSampleData') {
          gtag('event', 'sample_button_clicked');
       }
     });
    Shiny.addCustomMessageHandler('upload_sucess_message', function(message) {
      gtag('event', 'upload_success', {
        'event_category' : 'input_data',
        'event_label' : message
      });
      //Shiny.setInputValue('foo2', message, {priority: 'event'});
    });
    Shiny.addCustomMessageHandler('upload_attempted_message', function(message) {
      gtag('event', 'data_uploaded', {
        'event_category' : 'input_data',
        'event_label' : message
      });
      //Shiny.setInputValue('foo2', message, {priority: 'event'});
    });
    Shiny.addCustomMessageHandler('interactive_demo_message', function(message) {
      gtag('event', 'interactive_demo', {
        'event_category' : 'main_navigation',
        'event_label' : message
      });
      //Shiny.setInputValue('foo2', message, {priority: 'event'});
    });
    
    "
  ))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  useToastr(),
  useShinyjs(),
  title = "RoKAI App",
  tags$head(
    tags$link(rel="shortcut icon", href="favicon.png"),
    tags$meta(name="description", content="RoKAI: Robust Inference of Kinase Activity using functional networks"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    includeHTML(("www/google-analytics.html")),
    tags$script(on_ready)
  ),
  # tags$head(
  #   tags$script("$(document).ready(function(){
  #                                       $('#modalExample').modal();
  #                                       });"),
  #   tags$script("$('#data_norm_tooltip_icon').addEventListener('click', function() {
  #   $('#modalExample').modal();
  #   });" )
  # ),
  ga_scripts(),
  use_cicerone(),
  verticalLayout(
    div(
      class = "panel-heading",
      #style = "margin:0px; padding:0px;",
      style = "margin-bottom:0px; padding-bottom:0px;",
      div(
        style = "position: relative; width: 100%",
        img(src='rokai_app_logo.png', align = "left", style = "height: 150px; padding-bottom:10px;"),
        
        tags$p(version_text(), style = paste(version_style(), version_style_additional(), "position: absolute; top: 38px; left:510px; width: 70%;", sep = ""))
        #tags$p("v2.0.0", style = "color:#A3A3A3;")
      ),
      tags$br(),
      #tags$br(style = "display: block; content: \"\"; margin-top: 16px;")
     # tags$span("", style = "font-size: 16.5px; margin-bottom:5px; padding-bottom:0px;")
    ),
  fluidRow(
      id = "main_layout_div", 
    column(width = 4,
           tags$form(class = "well", style = "margin-bottom:8px;", id = "main_control_div", 
 # sidebarLayout(
#    sidebarPanel(
      #tags$div(
        tags$div(
          class = "inline-block", id = "sample_data_div", 
          tags$b("Sample Data: ", style = "margin-right: 10px;"),
          tags$div(
            class = "inline-block", 
          #  style = "align: center",
          fluidRow(column(12, align = "left", 
          helper(tags$div(
            actionButton("buttonSampleData", "Load Sample Data"),
            tags$b(style = "margin-left: 4px; margin-right: 4px;"), 
            downloadButton('buttonDownloadSampleData', 'Download')
          ), type = "inline", id = "input_data_tooltip",
          , title = "Input Data Format",
          content = c("To use rokai with your data, you only need a single input file having three columns:", 
                                    paste("<b>Protein:</b>", "The Uniprot protein identifier. "),
                                    paste("<b>Position:</b>", "The position of the site on the protein."),
                                    paste("<b>Quantification:</b>", "The phosphorylation of the site provided as log-fold change."),
                                          "Note that, you can download the sample data to see an example. ")          )
          ),
          tippy_this("input_data_tooltip", "<span style='font-size:14px; margin: 0px;'>Click to learn about the input data format.<span>", allowHTML = TRUE), 
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
        tags$div(style = "margin: 0px;", id = "upload_data_div", 
        tags$div(
          fileInput("file1", "Upload Data:", accept = c(".csv")),
          tags$style(".shiny-input-container {margin-bottom: 0px} #file1_progress { margin-bottom: 3px } .checkbox { margin-top: 0px}"),
          tags$style(".checkbox {margin-bottom: 0px;}"),
        ),
        multiChoicePicker("refproteome", "Reference Proteome:", c("Uniprot Human", "Uniprot Mouse", "Uniprot Rat")),
        tags$hr(style = "margin: 8px 0px 8px 0px;")
        ),
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
      tags$div(style = "margin: 0px", id = "inference_options_div", 
               helper(
                 multiChoicePicker("datanorm", "Fold Changes:", c("Raw", "Centered", "Normalized"), "Centered"),
                      type = "inline", id = "data_norm_tooltip_icon",
                 title = "Data Normalization Options",
                   content = c("Determines whether the input quantifications are to be normalized in preprocessing.", 
                               paste("<b>Raw:</b>", "Uses the input fold changes without modification."),
                               paste("<b>Centered:</b>", "This option centers the input log-fold changes around 0 (by subtracting the mean across all sites). "),
                               paste("<b>Normalized:</b>", "This option additionally scales the log-fold changes to have 1 standard deviation. "),
                              "</br> <b>Note:</b> The aim of this normalization procedure is to eliminate any possible imbalance between the case and control samples. The Centered option is recommended unless the necessary data preprocessing & quality control steps for this purpose are applied beforehand. ")
                 
                 ),
                 
               #
      tippy_this("data_norm_tooltip_icon", "<span style='font-size:14px; margin: 0px;'>Determines whether the input data should be normalized. Click to learn more. <span>", allowHTML = TRUE), 
    #  tippy("Hover me!", tooltip = "Hi, I'm the tooltip!"),
      multiChoicePicker("ksNetwork", "Kinase Substrate Dataset:", c("PhosphoSitePlus", "PSP+Signor"), "PSP+Signor"),
      multiChoicePicker("rokaiNetwork", "RoKAI Network:", c("KinaseSubstrate", "KS+PPI", "KS+PPI+SD", "KS+PPI+SD+CoEv"), "KS+PPI+SD+CoEv"),
    checkboxInput("rokaiEnabled", "Use sites in functional neighborhood", TRUE),
      #   tags$span("Include phosphatases in the analysis: "),
      #   materialSwitch(inputId = "includePhosphatases", label = "", status = "warning", inline = T),
      #   #tags$b(ls$HardcoreModeEnabledLabel, style = "color: orange;", id = "hardcore_enabled_label")
      # )
    
    #materialSwitch(inputId = "includePhosphatases", label = "", status = "warning", inline = T), 
      tags$hr(style = "margin: 1px 0px 1px 0px;"),
      tags$div(id = "phosphatase_options_div", 
        multiChoicePicker("phosphataseNetwork", "Phosphatase Substrate Dataset:", c("DEPOD"), "DEPOD"),
        helper(
          checkboxInput("includePhosphatases", "Include phosphatases in the analysis", TRUE),
          type = "markdown", id = "include_phosphatases_helper_icon", content = "include_phosphatases"
        ),
        tippy_this("include_phosphatases_helper_icon", "<span style='font-size:14px; margin: 0px;'>Determines whether phosphatases should be analyzed alongside the kinases. Click to learn more. <span>", allowHTML = TRUE), 
      ),
      #tags$hr(style = "margin: 8px 0px 8px 0px;")
      ),
   #   bsCollapsePanel("Title", 
   #         tags$div("fjkjdjkfd")
   #   )
      # tags$div(style = "text-align: right; min-height: 0px; padding: 0px; margin-bottom: 0px;",
      #   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
      #            tags$p(style = "margin:0px; padding:0px;", "Loading..."),id="loadmessage")
      # )

     # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
      #           tags$div(style = "text-align: right;", "Loading...",id="loadmessage"))
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
    "We are developing a new tool to analyze phospho-proteomics data. To access it, please visit: ",
    #tags$br(),
    #tags$a("http://explorer.rokai.io", href="http://explorer.rokai.io"),
   # tags$a("http://explorer.rokai.io", href="http://explorer.rokai.io"),
    tags$a("https://rokai.io/explorer", href="https://rokai.io/explorer"),
    #"<serhan.yilmaz@case.edu>"
  )
), 
tags$div(
  style = "margin-left: 14px;",
 tags$text(style = "color:#404040;", "Rokai App is updated! Read about"), tags$a("the changes in v2.2.0", href = "https://github.com/serhan-yilmaz/RokaiApp/tree/master/docs/v2.2.0"),
)
#, verbatimTextOutput("text")
#textOutput("text")

# tags$div(
#   class = "panel panel-default",
#   style = "margin:0px; margin-bottom:5px;",
#   tags$div(
#     class = "panel-body",
#     style = "padding-bottom:10px; padding-top:10px; margin:0px;", #  height: 78px;
#     #tags$p(),
#     "RoKAI App is recently updated. To access the older version, please visit: ",
#     #tags$br(),
#     tags$a("http://legacy.rokai.io", href="http://legacy.rokai.io"),
#     #"<serhan.yilmaz@case.edu>"
#   )
# )

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
          tags$div(id = "kinase_plot_div", 
          shinycssloaders::withSpinner(plotOutput("distPlot", height = "340px")), 
          #splitLayout(
          fluidRow(
            column(width = 6, style = "padding: 8px;", fluidRow(id = "plot_sliders_div", 
            column(width = 6, style = "padding: 8px;", sliderInput("minnumsubs", "Min. number of substrates", 1, 10, 3, step = 1, width = "220px")), 
            column(width= 6, style = "padding: 8px;", sliderInput("minzscore", "Min. absolute z-score", 0, 3, 1.5, step = 0.05, width = "220px"))
            )),
            column(width = 3, style = "padding: 8px; padding-left: 16px;", 
                   tags$div(
                     multiChoicePicker("yaxis", "Plot Y-Axis:", c("Activity", "Z-Score"), isInline = "F"),
                     tags$div(style = "margin-top: 2px;", 
                     multiChoicePicker("plotlayout", "Plot Layout:", c("Horizontal", "Vertical"), isInline = "F"))
                     )),
            column(width = 3, style = "padding: 8px;", tags$div(id = "plot_download_div", 
              downloadButton('downloadKinasePlotPNG', 'Download PNG'),
              tags$br(), 
              downloadButton('downloadKinasePlotPDF', 'Download PDF')
            )
            )
          )), 
        )
        ,
        tabPanel(id = "Kinases", 
          "Kinases",
          tags$div(id = "kinase_table_div", 
          shinycssloaders::withSpinner(DT::dataTableOutput("kinaseTable"))
          )
          #div(style = 'overflow: auto; max-height:450px;', div(style = "width: 96%", dataTableOutput("kinaseTable")))
        ),
        tabPanel(
          "Kinase Targets", id = "Kinase Targets", 
          tags$div(id = "kinase_targets_div", 
          shinycssloaders::withSpinner(DT::dataTableOutput("kinasesubsTable"))
          )
        )
      )
    )
  )
  )
)