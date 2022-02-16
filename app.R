Sys.setenv(LANG = "en")
source("rserver.R");
source("ui.R");
options(shiny.sanitize.errors = F)
shinyApp(ui, server, options = list(sanitize.errors = F))

