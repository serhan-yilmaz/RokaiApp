
#library(R.matlab)
library(Matrix)
library(shiny)
library(DT)
library(ggplot2)
source("compute_pvalues.R")
source("rokai_kinase_weights.R")
source("rokai_inference.R")
source("rokai_core.R")
source("rokai_circuit.R")
source("rokai_weights.R")
#applyCapturedAppOptions(shiny.sanitize.errors = TRUE)

#shinyOptions(shiny.sanitize.errors = TRUE)

#sites <- read.csv(paste(folder, "site.csv", sep=""))
#kinases <- read.csv(paste(folder, "kinase.csv", sep=""))

#net <- readMat(paste(folder, "rokai_networks_r.mat", sep=""))

#Wk2s = net$Wkin2site
folder = "data/"
Tsample <- read.csv(paste(folder, "sample_data_uniprot_human.csv", sep=""))

#T$ID = paste(T$Protein, T$Position, sep="_")



withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "message")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

server <- function(input, output, session) {
  
  observe({
    invalidateLater(1000)
    
    withConsoleRedirect("console", {
    })
  })
  
  network_value <- reactiveVal("uniprot.human")
  upload_name <- reactiveVal("")
  myvalue <- reactiveVal("")
  ready <- reactiveVal(TRUE)
  initialized <- reactiveVal(TRUE)
  
  reactive_network <- reactive({
    req(initialized())
    switch (network_value(),
            "uniprot.human" = fname <- "rokai_network_data_uniprotkb.rds",
            "uniprot.mouse" = fname <- "rokai_network_data_uniprotkb_mouse.rds",
            validate(
              need(FALSE, "Invalid network state.")
            )
    )
    NetworkData <- readRDS(paste("data/", fname, sep =""));
    return (NetworkData)
  })
  
  reactive_dataset <- reactive({
    req(initialized())
    switch (myvalue(),
            "sample" = Tsample,
            "upload" = upload_dataset(),
            validate(
              need(FALSE, "Waiting for data...")
            )
    )
  })
  
  refProteomeValue <- reactive({
    switch(input$refproteome, 
           "Uniprot Human" = "uniprot.human",
           "Uniprot Mouse" = "uniprot.mouse")
  })
  
  observeEvent(input$contactLink, {
    updateTabsetPanel(session, "aboutTabset", "Contact")
  })
  
  observeEvent(input$citeLink, {
    updateTabsetPanel(session, "aboutTabset", "How to cite us?")
  })
  
  observeEvent(input$buttonSampleData, {
    network_value("uniprot.human")
    myvalue("sample")
    if(input$mainTabset == "About"){
      updateTabsetPanel(session, "mainTabset", "Plot")
    }
  })
  
  observeEvent(input$file1, {
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    upload_dataset()
    req(upload_dataset())
    network_value(refProteomeValue())
    if(input$mainTabset == "About"){
      updateTabsetPanel(session, "mainTabset", "Plot")
    }
  })
  
  upload_dataset <- reactive({
    library(tools)
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    fileInfo <- input$file1
    ext = file_ext(inFile$datapath)
    switch(ext, 
           "csv" = x <- read.csv(inFile$datapath),
           validate(
             need(FALSE, "Invalid file type.")
           )
    )
    myvalue("upload")
    upload_name(fileInfo$name)
    message(cat("Dataset is uploaded: ", fileInfo$name))
    return(x)
  })
  
  current_dataset <- reactive({
    req(reactive_dataset())
    T <- reactive_dataset()
    T$ID = paste(T$Protein, T$Position, sep="_")
    return (T)
  })
  
  mapped_dataset <- reactive({
    req(reactive_dataset())
    T <- reactive_dataset()
    NetworkData <- reactive_network()
    T$Position = gsub('\\D+','', T$Position)
    
    T$ID = paste(T$Protein, T$Position, sep="_")
    indices = match(T$ID, NetworkData$Site$Identifier)
    valids = !is.na(indices);
    X = rep(NA, nrow(NetworkData$Site))
    X[indices[valids]] = T$Quantification[valids]
    
    validate(
      need(nnzero(!is.na(X))>0, "Mapping failed.")
    )
    
    return (X)
  })
  
  
  preprocessed_dataset <- reactive({
    req(mapped_dataset())
    X <- mapped_dataset()
    validSites = !is.na(X);
    Xv = X[validSites];
    
    switch (input$datanorm,
            "Centered" = Xv <- (Xv - mean(Xv)),
            "Normalized" = Xv <- (Xv - mean(Xv)) / sd(Xv))
    #Xv = (Xv - mean(Xv))
    Sx = rep(sd(Xv), length(Xv))
    return (list("Xv" = Xv, "Sx" = Sx, "validSites" = validSites))
  })
  
  site_table <- reactive({
    req(preprocessed_dataset())
    ds <- preprocessed_dataset();
    
    validSites = ds$validSites
    Xv = ds$Xv
    Sx = ds$Sx
    Zx = Xv / Sx
    res = compute_pvalues(as.matrix(Zx))
    
    NetworkData <- reactive_network()
    ST = NetworkData$Site[validSites,]
    ST$Phos = Xv
    ST$StdErr = Sx
    ST$ZScore = Zx
    ST$PValue = res$PValues
    ST$QValue = res$QValues
    
    return (ST)
  })

  kinase_activities <- reactive({
    req(preprocessed_dataset())
    ds <- preprocessed_dataset();
    
    validSites = ds$validSites
    Xv = ds$Xv
    Sx = ds$Sx
    
    NetworkData <- reactive_network()
    Wk2s = NetworkData$Wkin2site
    
    wk2s = Wk2s[, validSites];
    nSubs = (wk2s %*% rep(1, length(Xv)))
    
    switch(input$rokaiNetwork, 
           "KinaseSubstrate" = ropts <- list("ppi" = F, "sd" = F, "coev" = F),
           "KS+PPI" = ropts <- list("ppi" = T, "sd" = F, "coev" = F),
           "KS+PPI+SD" = ropts <- list("ppi" = T, "sd" = T, "coev" = F),
           "KS+PPI+SD+CoEv" = ropts <- list("ppi" = T, "sd" = T, "coev" = T))
    
    if(input$rokaiEnabled){
      if(ropts$ppi){
        Wk2k = NetworkData$net$Wkin2kin * 1e-3
      } else {
        Wk2k = NULL
      }
      nSite = ncol(NetworkData$Wkin2site)
      Ws2s = sparseMatrix(
        i = c(),
        j = c(), 
        x = T,
        dims = c(nSite, nSite)
      )
      if(ropts$sd){
        Ws2s = Ws2s | NetworkData$net$Wsite2site.sd
      }
      if(ropts$coev){
        Ws2s = Ws2s | NetworkData$net$Wsite2site.coev
      }
      Ws2s = Ws2s[validSites, validSites]
      rc <- rokai_core(Xv, Sx, wk2s, Wk2k, Ws2s)
      #Xs = rc$Xs
      Fk = rokai_kinase_weights(Xv, wk2s, rc$F)
      ri <- rokai_inference(Xv, Sx, Fk)
      A <- ri$A
      S <- ri$S
      Z <- ri$Z
    } else {
      A = (wk2s %*% Xv) / nSubs
      S = sd(Xv) / sqrt(nSubs)
      Z = A / S
    }
    res = compute_pvalues(as.matrix(Z))
    
    K = NetworkData$Kinase
    K$NumSubs = as.matrix(nSubs)
    K$Activity = as.matrix(A)
    K$StdErr = as.matrix(S)
    K$ZScore = as.matrix(Z)
    K$ZScore = as.matrix(Z)
    K$PValue = res$PValues
    K$FDR = res$QValues
    
    return (K)
  })
  
  kinase_subs_table <- reactive({
    req(preprocessed_dataset())
    req(site_table())
    
    ds <- preprocessed_dataset();
    validSites = ds$validSites
    
    ST  <- site_table()
    NetworkData <- reactive_network()
    K = NetworkData$Kinase
    

    wk2s = NetworkData$Wkin2site[, validSites];
    indices = which(wk2s)
    i1 = indices %% nrow(wk2s)
    i2 = floor(indices/nrow(wk2s))+ 1
    
    KS = data.frame(
      KinID = K$KinaseID[i1],
      KinName = K$KinaseName[i1],
      KinGene = K$Gene[i1],
      SubsProtein = ST$Protein[i2],
      SubsGene = ST$Gene[i2],
      Position = ST$Position[i2],
      Flanking = ST$Flanking[i2],
      Quantification = ST$Phos[i2],
      ZScore = ST$ZScore[i2],
      PValue = ST$PValue[i2],
      FDR = ST$QValue[i2]
    )
    
    return (KS)
  })
  
  kinasePlot <- reactive({
    req(kinase_activities())
    K <- kinase_activities()
    Ks <- K[!is.na(K$Activity),]
    si <- order(Ks$Activity, decreasing = TRUE)
    Ks <- Ks[si,]
    Ks <- Ks[Ks$NumSubs >= input$minnumsubs,]
    Ks <- Ks[abs(Ks$ZScore) >= input$minzscore,]
    
    c_limit = 4
    Ks$Color <- ifelse(Ks$ZScore> 0, "red", "blue")
    Ks$ColoringVar = Ks$ZScore
    Ks$ColoringVar = pmax( -1*c_limit, pmin(Ks$ColoringVar, c_limit))
    
    ## Add custom color scaling - If needed
    ## Add XLabel Coloring - If needed
    
    Ks$Sorting = -1*Ks$Activity
    Ks$Yaxis = Ks$Activity
    yaxisText = "Kinase Activity"
    showErrorBars = TRUE
    if(input$yaxis == "Z-Score"){
      Ks$Yaxis = Ks$ZScore
      Ks$Sorting = -1*Ks$ZScore
      yaxisText = "Z-Score"
      showErrorBars = FALSE
    }
    
    p <- ggplot(data=Ks, aes(x=reorder(KinaseName, Sorting), y=Yaxis, fill = ColoringVar)) +
      geom_bar(stat="identity", width=0.8, col="#333333", size = 0.75) +
      theme_minimal() +
      theme(text = element_text(size=16),
            axis.text.x = element_text(angle=90, hjust=1, face = "bold"),
            legend.key.height = unit(1.25, "cm"))
    
    if(showErrorBars){
      p <- p + geom_errorbar(aes(ymin=Activity-1.96*StdErr, ymax=Activity+1.96*StdErr), width=.5, size = 0.95)
    }
    
    # Check for other colors at: https://ggplot2.tidyverse.org/reference/scale_brewer.html
    # BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
    p <- p + scale_fill_distiller(palette = "RdYlBu", type = "div", limit = c_limit * c(-1, 1))
    
    p <- p + labs(fill = "Z-Score", x = "", y = yaxisText)
    return (p)
  })
  
  output$distPlot <- renderPlot({
    kinasePlot()
  })
  
  output$buttonDownloadSampleData <- downloadHandler(
    filename = function() { paste('sample_data.csv', sep='') },
    content = function(file) {
      write.csv(Tsample, file = file, row.names = FALSE, quote=FALSE)
      #write_csv(file, Tsample), 
    }
  )
  
  output$downloadKinasePlotPNG <- downloadHandler(
    filename = function() { paste('kinase-visualization.png', sep='') },
    content = function(file) {
      h = 4.1
      ggsave(file, plot = kinasePlot(), device = "png", width=3*h, height=h)
    }
  )
  
  output$downloadKinasePlotPDF <- downloadHandler(
    filename = function() { paste('kinase-visualization.pdf', sep='') },
    content = function(file) {
      h = 4.6
      ggsave(file, plot = kinasePlot(), device = "pdf", width=3*h, height=h)
    }
  )
  
  output$kinaseTable <- DT::renderDataTable(server = FALSE, {
    req(kinase_activities())
    K <- kinase_activities();
    Ks <- K[!is.na(K$Activity),]
    si <- order(abs(Ks$ZScore), decreasing = TRUE)
    Ks <- Ks[si,]
    Ks$ZScore = round(Ks$ZScore, digits = 3)
    Ks$Activity = round(Ks$Activity, digits = 3)
    Ks$StdErr = round(Ks$StdErr, digits = 3)
    colnames(Ks)[which( colnames(Ks)=="KinaseName" )] <- "Name"
   # colnames(Ks)[which( colnames(Ks)=="ZScore" )] <- "Z-Score"
  #  x <- slice_head(x, n=1000);
    
    fn = 'kinase_table'
    DT::datatable(Ks, rownames= FALSE, extensions = 'Buttons', 
              options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                             paging = TRUE, searching = TRUE, pageLength = 10, dom = 'Bfrtip', buttons = list('copy', list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn), list(extend = 'pdf', filename = fn)))) %>% 
      formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
  })
  
  output$kinasesubsTable <- DT::renderDataTable(server = FALSE, {
    req(kinase_subs_table())
    KS <- kinase_subs_table();
    #Ks <- K[!is.na(K$Activity),]
    #si <- order(abs(Ks$ZScore), decreasing = TRUE)
    #Ks <- Ks[si,]
    KS$Quantification = round(KS$Quantification, digits = 3)
    KS$ZScore = round(KS$ZScore, digits = 3)
    #KS$Activity = round(Ks$Activity, digits = 3)
    #KS$StdErr = round(Ks$StdErr, digits = 3)
    #colnames(KS)[which( colnames(KS)=="KinaseName" )] <- "Name"
    # colnames(Ks)[which( colnames(Ks)=="ZScore" )] <- "Z-Score"
    #  x <- slice_head(x, n=1000);
    
    fn = 'kinase_targets'
    DT::datatable(KS, rownames= FALSE, extensions = 'Buttons', 
                  options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                                 paging = TRUE, searching = TRUE, pageLength = 10, dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = fn), list(extend = 'excel', filename = fn)))) %>% 
      formatSignif('PValue', 3) %>% formatSignif('FDR', 3) 
  })
  
}