#.libPaths("/home/shiny/libs")
library(openxlsx)
library(visNetwork)
library(igraph)
#library(biomaRt)
library(shinyjs)
#source("NetworkPipeline.R")

shinyServer (function(input, output, session) {
    
  loadZaman <- reactiveFileReader(10000,session, filePath = "Zaman.rda",
                                  load, envir = .GlobalEnv)
  
  loadexp <- reactiveFileReader(10000,session, filePath = "exp.rda",
                                load, envir = .GlobalEnv)
  
  global.data <- reactiveValues(showtab = FALSE,
                                showdt = FALSE,
                                dts = NULL,
                                dss = NULL,
                                muts = NULL,
                                exps = NULL,
                                showdss = FALSE,
                                showmut = FALSE,
                                showexp = FALSE,
                                shownet = FALSE,
                                unmatch = NULL, 
                                mutmatched = NULL,
                                tour = FALSE,
                                nets = NULL,
                                netstart = FALSE,
                                noticeid= NULL)
  
  # use reactive expression for saving the uploaded file
  uploadDT <- reactive({
    input$drugTarget
  })
  
  uploadDss <- reactive({
    input$dss
  })
  
  uploadMut <- reactive({
    input$mut
  })
  
  uploadExp <- reactive({
    input$exp
  })
  
  uploadFile <- reactive({
    # if(global.data$restfile){
    #   return(NULL)
    # } else {
    #   input$dataAll
    # }
    input$dataAll
  })
  
  
  closeAll <- compiler::cmpfun(function(){
    global.data$tour <- FALSE
    global.data$showtab <- FALSE
    global.data$dts <- NULL
    global.data$dss <- NULL
    global.data$exps <- NULL
    global.data$muts <- NULL
    global.data$unmatch <- NULL
    global.data$nets <- NULL
    global.data$shownet <- FALSE
    #global.data$restfile <- TRUE
    shinyjs::runjs(paste0('$("#dataAll_progress").css("visibility", "hidden"); $(".form-control").val("");'));
    shinyjs::runjs("$('#dataAll').val('abc').clone(true);")
    shinyjs::reset("testing")
    print(uploadFile())
    #shinyjs::runjs("$('#HowToUse').modal('hide');$('#Save_full_').modal('hide');");
  })
  
  ### Tour ###
  
  observeEvent(input$typetour, 
               {
                 global.data$tour <- TRUE
                 updateCheckboxInput(session, "separate", value = 0)
               }
              )
  
  observeEvent(input$endtour,{
    closeAll();
    
  })
  
  observeEvent(input$Restart, 
               {
                 updateCheckboxInput(session, "separate", value = 0);
                 closeAll();
                 runjs('$("#wraptour").show();')
                 #uploadFile();
                 #global.data$restfile <- FALSE
                 })
  
  
  
  observeEvent(input$datatab, {
    name_ = "_example.xlsx"
    
    shinyjs::runjs(paste0('$("#dataAll_progress").html(\'<div class="progress-bar" style="width: 100%;">Upload complete</div>\').css("visibility", "visible");
                          $(".form-control").val("', name_,'");'))
    global.data$showtab <- TRUE
    loadexp()
    global.data$dts <- dts
    global.data$dss <- dss
    global.data$exps <- exps
    global.data$muts <- muts
    global.data$unmatch <- unmatched
    global.data$nets <- nets
  })
  
  observeEvent(input$loadnet, {
    global.data$shownet <- TRUE
  })

  
  
  output$tabs <- renderUI({
    inFile <- uploadFile()
    if(!is.null(inFile)) {
      tabsetPanel(type = "tabs",
                          tabPanel("Targets", dataTableOutput("displaytargets")),
                          tabPanel("DSS", dataTableOutput("displaydss")),
                          tabPanel("Mutation", dataTableOutput("displaymut")),
                          tabPanel("Gene expression", dataTableOutput("displaygene.exp"))
      )
    }
  })
  
  observe({
    
    if(!global.data$showtab) shinyjs::disable("Nets")
    else shinyjs::enable("Nets")
  }
    
  )
  
  observe({
      if(global.data$netstart){
          global.data$noticeid<-showNotification(paste("Patient specific network construction in progress..."), duration = NULL)
      }else {
          if(length(global.data$noticeid)>=1) {
              removeNotification(global.data$noticeid[1])
              global.data$noticeid <- global.data$noticeid[-1]
              
          }
      }
  })
  
  
  output$btnNet <- renderUI({
    if(input$separate==FALSE & global.data$showtab){
      #actionButton("nets", "Construct Patient Network")
    }
  })
  
  observeEvent(input$Nets, {
    loadZaman()
    global.data$shownet <- TRUE
    #ensembl <- useMart("ensembl",dataset="hsapiens_gene_ensembl")
    #idmatch <- getBM(attributes=c('hgnc_symbol', 'entrezgene'), 
                     #filters = 'hgnc_symbol', 
                     #values = global.data$muts, 
                     #mart = ensembl)
    #m <- match(idmatch$entrezgene, V(Zaman)$EntrezID)
    m <- match(global.data$muts[, 1], V(Zaman)$Symbol)
    gene.matched <- global.data$muts[which(!is.na(m)), 1] 
    global.data$unmatch <- setdiff(global.data$muts[,1], gene.matched)
    global.data$mutmatched <- gene.matched

    #global.data$unmatch <- setdiff(global.data$muts, global.data$muts)
    
  })
  
  #observeEvent(input$unmatchbtn, {
    
  #})
  
  output$loadExData_small <- downloadHandler(
    filename = function(){ paste0("test.xlsx") },
    content = function(file){ file.copy("test.xlsx", file)},
    contentType = NULL
  )
  
  output$Save <- downloadHandler(
    filename = function(){ paste0("test.html") },
    content = function(file){ file.copy("test.html", file)},
    contentType = NULL
  )
  
  
  
  
  output$renderdt <- reactive({
    if(input$separate){
      indt <- uploadDT()
      if(!is.null(indt)) TRUE
      else FALSE
    } else {
      FALSE
    }
  })
  
  output$showInputData <- renderUI({
    if(input$separate){
      indt <- uploadDT()
      indss <- uploadDss()
      inmut <- uploadMut()
      inexp <- uploadExp()
      if(!is.null(indt)) {
        global.data$dts <- read.csv(indt$datapath, stringsAsFactors = F)
        #if(!is.null())
        global.data$showdt <- TRUE
      
      }
        
      if(!is.null(indss)) global.data$showdss <- TRUE
      if(!is.null(inmut)) global.data$showmut <- TRUE
      if(!is.null(inexp)) global.data$showexp <- TRUE
    } else {
      in1 <- uploadFile()
      if(!is.null(in1)) {
        global.data$dts <- tryCatch(read.xlsx(in1$datapath, sheet = "Targets"), 
                                    error = function(e) e,
                                    warning = function(w) w)
        global.data$dss <- tryCatch(read.xlsx(in1$datapath, sheet = "DSS"), 
                                    error = function(e) e,
                                    warning = function(w) w)
        global.data$muts <- tryCatch(read.xlsx(in1$datapath, sheet = "Mutations"), 
                                    error = function(e) e,
                                    warning = function(w) w)
        global.data$exps <- tryCatch(read.xlsx(in1$datapath, sheet = "Gene.exp"), 
                                    error = function(e) e,
                                    warning = function(w) w)
        if(is(global.data$dts, "error")){
          showModal(modalDialog(title = "Error",
                                "Loading drug target data failed!"))
        } else if(is(global.data$dss, "error")) {
          showModal(modalDialog(title = "Error",
                                "Loading drug response data failed!"))
          
        } else if(is(global.data$muts, "error")) {
          showModal(modalDialog(title = "Error",
                                "Loading mutation data failed!"))
        } else if(is(global.data$exps, "error")) {
          showModal(modalDialog(title = "Error",
                                "Loading expression data failed!"))
        } else {
          global.data$showtab <- TRUE
        }
          
          
        
      }
    }
    
    if(input$separate==FALSE & global.data$showtab){
      runjs('$("#wraptour").hide();');
      fluidRow(
        tags$div(id="showdata"),
        box(title = "Input Data", solidHeader = TRUE, status = "primary", width = 12, 
            collapsible = TRUE,
            tabsetPanel(type = "tabs",
                        tabPanel("Drug-Target", dataTableOutput("displaytargets")),
                        tabPanel("Drug response", dataTableOutput("displaydss")),
                        tabPanel("Mutation", dataTableOutput("displaymut")),
                        tabPanel("Gene expression", dataTableOutput("displaygene.exp"))
            ))
        
      )

         
      
    } else {
      if(global.data$showdt | global.data$showdss |
         global.data$showmut | global.data$showexp) {
        fluidRow(
          box(title = "Input Data", solidHeader = TRUE, status = "primary", width = 12, 
              collapsible = TRUE,
              tabsetPanel(type = "tabs",
                          conditionalPanel(condition = "global.data.showdt == true",
                                           tabPanel("Drug-Target", dataTableOutput("displaytargets"))),
                          
                          tabPanel("Drug response", dataTableOutput("displaydss")),
                          tabPanel("Mutation", dataTableOutput("displaymut")),
                          tabPanel("Gene expression", dataTableOutput("displaygene.exp"))
              ))
        )
      }
      
    }
    
    
    
  })
  
  
  output$showNetwork <- renderUI({
    
    if(global.data$shownet) {
        global.data$netstart <- TRUE
      fluidRow(
        box(title = "Patient Network", solidHeader = TRUE, status = "primary", width = 12,
            collapsible = TRUE,
            fluidRow(
              infoBox(
                "Unmatched mutations", length(global.data$unmatch), 
                actionLink("unmatchbtn", "Check unmatched mutations"),
                icon = icon("close"),
                color = "red"
              )
            ),
            fluidRow(
              column(width = 12, imageOutput("extraLegend", height = "80px"))
            ),
            fluidRow(
              #column(width = 3, imageOutput("extraLegend")),
              if(global.data$tour){
                column(width = 12, visNetworkOutput("nettour", height = "500px"))
              } else {
                column(width = 12, visNetworkOutput("network", height = "500px"))
              }
              
            )
      ), 
      
      bsModal("unmatchmodel", "Unmatched mutations", "unmatchbtn",
                 size = "large",
                 dataTableOutput("unmatch"))
      
          
          
      )
      
    }
    
    
    
  })
  
  output$unmatch <- renderDataTable({
    data.frame(mutations = global.data$unmatch)
  })
  

  
  output$displaytargets <- renderDataTable({
    if(input$separate){
      inFile <- uploadDT()
      if(is.null(inFile)) return(NULL)
      read.csv(inFile$datapath, stringsAsFactors = F)
    } else {
      global.data$dts
    }
    
  }, options = list(pageLength = 5))
  output$displaydss <- renderDataTable({
    if(input$separate){
      
    } else {
      global.data$dss
    }
    
  }, options = list(pageLength = 5))
  
  output$displaymut <- renderDataTable({
    if(input$separate){
      
    } else {
      global.data$muts
    }
    
  }, options = list(pageLength = 5))
  
  output$displaygene.exp <- renderDataTable({
    if(input$separate){
      
    } else {
      global.data$exps
    }
    
  }, options = list(pageLength = 5))
  
  
  output$network <- renderVisNetwork({
    
    # one drug could have more targets
    drugs <- unique(global.data$dts$Drug)
    # one target could be targeted by many drugs
    targets <- unique(global.data$dts$Targets)
    muts <- global.data$mutmatched
    
    e.lists <- c()
    for(i in 1:length(targets)) {
      for(j in 1:length(muts)) {
        spaths <- get.shortest.paths(Zaman,from = which(V(Zaman)$Symbol == targets[i]),
                                     to = which(V(Zaman)$Symbol == muts[j]), output = "both", mode = "all")
        if(length(spaths$vpath)!=0)
          #dis[i, j] <- length(as.numeric(spaths$vpath[[1]]))
          e.lists <- c(e.lists, as.numeric(spaths$epath[[1]]))
      }
    }
    
    subnet <- subgraph.edges(Zaman, e.lists)
    V(subnet)$name <- V(subnet)$Symbol
    
    
    
    # add drug nodes
    node.count <- vcount(subnet)
    subnet <- add.vertices(subnet, length(drugs))
    drug.node.idx <- (node.count + 1):(node.count + length(drugs))
    V(subnet)$name[drug.node.idx] <- drugs
    V(subnet)$type[drug.node.idx] <- "drugs"
    V(subnet)$Symbol <- V(subnet)$name

    for(i in 1:nrow(global.data$dts)){
      id1 <- which(V(subnet)$name == global.data$dts[i, 1])
      id2 <- which(V(subnet)$name == global.data$dts[i, 2])
      subnet <- add.edges(subnet, c(id1, id2), kind = "Inhibition")
    }
    
    V(subnet)$exp <- global.data$exps$expression[match(V(subnet)$name, global.data$exps$gene)]
    
    max.exp <- max(V(subnet)$exp, na.rm = TRUE)
    min.exp <- min(V(subnet)$exp, na.rm = TRUE)
    V(subnet)$expclass <- NA
    
    # only positive gene expression
    if(min.exp > 0) {
        # positive gene expression
        id.pos <- which(V(subnet)$exp >= 0)
        breaks.tmp <- c(0, max.exp/6, max.exp/3,max.exp/2,
        max.exp/6*4, max.exp/6*5, max.exp)
        V(subnet)$expclass[id.pos] <- as.numeric(cut(V(subnet)$exp[id.pos],
        breaks = breaks.tmp,
        include.lowest = TRUE))
        # no negative gene expression
        id.neg <- NULL
    }
    
    # only negative gene expression
    if(max.exp < 0){
        id.neg <- which(V(subnet)$exp < 0)
        breaks.tmp <- c(0, min.exp/6, min.exp/3, min.exp/2,
        min.exp/6*4, min.exp/6*5, min.exp)
        V(subnet)$expclass[id.neg] <- as.numeric(cut(V(subnet)$exp[id.neg],
        breaks = breaks.tmp,
        include.lowest = TRUE))
        # no positive gene expression
        id.pos <- NULL
    }
    
    if(max.exp > 0 & min.exp < 0){
        # positive gene expression
        id.pos <- which(V(subnet)$exp >= 0)
        breaks.tmp <- c(0, max.exp/6, max.exp/3,max.exp/2,
        max.exp/6*4, max.exp/6*5, max.exp)
        V(subnet)$expclass[id.pos] <- as.numeric(cut(V(subnet)$exp[id.pos],
        breaks = breaks.tmp,
        include.lowest = TRUE))
        id.neg <- which(V(subnet)$exp < 0)
        breaks.tmp <- c(0, min.exp/6, min.exp/3, min.exp/2,
        min.exp/6*4, min.exp/6*5, min.exp)
        V(subnet)$expclass[id.neg] <- as.numeric(cut(V(subnet)$exp[id.neg],
        breaks = breaks.tmp,
        include.lowest = TRUE))
        
    }
    
    
    subnet.Vis <- toVisNetworkData(subnet)
    
    # node shape
    subnet.Vis$nodes$shape <- "image"
    subnet.Vis$nodes$image <- "/kinase.png"
    subnet.Vis$nodes$muts <- FALSE
    mut.node <- match(muts, subnet.Vis$nodes$label)
    subnet.Vis$nodes$muts[mut.node] <- TRUE
    id.mut <- which(subnet.Vis$nodes$muts == TRUE)
    
    # drug node
    d.node <- match(drugs, subnet.Vis$nodes$label)
    subnet.Vis$nodes$image[d.node] <- "/drug.png"
    
    # type "other"
    subnet.Vis <- addimg("other", id.mut, id.pos, id.neg, "/other/other1.",
    "/other/other2.", subnet.Vis)
    
    # type "cytokine"
    subnet.Vis <- addimg("cytokine", id.mut, id.pos, id.neg, "/cytokine/cytokine1.",
    "/cytokine/cytokine2.", subnet.Vis)
    
    # type "growth factor"
    subnet.Vis <- addimg("growth factor", id.mut, id.pos, id.neg, "/cytokine/cytokine1.",
    "/cytokine/cytokine2.", subnet.Vis)
    
    
    # type "enzyme"
    subnet.Vis <- addimg("enzyme", id.mut, id.pos, id.neg, "/enzyme/enzyme1.",
    "/enzyme/enzyme2.", subnet.Vis)
    
    
    # type "transcription regulator"
    subnet.Vis <- addimg("transcription regulator", id.mut, id.pos, id.neg, "/tr/tr1.",
    "/tr/tr2.", subnet.Vis)
    
    # type "transporter"
    subnet.Vis <- addimg("transporter", id.mut, id.pos, id.neg, "/transporter/transporter1.",
    "/transporter/transporter2.", subnet.Vis)
    
    # type "kinase"
    subnet.Vis <- addimg("kinase", id.mut, id.pos, id.neg, "/kinase/kinase1.",
    "/kinase/kinase2.", subnet.Vis)
    
    # type "ligand-dependent nuclear receptor"
    subnet.Vis <- addimg("ligand-dependent nuclear receptor", id.mut, id.pos, id.neg, "/ligand/ligand1.",
    "/ligand/ligand2.", subnet.Vis)
    
    # type "Transmembrane receptor"
    subnet.Vis <- addimg("transmembrane receptor", id.mut, id.pos, id.neg, "/mr/mr1.",
    "/mr/mr2.", subnet.Vis)
    save(subnet.Vis, file="subnet.Vis.rda")
    edge.t1 <- which(subnet.Vis$edges$kind == "Activation")
    edge.t2 <- which(subnet.Vis$edges$kind == "Interaction")
    edge.t3 <- which(subnet.Vis$edges$kind == "Inhibition")
    subnet.Vis$edges$width <- 3
    subnet.Vis$edges$arrows <- ""
    subnet.Vis$edges$color <- "gray"
    subnet.Vis$edges$dashes <- FALSE
    subnet.Vis$edges$arrows[edge.t1] <- "to"
    subnet.Vis$edges$color[edge.t1] <- "red"
    subnet.Vis$edges$arrows[edge.t3] <- "to"
    subnet.Vis$edges$color[edge.t3] <- "blue"
    subnet.Vis$edges$color[edge.t2] <- "gray"
    subnet.Vis$edges$arrows[edge.t2] <- NA
    subnet.Vis$edges$dashes[edge.t2] <- TRUE
    
    # # mutation node
    # m.node <- which(subnet.Vis$nodes$mutation == 1)
    # 
    # 
    # subnet.Vis$nodes$shape[d.node] <- "square"
    # subnet.Vis$nodes$shape[t.node] <- "diamond"
    # subnet.Vis$nodes$shape[m.node] <- "star"
    
    
    # nodes <- data.frame(id = 1:4, 
    #                     shape = c("image", "circularImage"),
    #                     image = "/kinase1.png",
    #                     label = "I'm an image",
    #                     color = "red")
    # 
    # edges <- data.frame(from = c(2,4,3,3), to = c(1,2,4,2))
    
    # make the legend
    # ledges <- data.frame(color = c("red", "blue", "gray"),
    #                     label = c("Activation", "Inhibition","Physical interaction"), 
    #                     arrows =c("to", "to", NA),
    #                     dashes = c(FALSE, FALSE, TRUE),
    #                     font = c(list(align = "bottom")))
    # 
    # lnodes <- data.frame(label = c("Compound", "Kinase", "Transmembrane \n Receptor", "Enzyme", "Transcription \n Regulator",
    #                                "Cytokine/ \n Growth Factor", "Transporter", "Ligand-dependent \n Nuclear Receptor",
    #                                "Other"),
    #                      shape = rep("image", 9),
    #                      image = c("/drug.png", "/kinase/kinase1.6.png", "/mr/mr1.6.png", "/enzyme/enzyme1.6.png",
    #                                "/tr/tr1.6.png", "/cytokine/cytokine1.6.png", "/transporter/transporter1.6.png",
    #                                "/ligand/ligand1.6.png", "/other/other1.6.png"))
    # 
    
    global.data$netstart <- FALSE
    visNetwork(subnet.Vis$nodes, subnet.Vis$edges, height = "1500px", width = "100%") %>%
      visNodes(shapeProperties = list(useBorderWithImage = FALSE)) %>%
      visOptions(manipulation = TRUE, highlightNearest = TRUE)
      
      # visLegend(addEdges = ledges,
      #           addNodes = lnodes,
      #           position = "right", useGroups = FALSE, ncol = 2)
  })
  
  output$nettour <- renderVisNetwork({
    if(global.data$tour) {
      global.data$netstart <- FALSE
      visNetwork(global.data$nets$nodes, global.data$nets$edges, height = "100%", width = "100%") %>%
        visNodes(shapeProperties = list(useBorderWithImage = FALSE)) %>%
        visOptions(manipulation = TRUE, highlightNearest = TRUE)

    }
    
  })
  
  output$zaman.sub <- renderVisNetwork({
    inFile <- uploadFile()
    
    if(!is.null(inFile)) {
      alldata <- list()
      loadZaman()
      alldata$targets <- read.xlsx(inFile$datapath, sheet = "Targets")
      alldata$dss <- read.xlsx(inFile$datapath, sheet = "DSS")
      alldata$muts <- read.xlsx(inFile$datapath, sheet = "Mutations")
      alldata$gene.exp <- read.xlsx(inFile$datapath, sheet = "Gene.exp")
      subnet <- NetworkPipeline(alldata$gene.exp, alldata$muts[, 1], alldata$dss, alldata$targets)
      node.count <- vcount(subnet)
      subnet.Vis <- toVisNetworkData(subnet)
      #str(subnet.Vis)
      # drug node
      d.node <- which(subnet.Vis$nodes$drug == 1)
      # target node
      t.node <- which(subnet.Vis$nodes$target == 1)
      # mutation node
      m.node <- which(subnet.Vis$nodes$mutation == 1)

      # drug nodes tips
      subnet.Vis$nodes$title[d.node] <- paste0("<p><b> DSS:",
                                       subnet.Vis$nodes$response[d.node], "</b></p>")
      color.opt <- c("#0080ff", "#ccffff","#99ccff", "#ff9999", "#ff0000")
      subnet.Vis$nodes$color[d.node] <- color.opt[
        as.numeric(cut(subnet.Vis$nodes$response[d.node], breaks = seq(0, 50, by = 10), include.lowest = T))]
      # other nodes tips
      subnet.Vis$nodes$title[-d.node] <-
        paste0("<p><b> Gene expression:", round(10^subnet.Vis$nodes$gex[-d.node], 3),
               "</b></p>")
      subnet.Vis$nodes$color[-d.node] <- color.opt[
        as.numeric(cut(subnet.Vis$nodes$gex[-d.node], breaks = 0:5, include.lowest = T))]
      #subnet.Vis$nodes$title <- paste0("<p><b> Entrez:", subnet.Vis$nodes$EntrezID, "</b></p>")

      # node shape
      subnet.Vis$nodes$shape <- "dot"
      subnet.Vis$nodes$shape[d.node] <- "square"
      subnet.Vis$nodes$shape[t.node] <- "diamond"
      subnet.Vis$nodes$shape[m.node] <- "star"
      # legend
      lnodes <- data.frame(label = c("Target", "Drug", "Mutation"), shape = c("diamond", "square", "star"))
      ledges <- data.frame(color = c("red", "blue", "gray"), arrows = c("to", "to", ""),
                           label = c("Activation", "Inhibition", "Interaction"), width = 3)

      subnet.Vis$nodes$font <- "16px"

      # edges settings
      edge.t1 <- which(subnet.Vis$edges$kind == "Activation")
      edge.t2 <- which(subnet.Vis$edges$kind == "Interaction")
      edge.t3 <- which(subnet.Vis$edges$kind == "Inhibition")
      subnet.Vis$edges$width <- 3
      subnet.Vis$edges$arrows <- ""
      subnet.Vis$edges$color <- "gray"
      subnet.Vis$edges$arrows[edge.t1] <- "to"
      subnet.Vis$edges$color[edge.t1] <- "red"
      subnet.Vis$edges$arrows[edge.t3] <- "to"
      subnet.Vis$edges$color[edge.t3] <- "blue"
      subnet.Vis$edges$color[edge.t2] <- "gray"
      subnet.Vis$edges$arrows[edge.t2] <- NA

      visNetwork(subnet.Vis$nodes, subnet.Vis$edges, height = "1000px", width = "100%") %>%
        visLegend(addNodes = lnodes, addEdges = ledges) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visOptions(manipulation = TRUE, highlightNearest = TRUE) %>%
        visIgraphLayout()




    }
  })
  output$extraLegend <- renderImage({
    # Return a list containing the filename
    list(src = "./www/legend.png",
         contentType = 'image/png',
         width = 7977/7.95,
         height = 449/7.95,
         alt = "This is alternate text")
  }, deleteFile = F)
}
)

addimg <- function(type, id.mut, id.pos, id.neg, img.pos, img.neg, subnet.Vis) {
    # type
    type.node <- which(subnet.Vis$nodes$type == type)
    type.mut <- which(type.node %in% id.mut)
    if(!is.null(id.pos)) {
        type.pos <- which(type.node %in% id.pos)
        # pos mut
        pos.m1 <- intersect(type.pos, type.mut)
        if(length(pos.m1)!=0) subnet.Vis$nodes$image[type.node[pos.m1]] <-
        paste(img.pos, subnet.Vis$nodes$expclass[type.node[pos.m1]], ".mut.png", sep = "")
        # pos non-mut
        pos.m2 <- setdiff(type.pos, type.mut)
        if(length(pos.m2)!=0) subnet.Vis$nodes$image[type.node[pos.m2]] <-
        paste(img.pos, subnet.Vis$nodes$expclass[type.node[pos.m2]],".png", sep = "")
        
    }
    if(!is.null(id.neg)) {
        type.neg <- which(type.node %in% id.neg)
        
        # neg mut
        neg.m1 <- intersect(type.neg, type.mut)
        if(length(neg.m1)!=0) subnet.Vis$nodes$image[type.node[neg.m1]] <-
        paste(img.neg, subnet.Vis$nodes$expclass[type.node[neg.m1]],".mut.png", sep = "")
        # neg non-mut
        neg.m2 <- setdiff(type.neg, type.mut)
        if(length(neg.m2)!=0) subnet.Vis$nodes$image[type.node[neg.m2]] <-
        paste(img.neg, subnet.Vis$nodes$expclass[type.node[neg.m2]],".png", sep = "")
        
        
    }
    return(subnet.Vis)
}