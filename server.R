library(openxlsx)
library(visNetwork)
library(igraph)
library(biomaRt)
source("NetworkPipeline.R")

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
                                mutmatched = NULL)
  
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
    input$dataAll
  })
  
  
  
  
  ### Tour ###
  
  observeEvent(input$typetour, updateCheckboxInput(session, "separate", value = 0))
  
  observeEvent(inpt$showdata, )
  
  
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
  
  observeEvent(
    !global.data$showtab, shinyjs::disable("nets")
  )
  
  output$btnNet <- renderUI({
    if(input$separate==FALSE & global.data$showtab){
      #actionButton("nets", "Construct Patient Network")
    }
  })
  
  observeEvent(input$nets, {
    loadZaman()
    global.data$shownet <- TRUE
    ensembl <- useMart("ensembl",dataset="hsapiens_gene_ensembl")
    idmatch <- getBM(attributes=c('hgnc_symbol', 'entrezgene'), 
                     filters = 'hgnc_symbol', 
                     values = global.data$muts, 
                     mart = ensembl)
    m <- match(idmatch$entrezgene, V(Zaman)$EntrezID)
    gene.matched <- idmatch[which(!is.na(m)), 1] 
    global.data$unmatch <- setdiff(global.data$muts, gene.matched)
    global.data$mutmatched <- gene.matched
    if(!is.null(global.data$unmatch)) {
      global.data$unmatch <- global.data$unmatch[, 1]
    } 
    #global.data$unmatch <- setdiff(global.data$muts, global.data$muts)
    
  })
  
  #observeEvent(input$unmatchbtn, {
    
  #})
  
  
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
      fluidRow(
        box(title = "Input Data", solidHeader = TRUE, status = "primary", width = 12, 
            collapsible = TRUE,
            tabsetPanel(type = "tabs",
                        tabPanel("Drug-Target", dataTableOutput("displaytargets")),
                        tabPanel("Drug response", dataTableOutput("displaydss")),
                        tabPanel("Mutation", dataTableOutput("displaymut")),
                        tabPanel("Gene expression", dataTableOutput("displaygene.exp"))
            )),
        if(global.data$shownet) {
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
                column(width = 3, imageOutput("extraLegend")),
                column(width = 9, visNetworkOutput("network"))
              )
              )
          
        },
        bsModal("unmatchmodel", "Unmatched mutations", "unmatchbtn",
                size = "large",
                dataTableOutput("unmatch"))
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
    # positive gene expression
    id.pos <- which(V(subnet)$exp >= 0)
    breaks.tmp <- c(0, max.exp/6, max.exp/3,max.exp/2,
                    max.exp/6*4, max.exp/6*5, max.exp)
    V(subnet)$expclass[id.pos] <- as.numeric(cut(V(subnet)$exp[id.pos], 
                                                 breaks = breaks.tmp, 
                                                 include.lowest = TRUE))
    # negative gene expression
    if(min.exp<0){
      id.neg <- which(V(subnet)$exp < 0)
      breaks.tmp <- c(0, min.exp/6, min.exp/3, min.exp/2,
                      min.exp/6*4, min.exp/6*5, min.exp)
      V(subnet)$expclass[id.neg] <- as.numeric(cut(V(subnet)$exp[id.neg], 
                                                   breaks = breaks.tmp, 
                                                   include.lowest = TRUE))
    }
    
    print(V(subnet)$exp)
    print(V(subnet)$expclass)
    subnet.Vis <- toVisNetworkData(subnet)
    
    # node shape
    subnet.Vis$nodes$shape <- "image"
    subnet.Vis$nodes$image <- "/kinase.png"
    
    # drug node
    d.node <- match(drugs, subnet.Vis$nodes$label)
    subnet.Vis$nodes$image[d.node] <- "/drug.png"
    # type "other"
    other.node <- which(subnet.Vis$nodes$type == "other")
    other.pos <- which(other.node %in% id.pos)
    if(length(other.pos)!=0) subnet.Vis$nodes$image[other.node[other.pos]] <- 
      paste("/other/other1.", subnet.Vis$nodes$expclass[other.node[other.pos]],".png", sep = "")
    
    other.neg <- which(other.node %in% id.neg)
    if(length(other.neg)!=0) subnet.Vis$nodes$image[other.node[other.neg]] <- 
      paste("/other/other2.", subnet.Vis$nodes$expclass[other.node[other.neg]],".png", sep = "")
    
    # type "cytokine"
    cytokine.node <- which(subnet.Vis$nodes$type == "cytokine")
    cytokine.pos <- which(cytokine.node %in% id.pos)
    if(length(cytokine.pos)!=0) subnet.Vis$nodes$image[cytokine.node[cytokine.pos]] <- 
      paste("/cytokine/cytokine1.", subnet.Vis$nodes$expclass[cytokine.node[cytokine.pos]],".png", sep = "")
    
    cytokine.neg <- which(cytokine.node %in% id.neg)
    if(length(cytokine.neg)!=0) subnet.Vis$nodes$image[cytokine.node[cytokine.neg]] <- 
      paste("/cytokine/cytokine2.", subnet.Vis$nodes$expclass[cytokine.node[cytokine.neg]],".png", sep = "")
    
    # type "growth factor"
    gf.node <- which(subnet.Vis$nodes$type == "growth factor")
    gf.pos <- which(gf.node %in% id.pos)
    if(length(gf.pos)!=0) subnet.Vis$nodes$image[gf.node[gf.pos]] <- 
      paste("/cytokine/cytokine1.", subnet.Vis$nodes$expclass[gf.node[gf.pos]],".png", sep = "")
    
    gf.neg <- which(gf.node %in% id.neg)
    if(length(gf.neg)!=0) subnet.Vis$nodes$image[gf.node[gf.neg]] <- 
      paste("/cytokine/cytokine2.", subnet.Vis$nodes$expclass[gf.node[gf.neg]],".png", sep = "")
    
    # type "enzyme"
    enzyme.node <- which(subnet.Vis$nodes$type == "enzyme")
    enzyme.pos <- which(enzyme.node %in% id.pos)
    if(length(enzyme.pos)!=0) subnet.Vis$nodes$image[enzyme.node[enzyme.pos]] <- 
      paste("/enzyme/enzyme1.", subnet.Vis$nodes$expclass[enzyme.node[enzyme.pos]],".png", sep = "")
    
    enzyme.neg <- which(enzyme.node %in% id.neg)
    if(length(enzyme.neg)!=0) subnet.Vis$nodes$image[enzyme.node[enzyme.neg]] <- 
      paste("/enzyme/enzyme2.", subnet.Vis$nodes$expclass[enzyme.node[enzyme.neg]],".png", sep = "")
    
    # type "transcription regulator"
    tr.node <- which(subnet.Vis$nodes$type == "transcription regulator")
    tr.pos <- which(tr.node %in% id.pos)
    if(length(tr.pos)!=0) subnet.Vis$nodes$image[tr.node[tr.pos]] <- 
      paste("/tr/tr1.", subnet.Vis$nodes$expclass[tr.node[tr.pos]],".png", sep = "")
    
    tr.neg <- which(tr.node %in% id.neg)
    if(length(tr.neg)!=0) subnet.Vis$nodes$image[tr.node[tr.neg]] <- 
      paste("/tr/tr2.", subnet.Vis$nodes$expclass[tr.node[tr.neg]],".png", sep = "")
    
    # type "transporter"
    transporter.node <- which(subnet.Vis$nodes$type == "transporter")
    transporter.pos <- which(transporter.node %in% id.pos)
    if(length(transporter.pos)!=0) subnet.Vis$nodes$image[transporter.node[transporter.pos]] <- 
      paste("/transporter/transporter1.", subnet.Vis$nodes$expclass[transporter.node[transporter.pos]],".png", sep = "")
    
    transporter.neg <- which(transporter.node %in% id.neg)
    if(length(transporter.neg)!=0) subnet.Vis$nodes$image[transporter.node[transporter.neg]] <- 
      paste("/transporter/transporter2.", subnet.Vis$nodes$expclass[transporter.node[transporter.neg]],".png", sep = "")
    
    
    # type "kinase"
    kinase.node <- which(subnet.Vis$nodes$type == "kinase")
    kinase.pos <- which(kinase.node %in% id.pos)
    if(length(kinase.pos)!=0) subnet.Vis$nodes$image[kinase.node[kinase.pos]] <- 
      paste("/kinase/kinase1.", subnet.Vis$nodes$expclass[kinase.node[kinase.pos]],".png", sep = "")
    
    kinase.neg <- which(kinase.node %in% id.neg)
    if(length(kinase.neg)!=0) subnet.Vis$nodes$image[kinase.node[kinase.neg]] <- 
      paste("/kinase/kinase2.", subnet.Vis$nodes$expclass[kinase.node[kinase.neg]],".png", sep = "")
    
    # type "ligand-dependent nuclear receptor"
    ligand.node <- which(subnet.Vis$nodes$type == "ligand-dependent nuclear receptor")
    ligand.pos <- which(ligand.node %in% id.pos)
    if(length(ligand.pos)!=0) subnet.Vis$nodes$image[ligand.node[ligand.pos]] <- 
      paste("/ligand/ligand1.", subnet.Vis$nodes$expclass[ligand.node[ligand.pos]],".png", sep = "")
    
    ligand.neg <- which(ligand.node %in% id.neg)
    if(length(ligand.neg)!=0) subnet.Vis$nodes$image[ligand.node[ligand.neg]] <- 
      paste("/ligand/ligand2.", subnet.Vis$nodes$expclass[ligand.node[ligand.neg]],".png", sep = "")
    
    
    # type "Transmembrane receptor"
    mr.node <- which(subnet.Vis$nodes$type == "transmembrane receptor")
    mr.pos <- which(mr.node %in% id.pos)
    if(length(mr.pos)!=0) subnet.Vis$nodes$image[mr.node[mr.pos]] <- 
      paste("/mr/mr1.", subnet.Vis$nodes$expclass[mr.node[mr.pos]],".png", sep = "")
    
    mr.neg <- which(mr.node %in% id.neg)
    if(length(mr.neg)!=0) subnet.Vis$nodes$image[mr.node[mr.neg]] <- 
      paste("/mr/mr2.", subnet.Vis$nodes$expclass[mr.node[mr.neg]],".png", sep = "")
    
    
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
    visNetwork(subnet.Vis$nodes, subnet.Vis$edges, height = "100%", width = "100%") %>%
      visNodes(shapeProperties = list(useBorderWithImage = FALSE)) %>%
      visOptions(manipulation = TRUE, highlightNearest = TRUE)
      # visLegend(addEdges = ledges,
      #           addNodes = lnodes,
      #           position = "right", useGroups = FALSE, ncol = 2)
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
    list(src = "./www/leg1.png",
         contentType = 'image/png',
         width = 1068/3.5,
         height = 1216/3.5,
         alt = "This is alternate text")
  }, deleteFile = F)
}
)