library(shiny)
upload_data_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "upload-body",
        fluidRow(
            column(4,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Upload data",
                       status = "primary",
                       collapsible = TRUE,
                       fileInput(ns("otuda"),
                                 label = "Abundance profile (.txt or .biom):",
                                 accept = c(".txt", ".biom")
                       ),
                       fileInput(ns("metada"),
                                 label = "Metadata file (.txt):",
                                 accept = c(".txt")
                       ),
                       fileInput(ns("treeda"),
                                 label = "(Optional) Phylogenetic tree (.tre or .nwk):",
                                 accept = c(".tre", ".nwk")
                       ),
                       fluidRow(
                           column(4, 
                                  actionButton(ns("btn"), "Submit")),
                           column(4,
                                  downloadButton(ns("example"), "Example Data")),
                           column(4,
                                  downloadButton(ns("tutorial"), "Tutorial"))
                       )
                   )
                   
            ),
            column(8,
                   shinyjs::hidden(
                       div(id = "hiddenbox",
                           shinydashboardPlus::box(
                               width = NULL,
                               title = "Data preview",
                               status = "success",
                               solidHeader = FALSE,
                               collapsible = TRUE,
                               verbatimTextOutput(ns("mp_print"))
                           )
                       )
                   )
            )
        )
        
    )
    return(res)
}


upload_data_mod <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      #load packages
      observe({
        #if(input$btn > 0) {
          library(DT)
          library(MicrobiotaProcess)
          library(phyloseq)
          library(ggplot2)
          library(gghalves)
          library(dplyr) 
          library(picante)
          library(ggside)
          library(Maaslin2) 
          library(ggupset) 
          library(ggtree)
          library(ggstatsplot)
          library(ggtreeExtra)
          library(tidyr)
          library(Rtsne)
      #}
      })
      
      #download example data 
      output$example <- downloadHandler(
        filename = function() {
          "CMAP_example_data.zip"
        },
        content = function(filename) {
          zip(filename, "data/example_data_20211016", flags = "-r9Xj")
        },
        contentType = "application/zip"
      )
      
      #download tutorial
      output$tutorial <- downloadHandler(
        filename = function() {
          "CMAP_Tutorial.zip"
        },
        content = function(filename) {
          zip(filename, "data/CMAP_Tutorial.pdf", flags = "-r9Xj")
        },
        contentType = "application/zip"
      )
      
      # output$contents <- renderTable({
      #   req(input$file1)
      #   df <- read.csv(input$file1$datapath,
      #                  header = input$header,
      #                  sep = input$sep,
      #                  quote = input$quote)
      # })
      #render table of update files 
      #output$update_files <- renderTable(c(input$otuda, input$metada))
      
      #load tree
      observe({
        if (!is.null(input$treeda)) {
          treeda <- ape::read.tree(input$treeda$datapath)
          
        } else {
          treeda <- NULL
        }
        saveRDS(treeda, "data/treeda.rds")
      })
      
      #covert data to a MPSE class
      mpse <- eventReactive(input$btn, {
        req(input$otuda, input$metada)
        otuda_ext <- tools::file_ext(input$otuda$datapath)
        if (otuda_ext == "txt") {
          
          mpse <- tryCatch(
            calm_upload_plain(
              input$otuda$datapath,
              input$metada$datapath,
              NULL
            ),
            error = function(e) e
          )
        } else if (otuda_ext == "biom") {
          mpse <- tryCatch({
            calm_upload_biom(
              input$otuda$datapath,
              input$metada$datapath,
              NULL
            )
            
          },
          error = function(e) e
          )
        }
        
        #mpse %<>% mp_rrarefy
        
        if (!inherits(mpse, "MPSE")) {
          showNotification("An error occurred while parsing the file",
                           type = "error"
          )
        }
        return(mpse)
      })
      
      # ----show hiddenbox----
      observeEvent(input$btn, {
        shinyjs::show(id = "hiddenbox")
      })
      
      output$mp_print <- renderPrint({
          mpse() %>%  as_tibble
          # mpse() %>% 
          #     mp_stat_taxa(action = "add") %>% 
          #     select(!StatTaxaInfo) %>% 
          #     arrange(desc(Abundance)) %>% as_tibble
      })
      
      # output$mp_print <- renderPrint({
      #   input$btn
      #   req(input$btn) #to prevent print at first lauch
      #   isolate(mpse() %>%  as_tibble)
      # })
      
      # #get MP summary
      # mpsum <- reactive({
      #   req(inherits(mpse(), "MPSE"))
      #   mpse() %>%
      #     mp_stat_taxa(.abundance = Abundance, action = "get") %>%
      #     rename(Counts = TotalNumByAbundanceEachTaxonomy) %>%
      #     distinct(Sample, Counts) %>%
      #     arrange(Counts)
      # })
      # 
      # #Render overview
      # output$overview <- renderUI({
      #   req(inherits(mpsum(), "tbl"))
      #   shinydashboardPlus::box(
      #     width = 12,
      #     title = "Library size overview",
      #     status = "success",
      #     collapsible = TRUE,
      #     div(
      #       class = "overview_plot",
      #       style = "height:600px; width:95%; overflow:scroll;",
      #       plotOutput(ns("plot"), width = "95%", height = paste0(nrow(mpsum()) * 20, "px"))
      #     )
      #   )
      # })
      # 
      # 
      # output$summary <- renderUI({
      #   req(inherits(mpse(), "MPSE"))
      #   taxa <- mp_extract_taxonomy(mpse()) %>% names()
      #   summary_div <- tagList(
      #     div(
      #       class = "summary",
      #       style = "width: 40%; float: left;",
      #       tags$b("OTU number: "),
      #       tags$b("OTU annotation: "),
      #       tags$b("Total read counts: "),
      #       tags$b("Phylogenetic tree uploaded: "),
      #       tags$b("Number of samples in metadata: ")
      #     ),
      #     div(
      #       class = "summary",
      #       style = "width: 60%; float: left;",
      #       tags$b(nrow(mp_extract_feature(mpse()))),
      #       tags$b(paste0(taxa[-1], collapse = ";")),
      #       tags$b(sum(mpsum()[[2]])),
      #       tags$b(!is.null(input$treeda)),
      #       tags$b((nrow(mpsum())))
      #     )
      #   )
      #   shinydashboardPlus::box(
      #     width = 12,
      #     title = "Text summary",
      #     status = "success",
      #     collapsible = TRUE,
      #     summary_div
      #   )
      #   
      # })
      # 
      # #plot overview
      # output$plot <- renderPlot(res = 90, {
      #   req(inherits(mpsum(), "tbl"))
      #   x_end <- max(mpsum()$Counts) * 1.2
      #   p <- ggplot(mpsum(), aes(y = reorder(Sample, Counts), x = Counts)) +
      #     geom_point(color = "#FD9347", size = 3) +
      #     # ggrepel::geom_text_repel(aes(label = Counts), size = 5) +
      #     geom_text(aes(label = Counts), hjust = -0.2,  size = 5) +
      #     ylab("Sample") +
      #     scale_x_continuous(limits = c(0, x_end)) +
      #     theme_classic() +
      #     theme(text = element_text(size = 16, family = "serif"))
      #   return(p)
      # })
      
      return(mpse)
    }
  )
}
