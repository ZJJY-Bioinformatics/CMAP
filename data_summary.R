linebreaks <- function(n){HTML(strrep(br(), n))}
error_info <- "Invalid operation: Attempt to delete all data"

data_summary_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = 12,
                       title = "Data rarefying",
                       status = "primary",
                       collapsible = TRUE,
                       # numericInput(ns("abun"),
                       #              "Minimum counts of features:",
                       #              value = 1
                       # ),
                       # numericInput(ns("prop"),
                       #              "Prevalence in samples (%):",
                       #              value = 5,
                       #              max = 100,
                       #              min = 0
                       # ),
                       # radioButtons(ns("rarefying"), "Data rarefying",
                       #              c(
                       #                  "Minimum" = "min",
                       #                  "Default" = "default",
                       #                  "Do not" = "no"
                       #              ),
                       #              inline = T
                       # ),
                       # conditionalPanel(
                       #     "input.rarefying == 'default'",
                       #     ns = ns,
                       #     numericInput(ns("default"),
                       #                  "Default size:",
                       #                  value = 0
                       #     )
                       # ),
                       #fluidRow(),
                       p("Rarefied species richness for community ecologists."),
                       actionButton(ns("feature"), "Submit")
                       # actionBttn(
                       #     inputId = ns("overview_btn"),
                       #     label = "Show Libray Size",
                       #     style = "jelly", 
                       #     color = "primary"
                       # )
                   )
                   # shinydashboardPlus::box(
                   #     width = NULL,
                   #     title = "Data Summary(under construction)",
                   #     status = "primary",
                   #     collapsible = TRUE,
                   #     
                   #     #h3("Text Summary"),
                   #     # p(
                   #     #     "Text summary of uploaded data, inclouding:",
                   #     #     br(),
                   #     #     "OTU number; OTU annotation; Total read counts and Number of samples in metadata."
                   #     # ),
                   #     # #br(),
                   #     # #actionButton(ns("btn"), "Show Summary"),
                   #     # actionBttn(
                   #     #     inputId = ns("btn"),
                   #     #     label = "Show Summary",
                   #     #     style = "jelly", 
                   #     #     color = "primary"
                   #     # ),
                   #     # br(),
                   #     # br(),
                   #     # br(),
                   #     # # h3("Sample Table"),
                   #     # p("Displays group information in the sample."),
                   #     # #br(),
                   #     # #actionButton(ns("tb_btn"), "Show Sample Table"),
                   #     # actionBttn(
                   #     #     inputId = ns("tb_btn"),
                   #     #     label = "Show Sample Table",
                   #     #     style = "jelly", 
                   #     #     color = "primary"
                   #     # ),
                   #     # br(),
                   #     # br(),
                   #     # br(),
                   #     #h3("Libray Size"),
                   #     p("Libray Size of sample with bubble chart.",
                   #       br(),
                   #       br(),
                   #       "Suggestion: Perform Data Rarefying = Minimum in the Data Filter module 
                   #       if the count is extremely inconsistent between samples"
                   #     ),
                   #     #br(),
                   #     #actionButton(ns("overview_btn"), "Show Libray Size")
                   #     actionBttn(
                   #         inputId = ns("overview_btn"),
                   #         label = "Show Libray Size",
                   #         style = "jelly", 
                   #         color = "primary"
                   #     )
                   # )
            ),
            column(9,
                   # uiOutput(ns("summary")),
                   # uiOutput(ns("display_table")),
                   uiOutput(ns("overview"))
                   )
        )
    )
    return(res)
}

data_summary_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            
            mpse_filter <- reactiveValues(mpse = NULL)
            
            observe({
                req(inherits(mpse(), "MPSE"))
                mpse_filter$mpse <- mpse() #class(mpse_filter$mpse ) = "MPSE"
            })
            

            
            observeEvent(input$feature, {
                req(inherits(mpse_filter$mpse, "MPSE"))
                #req(inherits(mpse(), "MPSE"))
                #input$feature

                abun <- isolate({input$abun})
                prop <- isolate({input$prop})
                res <- tryCatch(
                    # mpse() %>%
                    #     mp_filter_taxa(.abundance = Abundance, abun, prop, TRUE),
                    mpse() %>% mp_rrarefy,
                    error = function(e) e
                )
                if (inherits(res, "MPSE")) {
                    mpse_filter$mpse  <- res
                } else {
                    showNotification(error_info, type = "warning")
                }

            })
            
            # mpsum <- reactive({
            #     req(inherits(mpse(), "MPSE"))
            #     if(input$feature) {
            #         mpse_filter$mpse %>%
            #             mp_stat_taxa(.abundance = Abundance, action = "get") %>%
            #             rename(Counts = TotalNumByAbundanceEachTaxonomy) %>%
            #             distinct(Sample, Counts) %>%
            #             arrange(Counts)
            #     }else{
            #         mpse() %>%
            #             mp_stat_taxa(.abundance = Abundance, action = "get") %>%
            #             rename(Counts = TotalNumByAbundanceEachTaxonomy) %>%
            #             distinct(Sample, Counts) %>%
            #             arrange(Counts)
            #     }
            #     
            # })
            
            mpsum <- reactive({
                req(inherits(mpse(), "MPSE"))
                if(input$feature) {
                    x <- mpse_filter$mpse %>%
                        mp_extract_assays(.abundance=RareAbundance) %>% colSums() %>% as.data.frame
                    colnames(x) <- "OTU"
                    x$Sample <- rownames(x)
                }else{
                    x <-  mpse() %>%
                        mp_extract_assays(.abundance=Abundance) %>% colSums() %>% as.data.frame
                    colnames(x) <- "OTU"
                    x$Sample <- rownames(x)
                }
                return(x)
            })
            
            #Render text summary
            # output$summary <- renderUI({
            #     req(input$btn)
            #     taxa <- mp_extract_taxonomy(mpse()) %>% names()
            #     summary_div <- tagList(
            #         div(
            #             class = "summary",
            #             style = "width: 40%; float: left;",
            #             tags$b("OTU number: "),
            #             tags$b("OTU annotation: "),
            #             tags$b("Total read counts: "),
            #             tags$b("Phylogenetic tree uploaded: "),
            #             tags$b("Number of samples in metadata: ")
            #         ),
            #         div(
            #             class = "summary",
            #             style = "width: 60%; float: left;",
            #             tags$b(nrow(mp_extract_feature(mpse()))),
            #             tags$b(paste0(taxa[-1], collapse = ";")),
            #             tags$b(sum(mpsum()[[2]])),
            #             tags$b(!is.null(input$treeda)),
            #             tags$b((nrow(mpsum())))
            #         )
            #     )
            #     shinydashboardPlus::box(
            #         width = 12,
            #         title = "Text summary",
            #         status = "success",
            #         collapsible = TRUE,
            #         summary_div
            #         # linebreaks(8),
            #     )
            #     
            # })
            
            #Render sample table
            # table <- reactive({
            #     req(input$tb_btn)
            #     sample_table <- mpse() %>% mp_extract_sample
            #     n <- names(sample_table)[sapply(sample_table, class) == "list"]
            #     sample_table %<>% select(-c(n))
            #     return(sample_table)
            # })
            # 
            # output$table <- renderDataTable({
            #     req(input$tb_btn)
            #     DT::datatable(
            #         table(),
            #         options = list(
            #             scrollX = TRUE,
            #             scrollY = "400px"
            #         )
            #     )
            # })
            # 
            # output$display_table <- renderUI({
            #     req(input$tb_btn)
            #     shinydashboardPlus::box(
            #         width = 12,
            #         status = "success",
            #         title = "Sample Details",
            #         collapsible = TRUE,
            #         DTOutput(ns("table"))
            #     )})
            
            # #Render overview
            output$overview <- renderUI({
                #req(input$feature)
                shinydashboardPlus::box(
                    width = 12,
                    title = "Library size overview",
                    status = "success",
                    collapsible = TRUE,
                    div(
                        class = "overview_plot",
                        style = "height:600px; width:95%; overflow:scroll;",
                        plotOutput(ns("plot"), width = "95%", height = paste0(nrow(mpsum()) * 20, "px"))
                    )
                )
            })
            
            output$plot <- renderPlot(res = 90, {
                req(inherits(mpsum(), "data.frame"))
                x_end <- max(mpsum()$OTU) * 1.2
                p <- ggplot(mpsum(), aes(y = reorder(Sample, OTU), x = OTU)) +
                    geom_point(color = "#FD9347", size = 3) +
                    # ggrepel::geom_text_repel(aes(label = Counts), size = 5) +
                    geom_text(aes(label = OTU), hjust = -0.2,  size = 5) +
                    ylab("Sample") +
                    scale_x_continuous(limits = c(0, x_end)) +
                    theme_classic() +
                    theme(text = element_text(size = 16, family = "serif"))
                return(p)
            })
            
        })
}