cst_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Vaginal CSTs analysis",
                       status = "warning",
                       collapsible = TRUE,
                       p("Based on a cross-sectional study of 394 healthy women at reproductive ages, Ravel et al.classified the human vaginal microbial communities (HVMC) into five community state types (CSTs)."),
                       br(),
                       p("Powered by VALENCIA") ,
                       
                       fluidRow(
                           column(6,
                                  actionButton(ns("btn"), "Run"),
                           ),
                           column(6,
                                  #style=list("padding-right: 5px;"),
                                  bs_modal(
                                      id = "cst_markdown",
                                      title = "CST architecture",
                                      body = includeMarkdown("data/CST_help.md")
                                  ),
                                  bs_button("CST architecture info") %>%
                                      bs_attach_modal(id_modal = "cst_markdown")
                           )
                       )
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Download"),
                                   fluidRow(
                                       column(width = 6,
                                              style=list("padding-right: 5px;"),
                                              numericInput(ns("width_slider"), "Width:", 10,1, 20)
                                       ),
                                       column(width = 6,
                                              style=list("padding-left: 5px;"),
                                              numericInput(ns("height_slider"), "Height:", 8, 1, 20)
                                       )
                                   ),
                                   fluidRow(
                                       column(width = 6,
                                              style=list("padding-right: 5px;"),
                                              selectInput(inputId = ns('extPlot'),
                                                          label = 'Output format',
                                                          choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff')
                                              ),
                                       ),
                                       column(width = 6,
                                              style=list("padding-left: 5px;"),
                                              numericInput(ns("dpi"), "DPI:", 300, 100, 600)
                                       )
                                   ),
                                   fluidRow(
                                       column(width = 6,
                                              downloadButton(ns("downloadPlot"), "Plot(not available)")),
                                       column(width = 6,
                                              downloadButton(ns("downloadCst"), "CSTs Table"))
                                   )
                          )
                   )
            ),
            column(9,
                   reactable::reactableOutput(outputId = ns("table"))
            )
        )
    )
    return(res)
}


cst_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            
            cst_res <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
             
                # species level for key phylotypes 
                s_abunt <- mpse %>%
                    mp_cal_abundance(.abundance=Abundance, action="add", force = T) %>%
                    mp_extract_tree() %>%
                    tidyr::unnest(AbundanceBySample) %>%
                    dplyr::filter(nodeClass == "Speies")
                
                s_abunt_spr <- s_abunt %>% select(label,  Sample, Abundance) %>% spread(label, Abundance)
                speies_names <- colnames(s_abunt_spr)
                key_phylotypes <- c("Lactobacillus", "Gardnerella", "Prevotella", "Atopobium", "Sneathia", "Mobiluncus")
                key_column <- speies_names[grepl(paste(key_phylotypes, collapse = "|"), speies_names)]
                key_abun <- s_abunt_spr[,key_column]
                names_new <- key_abun %>% colnames %>% strsplit("__") %>%  sapply("[", 2) 
                colnames(key_abun) <- names_new
                key_abun <- key_abun[,colnames(key_abun) != "un_g"]
                key_phy_abun <- cbind(s_abunt_spr[,1], key_abun)
                colnames(key_phy_abun)[1] <- "sampleID"
                
                #other taxa are summarized to the genus
                g_abunt <- mpse %>%
                    mp_cal_abundance(.abundance=Abundance, action="add", force = T) %>%
                    mp_extract_tree() %>%
                    tidyr::unnest(AbundanceBySample) %>%
                    dplyr::filter(nodeClass == "Genus")
                g_abunt_spr <- g_abunt %>% select(label,  Sample, Abundance) %>% spread(label, Abundance)


                
                #merge all
                read_count <- mpse %>%
                    mp_extract_assays(.abundance=Abundance) %>% colSums() %>% as.data.frame()
                clo2 <- data.frame("sampleID" = rownames(read_count), "read_count" = read_count[[1]])
                clo2  %<>% left_join(key_phy_abun, by = c("sampleID" = "sampleID"))
                clo2  %<>% left_join(g_abunt_spr, by = c("sampleID" = "Sample"))
                
                #calling Valencia
                write.csv(clo2, "./cst_input.csv", row.names = FALSE)
                use_python("/usr/bin/python3")
                source_python("/app/data/Valencia1.py")
                res <- cst_run("/app/data/CST_centroids_012920.csv", "./cst_input.csv")

                file.remove("./cst_input.csv")
                #return(clo2)
                return(res)
            })
            
            observeEvent(cst_res, {
                output$table <- reactable::renderReactable({
                    reactable::reactable(cst_res())
                })
            })
            
            output$downloadCst <- downloadHandler(
                filename = function(){"CSTs_result.csv"},
                content = function(file){
                    req(inherits(mpse, "MPSE"))
                    write.csv(cst_res(), 
                              file,
                              row.names = FALSE)
                })
            
        }
    )
}

