buglist_tSNE_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "t-SNE Analysis",
                       status = "warning",
                       collapsible = TRUE,
                       fluidRow(
                           column(6,
                                  numericInput(ns("perplexity"), "Perplexity:", 0.5, 0, 2, 0.1)
                           ),
                           column(6,
                                  numericInput(ns("theta"), "Theta:", 0.5, 0, 2, 0.1)
                           )
                       ),
                       fluidRow(
                           column(width = 6,
                                  style=list("padding-right: 5px;"),
                                  pickerInput(ns("taxonomy"), "taxonomy:", NULL)
                           ),
                           column(width = 6,
                                  style=list("padding-left: 5px;"),
                                  numericInput(ns("topn"), "Top N:", 5, 5, 50)
                           )
                       ),
                       actionButton(ns("btn"), "Submit")
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Graphics Options"),
                                   tags$b("Whether to show:"),
                                   prettyCheckbox(
                                       inputId = ns("sample_label"),
                                       label = "Label",
                                       value = FALSE,
                                       status = "danger",
                                       shape = "curve"
                                   )
                                   #uiOutput(ns("box_order"))
                                   
                          ),
                          # tabPanel(
                          #     h5("Color"),
                          #     fluidRow(
                          #         column(6,
                          #                uiOutput(ns("color"))
                          #     )
                          # ),
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
                                              downloadButton(ns("downloadPlot"), "Download Plot")),
                                       column(width = 6,
                                              downloadButton(ns("downloadTable"), "Download Table"))
                                   )
                          )
                   ),
                   shinydashboard::box(
                       title = "Example Plot",
                       width = NULL,
                       img(src="tSNE_plot.png",
                           align = "center",
                           width = "100%")
                   )
                   
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("plot"), width = '900px', height = '600px'),
                       operation = c("enable", "disable", "destroy", "save", "load"),
                       options = list(
                           minHeight = 300, maxHeight = 900,
                           minWidth = 300, maxWidth = 1200
                       )
                   ))
        )
        
        
    )
    return(res)
}

buglist_tSNE_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            observe({
                req(inherits(mpse, "MPSE"))
                tax <- mp_extract_taxonomy(mpse) %>% names()
                updatePickerInput(session, "taxonomy",
                                  choices = tax,
                                  selected = tail(tax)
                )
            })
            mp_tsne <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                taxonomy <- isolate({
                    input$taxonomy
                })
                
                perplexity <- isolate({
                    input$perplexity
                })
                
                g_abunt <- mpse %>%
                    mp_cal_abundance(.abundance=Abundance, action="add", force = T) %>%
                    mp_extract_tree() %>%
                    tidyr::unnest(AbundanceBySample) %>%
                    dplyr::filter(nodeClass == taxonomy)
                g_abunt_spr <- g_abunt %>% select(label,  Sample, Abundance) %>% spread(label, Abundance) %>% as.data.frame
                rownames(g_abunt_spr) <- g_abunt_spr$Sample
                g_abunt_spr <- g_abunt_spr[,-1]
                
                #definition of "other"
                most_abu <- g_abunt %>% select(label,  Sample, Abundance) %>% group_by(Sample) %>% slice(which.max(Abundance)) %>% ungroup
                other <- most_abu$label %>% table() %>% as.data.frame() %>% slice_max(Freq, n = input$topn) %>% select(".") 
                other <- other[[1]] %>% as.character
                #most_abu[!most_abu$label %in% other, "label"] <- "other"
                
                most_abu$most_seq_ohter <- most_abu$label
                most_abu[!most_abu$most_seq_ohter %in% other, "most_seq_ohter"] <- NA
                
                
                tsne_out <- Rtsne(as.matrix(g_abunt_spr),perplexity=perplexity,theta=0.5)
                tsne_plot <- data.frame(x = tsne_out$Y[,1], y = tsne_out$Y[,2])
                tsne_plot <- tsne_plot %>% cbind(most_abu)
                
                return(tsne_plot)
            })
            
            p_tsne <- reactive({
                req(mp_tsne())
                input$btn 
                
                tsne_data <- data.frame(x = mp_tsne()$Y[,1], y = mp_tsne()$Y[,2])#, col =metada$MetS) 
                
                p <- ggplot(mp_tsne(), aes(x = x, y = y)) +
                    geom_point(size = 3, aes(color = most_seq_ohter)) +
                    scale_color_manual(values = define_color2) +
                    # geom_label(aes(x_pos, y_pos, label = taxon),
                    #            data = df_label,
                    #            label.r = unit(0.2, 'lines'),
                    #            alpha = .5,
                    #            size = 5) +
                    theme_bw() +
                    define_theme +
                    labs(x = "t-SNE1", y = "t-SNE2") + 
                    guides(colour = guide_legend(override.aes = list(size = 5))) +
                    geom_segment(aes(x = min(x) - 10,
                                     y = min(y) - 10,
                                     xend = min(x) + 15,
                                     yend = min(y) - 10),
                                 colour = "black",
                                 size = 1,
                                 arrow = arrow(length = unit(0.3,"cm"))) +
                    geom_segment(aes(x = min(x) - 10,
                                     y = min(y) - 10,
                                     xend = min(x) - 10,
                                     yend = min(y) + 15),
                                 colour = "black",
                                 size = 1,
                                 arrow = arrow(length = unit(0.3,"cm"))) 
                return(p)
            })
            
            
            output$plot <- renderPlot({
                req(p_tsne())
                p_tsne()
            })
            
            # output$color <- renderUI({
            #     req(mp_pcoa())
            #     input$btn
            #     color_list()
            # })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("tSNE_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_tsne())
                    ggsave(file, 
                           plot =p_tsne(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "tsne_Data.csv" },
                content = function(file){
                    req(p_tsne())
                    table <- p_tsne()$data 
                    write.csv(table, 
                              file,
                              row.names = FALSE)
                })
            
            
            
        }
    )
}