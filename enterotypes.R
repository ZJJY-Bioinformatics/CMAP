biotype_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Enterotypes",
                       status = "warning",
                       collapsible = TRUE,
                       fluidRow(
                           column(6,
                                  pickerInput(ns("group"),
                                              "Group:",
                                              NULL
                                  )

                           ),
                           column(6,
                                  pickerInput(ns("level"),
                                              "Levels:",
                                              NULL
                                  )
                           )
                       ),
                       fluidRow(
                           column(6,
                                  numericInput(ns("k"),
                                               "Number of clusters:",
                                               value = 3,
                                               min = 1,
                                               max = 10
                                  )
                           )
                       ),
                       actionButton(ns("btn"), "Submit")
                   ),
                  
                   tabBox(width = NULL,
                          tabPanel(h5("Graphics Options"),
                                   uiOutput(ns("CH"))),
                          # tabPanel(
                          #     h5("Color"),
                          #     fluidRow(
                          #         column(6,
                          #                uiOutput(ns("color")))
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
                       img(src="biotype.png",
                           align = "center",
                           width = "100%")
                   )
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("plot"), width = '600px', height = '1000px'),
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

biotype_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            observe({
                req(inherits(mpse, "MPSE"))
                lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                updatePickerInput(session, "group",
                                  choices = group,
                                  selected = tail(names(lev[lev == 2]), 1)
                )
                tax <- mp_extract_taxonomy(mpse) %>% names
                updatePickerInput(session, "level",
                                  choices = tax,
                                  selected = tail(tax)
                )
                
            })
            
            result <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                level <- isolate({
                    input$level
                })
                k_mer <- isolate({
                    input$k
                })
                sample_df <- mpse %>%  
                    mp_cal_abundance %>%
                    mp_extract_abundance(taxa.class = !!sym(level), force = TRUE, rmun = TRUE) %>%
                    tidyr::unnest(col = RareAbundanceBySample)
                
                sample_table <- sample_df[,c("label", "Sample","RelRareAbundanceBySample")] %>%
                    spread(Sample, RelRareAbundanceBySample) %>% as.data.frame 
                
                sample_table <- sample_table[sample_table$label != "Others",]
                row.names(sample_table) <- sample_table[[1]]
                sample_table <- sample_table[-1]
                
                #PAM
                jsd =dist.JSD(sample_table)
                ch=CH.index(sample_table,jsd,k=1:10)
                k=which(ch==max(ch))
                
                result <- biotyper(sample_table,k_mer, 
                                   distance.jsd=jsd, 
                                   manalysis=TRUE, 
                                   plot=FALSE)
                result$k <- k
                # print(k)
                # validate(
                #     need(result$k == 0, paste0("Optimal number of clusters is: ", result$k)
                #     )
                # )
                
                return(result)
            })
            
            p_biotype <- reactive({
                req(result())
                
                group <- isolate({
                    input$group
                })
                
                result <- result()
                data_plot <- data.frame(result$BET$ls, biotypes = result$biotypes)
                data_plot$sample.names <- data_plot %>% rownames
                
                centroid <- aggregate(cbind(data_plot$CS1,data_plot$CS2) ~ biotypes,
                                      data = data_plot,
                                      FUN = mean)
                
                pca.results <- data_plot %>%  left_join(centroid, by = "biotypes")
                mp_meta <- mpse %>% mp_extract_sample
                pca.results %<>%  left_join(mp_meta , by = c("sample.names" = "Sample"))
                pca.results$biotypes <-  as.character(pca.results$biotypes)
                
                sankey_data <- pca.results %>% 
                    select(biotypes,sample.names,!!sym(group)) %>% 
                    group_by(biotypes, !!sym(group)) %>%
                    count
                
                p1 <- ggplot(pca.results,aes(x=CS1,y=CS2))+
                    geom_vline(xintercept = 0,lty="dashed")+
                    geom_hline(yintercept = 0,lty="dashed")+
                    geom_point(aes(color=biotypes))+
                    geom_segment(aes(xend=V1,yend=V2,color=biotypes),
                                 show.legend = FALSE)+
                    bca.ggtheme +
                    scale_colour_manual(values = bca.color)+
                    geom_label(aes(label = biotypes, fill = biotypes, x = V1, y = V2), size = 5,
                               show.legend = FALSE,
                               color="white") +
                    scale_fill_manual(values = bca.color) +
                    ggrepel::geom_text_repel(aes(label = sample.names, color = biotypes),size=3, segment.size=0.25) + 
                    theme(legend.position = "top") + 
                    labs(x = "PC1", y = "PC2") 
                
                p2 <- ggplot(data = sankey_data,
                             aes(axis1 = !!sym(group), axis2 =  biotypes,y = n)) +
                    scale_x_discrete(limits = c(group, "biotypes"), expand = c(.02,.05)) +
                    xlab(NULL) + ylab("Count") + 
                    geom_alluvium(aes(fill = biotypes),) +
                    scale_fill_manual(values = bca.color) + 
                    geom_stratum(width = 1/5) +
                    geom_text(stat = "stratum", aes(label = after_stat(stratum)))+ 
                    theme_minimal() + 
                    theme(legend.title = element_blank(),
                          axis.text.y = element_text(colour="black", size=14),
                          axis.text.x = element_text(colour="black", size=14),
                          axis.title = element_text(colour="black", size=14))
                
                p <- p1/p2
                return(p)
            })
                
                output$plot <- renderPlot({
                    req(p_biotype())
                    p_biotype()
                })
                
                output$downloadPlot <- downloadHandler(
                    filename = function(){
                        paste("biotype_plot", input$extPlot, sep='')},
                    content = function(file){
                        req(p_biotype())
                        ggsave(file, 
                               plot = p_biotype(), 
                               width = input$width_slider, 
                               height = input$height_slider,
                               dpi = input$dpi)
                    })
                
                output$downloadTable <- downloadHandler(
                    filename = function(){ "biotype_Data.csv" },
                    content = function(file){
                        req(p_biotype())
                        table <- p_biotype()$data
                        write.csv(table, 
                                  file,
                                  row.names = FALSE)
                    })
                output$CH <- renderUI({
                    req(p_biotype())
                    navPills(
                        color = "gray",
                        navPillsItem(
                            selected = TRUE,
                            left = "Optimal number of clusters", 
                            color = "red",
                            icon = icon("fa fa-angle-down"),
                            right = result()$k
                        )#close navPillsItem
                    )#close navPills
                })
            
        })
    }
    


