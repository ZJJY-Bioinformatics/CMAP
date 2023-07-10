diff_barplot_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Different barplot",
                       status = "warning",
                       collapsible = TRUE,
                       fluidRow(
                           column(6,
                                  numericInput(ns("lda"),
                                               "LDA score threshold:",
                                               value = 3)
                           ),
                           column(6,
                                  numericInput(ns("p_val"),
                                               "P value cutoff:",
                                               value = 0.05,
                                               step = 0.001)
                           )
                       ),
                       fluidRow(
                           column(6,
                                  pickerInput(ns("group"), "Group:", NULL)
                           ),
                           column(6,
                                  pickerInput(ns("level"),
                                              "Taxonomy level:",
                                              "all",
                                              #options = pickerOptions(noneSelectedText = "All"),
                                              choices = c("all","OTU","Kingdom", "Phylum","Class","Order","Family","Genus","Species"),
                                              multiple = TRUE)
                           )
                           # column(6,
                           #        pickerInput(ns("level"),
                           #                    "Taxonomy level:",
                           #                    choices = NULL)
                           # )
                       ),
                       
                       actionButton(ns("btn"), "Submit")
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
                                              downloadButton(ns("downloadPlot"), "Download Plot")),
                                       column(width = 6,
                                              downloadButton(ns("downloadTable"), "Download Table"))
                                   )
                          )
                   ),
                   shinydashboard::box(
                       title = "Example Plot",
                       width = NULL,
                       img(src="diff_taxa_barplot.png",
                           align = "center",
                           width = "100%")
                   )
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("diff_barplot"), width = '900px', height = '600px'),
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



diff_barplot_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            observe({
                req(inherits(mpse, "MPSE")) 
                lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                #taxa <- c("OTU",names(mp_extract_taxonomy(mpse))[-1])
                updatePickerInput(session, "group", choices = group,
                                  selected = tail(names(lev[lev == 2]), 1))
                # updatePickerInput(session, "level", choices = taxa,
                #                   selected = tail(taxa, 1))
            })
            mp_lefse <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$btn
                #level <- isolate({input$level})
                lda <- isolate({input$lda})
                p_val <- isolate({input$p_val})
                group <- isolate({input$group})
                
                mpse  %<>%
                    mp_cal_abundance( # for each samples
                        .abundance = RareAbundance
                    ) %>%
                    mp_cal_abundance( # for each groups 
                        .abundance=RareAbundance,
                        .group=!!sym(group) 
                    )
                
                
                mpse %<>% mp_diff_analysis(.abundance = RelRareAbundanceBySample,
                                           .group = !!sym(group),
                                           action = "add",
                                           tip.level = "Speies",
                                           first.test.alpha = p_val,
                                           second.test.alpha = p_val,
                                           ldascore = lda,
                                           force = FALSE)
                validate(
                    need(!is.null(mpse), "There are not significantly discriminative features !!"
                    )
                )
                
                return(mpse)
            })
            
            p_diffbar <- reactive({
                #req(inherits(mp_lefse(), "diffAnalysisClass"))
                req(inherits(mp_lefse(), "MPSE"))
                input$btn
                level <- isolate({input$level})
                #p <- mp_lefse() %>% ggdiffclade(linewd = 0.1)
                    p <- mp_lefse() %>%
                        mp_plot_diff_boxplot(
                            taxa.class = !!sym(level), # select the taxonomy level to display
                            group.abun = TRUE, # display the mean abundance of each group
                            removeUnknown = TRUE, # whether mask the unknown taxa.
                        ) %>%
                        set_diff_boxplot_color(
                            values = c(define_color3),
                            guide = guide_legend(title=NULL)
                        )
 
                return(p)
            })

            output$diff_barplot <- renderPlot({
                req(p_diffbar())
                p_diffbar()

            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("diff_taxa_barplot", input$extPlot, sep='')},
                content = function(file){
                    req(p_diffbar())
                    ggsave(file, 
                           plot = p_diffbar(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "diff_taxa_bar_Data.csv" },
                content = function(file){
                    req(p_diffbar())
                    table <- p_diffbar()$data
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })  

        }
    )
}


