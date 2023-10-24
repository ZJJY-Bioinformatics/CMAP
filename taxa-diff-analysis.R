p_adjust <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")


lefse_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "LEfSe Analysis",
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
                                  # pickerInput(ns("p_adj"),
                                  #             "P value adjust:",
                                  #             choices = p_adjust,
                                  #             selected = "fdr")
                                  pickerInput(ns("group"), "Group:", NULL)
                           ),
                           column(6,
                                  pickerInput(ns("level"),
                                              "Taxonomy level:",
                                              choices = NULL)
                           )
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
                       img(src="diff_taxa_plot.png",
                           align = "center",
                           width = "100%")
                   )
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("diff_plot"), width = '900px', height = '600px'),
                       operation = c("enable", "disable", "destroy", "save", "load"),
                       options = list(
                           minHeight = 300, maxHeight = 900,
                           minWidth = 300, maxWidth = 1200
                       )#close
                   )#close jqui
            )#close column
        )#close fluidRow
     
        # shinydashboardPlus::box(
        #     width = 12,
        #     title = "LEfSe Analysis",
        #     status = "warning",
        #     collapsible = TRUE,
        #     numericInput(ns("lda"),
        #                  "LDA score threshold:",
        #                  value = 3),
        #     numericInput(ns("p_val"),
        #                  "P value cutoff:",
        #                  value = 0.05,
        #                  step = 0.001),
        #     pickerInput(ns("p_adj"),
        #                 "P value adjust:",
        #                 choices = p_adjust,
        #                 selected = "fdr"),
        #     pickerInput(ns("level"),
        #                 "Taxonomy level:",
        #                 choices = NULL),
        #     pickerInput(ns("group"), "Group:", NULL),
        #     radioButtons(ns("layout"), "Graph layout:",
        #         c(
        #             "Box plot" = "box",
        #             "Clade plot" = "clade"
        #         ),
        #         inline = T
        #     ),
        #     actionButton(ns("btn"), "Submit")
        #     
        # ),
        # fluidRow(),
        # uiOutput(ns("ui"))
    )
    return(res)
}



lefse_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            observe({
                req(inherits(mpse, "MPSE")) 
                lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                taxa <- c("OTU",names(mp_extract_taxonomy(mpse))[-1])
                updatePickerInput(session, "group", choices = group,
                                  selected = tail(names(lev[lev == 2]), 1))
                updatePickerInput(session, "level", choices = taxa,
                                  selected = tail(taxa, 1))
            })
            # observeEvent(input$btn, {
            #     input$btn
            #     layout <- isolate({input$layout})
            #     if (layout == "clade") {
            #         options <- list(
            #             minHeight = 400, maxHeight = 1400,
            #             minWidth = 500, maxWidth = 1500
            #         )
            #         width <-  "1000px"
            #         height <- "900px"
            #     } else {
            #         options <- list(
            #             minHeight = 100, maxHeight = 900,
            #             minWidth = 300, maxWidth = 1200
            #         )
            #         width <-  "900px"
            #         height <- "600px"
            #     }
            #     output$ui <- renderUI({
            #         jqui_resizable(
            #             plotOutput(ns("plot"), width = width, height = height),
            #             operation = c("enable", "disable", "destroy", "save", "load"),
            #             options = options
            #         )
            #     })
            # })
            mp_lefse <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$btn
                level <- isolate({input$level})
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
                
                # mp_lefse <- mpse %>%
                #     mp_diff_analysis(.abundance = RareAbundance,
                #                      .group = !!sym(group),
                #                      action = "get",
                #                      tip.level = !!level,
                #                      first.test.alpha = p_val,
                #                      second.test.alpha = p_val,
                #                      ldascore = lda,
                #                      force = TRUE)
                
                mpse %<>% mp_diff_analysis(.abundance = RelRareAbundanceBySample,
                                           .group = !!sym(group),
                                           action = "add",
                                           tip.level = !!level,
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
            
            p_diffclade <- reactive({
                #req(inherits(mp_lefse(), "diffAnalysisClass"))
                req(inherits(mp_lefse(), "MPSE"))
                
                input$btn
                #p <- mp_lefse() %>% ggdiffclade(linewd = 0.1)
                p <- mp_lefse() %>%
                    mp_plot_diff_cladogram(
                        label.size = 2.5,
                        hilight.alpha = .3,
                        bg.tree.size = .5,
                        bg.point.size = 2,
                        bg.point.stroke = .25
                    ) + 
                    scale_fill_diff_cladogram( # set the color of different group.
                        values = c(define_color3)
                    )
                
                # p2 <- mp_lefse() %>%
                #     mp_plot_diff_boxplot(
                #         #taxa.class = Speies, # select the taxonomy level to display
                #         group.abun = TRUE, # display the mean abundance of each group
                #         removeUnknown = TRUE, # whether mask the unknown taxa.
                #     ) %>%
                #     set_diff_boxplot_color(
                #         values = c(define_color3),
                #         guide = guide_legend(title=NULL)
                #     )
 
                return(p)
            })
            
            # observe({
            #     layout <- isolate({input$layout})
            #     if (layout == "box") {
            #         res <- 120
            #     } else {
            #         res <- 200
            #     }
                output$diff_plot <- renderPlot({
                # ggfun::set_font(p, size = 3)
                req(p_diffclade())
                    p_diffclade()
                #layout <- isolate({input$layout})
                # if (layout == "box") {
                #     p <- mp_lefse() %>% ggdiffbox()
                # } else {
                    #p <- mp_lefse() %>% ggdiffclade(linewd = 0.1)
                #}
                #return(p)
            })
                
                output$downloadPlot <- downloadHandler(
                    filename = function(){
                        paste("diff_taxa_plot", input$extPlot, sep='')},
                    content = function(file){
                        req(p_diffclade())
                        ggsave(file, 
                               plot = p_diffclade(), 
                               width = input$width_slider, 
                               height = input$height_slider,
                               dpi = input$dpi)
                    })
                
                output$downloadTable <- downloadHandler(
                    filename = function(){ "diff_taxa_Data.csv" },
                    content = function(file){
                        req(p_diffclade())
                        table <- p_diffclade()$data
                        n <- names(table)[sapply(table, class) == "list"] 
                        write.csv(table %>% select(-c(n)), 
                                  file,
                                  row.names = FALSE)
                    })  
                
                
            #})
        }
    )
}


