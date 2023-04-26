rare_curve_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Rarefaction Curve",
                       status = "warning",
                       collapsible = TRUE,
                       fluidRow(
                           column(6,
                                  pickerInput(ns("index"), "Alpha-diversity Index:",
                                              options = list(`actions-box` = TRUE),
                                              choices = alpha_index, multiple = T,
                                              selected = c("Observe", "ACE", "Chao1"))
                           ),
                           column(6,
                                  pickerInput(ns("group"), "Group:", NULL)
                           )
                       ),
                       actionButton(ns("btn"), "Submit")
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Graphics Options"),
                                   tags$b("Whether to show:"),
                                   prettyCheckbox(
                                       inputId = ns("plot_group"),
                                       label = "Combine the samples",
                                       value = FALSE,
                                       status = "danger",
                                       shape = "curve"
                                   ),
                          ),
                          tabPanel(
                              h5("Color"),
                              fluidRow(
                                  column(6,
                                         uiOutput(ns("color")))
                              )
                          ),
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
                       img(src="rare_curve_plot.png",
                           align = "center",
                           width = "100%")
                   )
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("rare_curve_plot"), width = '900px', height = '600px'),
                       operation = c("enable", "disable", "destroy", "save", "load"),
                       options = list(
                           minHeight = 100, maxHeight = 900,
                           minWidth = 300, maxWidth = 1200
                       )
                   )
            )
        )

        # shinydashboardPlus::box(
        #     width = 12, 
        #     title = "Rarefaction Curve",
        #     status = "warning",
        #     collapsible = TRUE,
        #     pickerInput(ns("index"), "Alpha-diversity Index:",
        #                     options = list(`actions-box` = TRUE),
        #                     choices = alpha_index, multiple = T,
        #                     selected = c("Observe", "ACE", "Chao1")),
        #     pickerInput(ns("group"), "Group:", NULL),
        #     actionButton(ns("btn"), "Submit")
        # ),
        # shinydashboardPlus::box(
        #     width = 12,
        #     title = "Plot Download",
        #     status = "success",
        #     solidHeader = FALSE,
        #     collapsible = TRUE,
        #     plotOutput(ns("rare_curve_plot")),
        #     numericInput(ns("width_slider"), "width:", 10,1, 20),
        #     numericInput(ns("height_slider"), "height:", 8, 1, 20),
        #     radioButtons(inputId = ns('extPlot'),
        #                  label = 'Output format',
        #                  choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
        #                  inline = TRUE),
        #     downloadButton(ns("downloadPlot"), "Download Plot")
        #     #downloadButton(ns("downloadTable"), "Download Table")
        # )
        # fluidRow(),
        # jqui_resizable(
        #     plotOutput(ns("plot"), width = "600px"),
        #     operation = c("enable", "disable", "destroy", "save", "load"),
        #     options = list(
        #         minHeight = 100, maxHeight = 900,
        #         minWidth = 300, maxWidth = 1200
        #     )
        # )
    )
    return(res)
}





rare_curve_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            observe({
                req(inherits(mpse, "MPSE"))
                group <- names(mp_extract_sample(mpse))
                updatePickerInput(session, "group", choices = group)
            })
            
            mp_rare <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                # res <- tryCatch(
                #     {
                #         rare <- readRDS("data/mp_rare.rda")
                #         samples <- intersect(
                #             mp_extract_sample(mpse)[[1]],
                #             mp_extract_sample(rare)[[1]]
                #         )
                #         rare %>% filter(Sample %in% samples)
                #     },
                #     error = function(e) e
                # )
                # if (! inherits(res, "MPSE")) {
                    res <- mpse %>%
                        mp_cal_rarecurve(
                            .abundance = Abundance,
                            chunks = 50,
                            action = "add",
                            force = T
                        )
                # }
                return(res)
            })
            p_rare_curve <- reactive({
                req(inherits(mp_rare(), "MPSE"))
                input$btn
                group <- isolate({input$group})
                index <- isolate({input$index})
                p <- mp_rare() %>%
                    mp_plot_rarecurve(.rare=AbundanceRarecurve,
                                      .alpha=!!index,
                                      .group=!!group,
                                      plot.group = input$plot_group) +
                    cmap_theme
                if (group=="Sample") {
                    p <- p + theme(legend.position = "none")
                }
                
                color_content <- mpse %>% mp_extract_sample %>%
                    select(!!sym(group)) %>% unique #It is a tibble
                
                if(color_content[[1]] %>% is.numeric) {
                    return(p)
                }
                
                ncolors <- color_content[[1]] %>% length #length of group 
                color_input <- lapply(seq(ncolors), function (i){
                    input[[paste0("colors",i)]]
                }) %>% unlist #calling input color by length of group 
                
                if(length(color_input) != ncolors) {
                    p <- p + 
                        scale_color_manual(values = cc(ncolors)) +
                        scale_fill_manual(values=cc(ncolors),guide="none")  
                }else{
                    p <- p + 
                        scale_color_manual(values = color_input) +
                        scale_fill_manual(values=color_input, guide="none") 
                }
                
                
                return(p)
                
            })
            
            #Modify color
            color_list <- reactive({
                req(mp_rare())
                input$btn
                group <- isolate({
                    input$group
                })
                ns <- NS(id)
                if(!is.numeric(mp_extract_sample(mpse)[[group]])){
                color_content <- mpse %>% mp_extract_sample %>% 
                    select(!!sym(group)) %>% unique #It is a tibble
                name_colors <- color_content[[1]] %>% sort #getting chr.
                pal <- cc(length(name_colors)) #calling color palette
                names(pal) <- name_colors #mapping names to colors 
                
                picks <- lapply(seq(pal), function(i) {#building multiple color pickers
                    colorPickr(
                        inputId = ns(paste0("colors",i)),
                        label = names(pal[i]),
                        selected = pal[[i]],
                        swatches = cols,
                        theme = "monolith",
                        useAsButton = TRUE
                    )
                })
                return(picks)
                }
            })
            
            output$rare_curve_plot <- renderPlot({
                req(p_rare_curve())
                p_rare_curve()
            })
            
            output$color <- renderUI(
                #req(color_list)
                color_list()
            )
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("rare_curve_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_rare_curve())
                    ggsave(file, 
                           plot = p_rare_curve(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
        }
    )
}

