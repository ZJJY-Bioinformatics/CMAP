alpha_index <- c("Observe", "Chao1", "ACE", "Shannon", "Simpson", "Pielou")
# names(alpha_index) <- alpha_index

alpha_index_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Alpha-diversity Index",
                       status = "warning",
                       collapsible = TRUE,
                       fluidRow(
                           column(6,
                                  pickerInput(ns("index"), "Alpha-diversity Index:",
                                              options = list(`actions-box` = TRUE),
                                              choices = alpha_index, multiple = T,
                                              selected = c("Observe", "ACE", "Chao1")
                                  )
                           ),
                           column(6,
                                  pickerInput(
                                      ns("test"), "Statistical method:",
                                      choices = c("parametric", "nonparametric", "robust", "bayes"),
                                      selected = "robust"
                                  )
                           )
                       ),
                       fluidRow(
                           column(6,
                                  pickerInput(ns("group"), "Group:", NULL)
                           ),
                           column(6,
                                  pickerInput(ns("type"), "Type:", c("discrete"))
                           )
                       ),
                       actionButton(ns("btn"), "Submit")
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Graphics Options"),
                                   tags$b("Whether to show:"),
                                   prettyCheckbox(
                                       inputId = ns("subtitle"),
                                       label = "statistical tests results",
                                       value = FALSE,
                                       status = "danger",
                                       shape = "curve"
                                   ),
                                   radioGroupButtons(
                                       inputId = ns("box_style"),
                                       label = "Plot type",
                                       choices = c("box", 
                                                   "violin", 
                                                   "boxviolin"),
                                       checkIcon = list(
                                           yes = tags$i(class = "fa fa-check-square", 
                                                        style = "color: steelblue"),
                                           no = tags$i(class = "fa fa-square-o", 
                                                       style = "color: steelblue"))
                                   )
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
                   )
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("alpha_index_plot"), width = '900px', height = '600px'),
                       operation = c("enable", "disable", "destroy", "save", "load"),
                       options = list(
                           minHeight = 100, maxHeight = 900,
                           minWidth = 300, maxWidth = 1200
                       )
                   )
            )
        )
    )
    return(res)
}


alpha_index_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            treeda <- reactiveVal({readRDS("data/treeda.rds")})
            observe({
                req(inherits(mpse, "MPSE")) 
                lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                updatePickerInput(session, "group", choices = group,
                                  selected = tail(names(lev[lev == 2]), 1))
                if (!is.null(treeda())) {
                    updatePickerInput(session, "index",
                                      choices = c(alpha_index, c("PD Whole Tree" = "PD", "Species Richness" = "SR")),
                                      selected = c("Observe", "ACE", "Chao1")
                    )
                }
                
            })
            observe({
                req(inherits(mpse, "MPSE"), input$group)
                if(is.numeric(mp_extract_sample(mpse)[[input$group]])) {
                    updatePickerInput(session, "type", choices = c("discrete", "continuous"), selected = "continuous")
                    updatePickerInput(session, "test", selected = "nonparametric")
                } else {
                    updatePickerInput(session, "type", choices = c("discrete"))
                    updatePickerInput(session, "test", selected = "robust")
                }
            })
            mp_alpha <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                mp_alpha <- mp_cal_alpha(mpse, .abundance = Abundance, force = T)
                if (!is.null(treeda())) {
                    pd_a_index <- pd(
                        t(mp_extract_assays(mpse, Abundance)),
                        treeda(), ape::is.rooted(treeda())
                    )
                    pd_a_index <- data.frame(
                        Sample = row.names(pd_a_index),
                        pd_a_index, row.names = NULL
                    )
                    mp_alpha <- mp_alpha %>%
                        left_join(y = pd_a_index, by = "Sample")
                }
                return(mp_alpha)
            })
            
            p_alpha_index <- reactive({
                req(inherits(mp_alpha(), "MPSE"))
                input$btn
                group <- isolate({input$group})
                index <- isolate({input$index})
                # fac <- isolate({as.numeric(input$factor)})
                test <- isolate({input$test})
                type <- isolate({input$type})
                fac <- unique(mp_extract_sample(mpse)[[group]])
                # validate(need(fac >= 2,
                #          "Please select a group with 2 (or >2) different levels."))
                
                tbl <- mp_alpha() %>%
                    mp_extract_sample() %>%
                    select(c(group, index)) %>%
                    tidyr::pivot_longer(
                        cols = !!index,
                        names_to = "Alpha")
               
                if (is.numeric(tbl[[group]]) && type == "continuous") {#continuous vector don't call color pal.
                    p <- grouped_ggscatterstats(tbl, 
                                                x = !!sym(group), 
                                                y = value, 
                                                grouping.var = Alpha, 
                                                type = test, 
                                                results.subtitle =FALSE,
                                                centrality.plotting = FALSE)
                } else {
                    color_content <- mpse %>% mp_extract_sample %>%
                        select(!!sym(group)) %>% unique #It is a tibble
                    
                    ncolors <- color_content[[1]] %>% length #length of group 
                    color_input <- lapply(seq(ncolors), function (i){
                        input[[paste0("colors",i)]]
                    }) %>% unlist #calling input color by length of group 
                    
                    if(length(color_input) != ncolors) {
                        p <- grouped_ggbetweenstats(tbl, 
                                                    x = !!sym(group), 
                                                    y = value, 
                                                    plot.type = input$box_style, 
                                                    grouping.var = Alpha, 
                                                    type = test,
                                                    results.subtitle = input$subtitle,
                                                    centrality.plotting = FALSE,
                                                    pairwise.display = "s"
                        )
                    }else{
                        p <- grouped_ggbetweenstats(tbl, 
                                                    x = !!sym(group), 
                                                    y = value, 
                                                    plot.type = input$box_style,
                                                    grouping.var = Alpha, 
                                                    type = test,
                                                    results.subtitle =input$subtitle,
                                                    centrality.plotting = FALSE,
                                                    pairwise.display = "s",
                                                    ggplot.component = list(
                                                        scale_color_manual(values = color_input)
                                                        )
                                                    )
                    }
                }
                return(p)
            })
            
            #Modify color
            color_list <- reactive({
                req(mp_alpha())
                input$btn
                group <- isolate({input$group})
                ns <- NS(id)
                if(!is.numeric(mp_extract_sample(mpse)[[group]])){
                    color_content <- mpse %>% mp_extract_sample %>% 
                        select(!!sym(group)) %>% unique #It is a tibble
                    name_colors <- color_content[[1]] %>% sort #getting chr.
                    pal <- pattle_drak2(length(name_colors)) #calling color palette:"pattle_drak2"
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
            
            output$alpha_index_plot <- renderPlot({
                req(p_alpha_index())
                p_alpha_index()
            })
            
            output$color <- renderUI(
                #req(color_list)
                color_list()
            )
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("alpha_index_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_alpha_index())
                    ggsave(file, 
                           plot = p_alpha_index(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "Alpha_Data.csv" },
                content = function(file){
                    req(p_alpha_index())
                    table <- mp_alpha() %>% mp_extract_sample 
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
        }
    )
}