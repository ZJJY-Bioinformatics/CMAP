rda_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "RDA Analysis",
                       status = "warning",
                       collapsible = TRUE,
                       pickerInput(ns("envf"),
                                   "Environmental Factor:",
                                   NULL,
                                   multiple = TRUE,
                                   options = list(`multiple-separator` = " | "),
                       ),
                       pickerInput(ns("group"), "Group:", NULL),
                       actionButton(ns("btn"), "Submit")
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Graphics Options"),
                                   tags$b("Whether to show:"),
                                   fluidRow(
                                       column(6,
                                              prettyCheckbox(
                                                  inputId = ns("sample_label"),
                                                  label = "Sample label",
                                                  value = FALSE,
                                                  status = "danger",
                                                  shape = "curve"
                                              )
                                       ),
                                       column(6,
                                              prettyCheckbox(
                                                  inputId = ns("btn_side"),
                                                  label = "Side box",
                                                  value = FALSE,
                                                  status = "danger",
                                                  shape = "curve"
                                              )
                                       )
                                       
                                   ),
                                   uiOutput(ns("box_order"))
                                   
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
                       img(src="rda_plot.png",
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

rda_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            treeda <- reactiveVal({
                readRDS("data/treeda.rds")
            })
            observe({
                req(inherits(mpse, "MPSE"))
                lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                sample_na <- mpse %>% mp_extract_sample %>% names
                envf_name <- sample_na[unlist(!lapply(mpse %>% mp_extract_sample,class) %in% "character")]
                
                updatePickerInput(session, "group",
                                  choices = group,
                                  selected = tail(names(lev[lev == 2]), 1)
                )
                updatePickerInput(session, "envf",
                                  choices = envf_name,
                                  selected = tail(envf_name, 1)
                )
                
            })
            mp_rda <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                
                group <- isolate({input$group})
                envf <- isolate({input$envf})
                envformula <- paste("~", paste(envf, collapse="+")) %>% as.formula
                mp_rda <- mpse %>%
                    mp_cal_rda(.abundance=Abundance, .formula = envformula, action = "add")
                return(mp_rda)
            })
            
            p_rda <- reactive({
                req(mp_rda())
                input$btn
                group <- isolate({
                    input$group
                })
                
                p <- mp_rda() %>%
                    mp_plot_ord(
                        .ord = rda,
                        .group = !!sym(group),
                        .color = !!sym(group),
                        show.side = input$btn_side,
                        show.sample = input$sample_label,
                        
                    ) + 
                    cmap_theme
                
                if(is.character(mp_extract_sample(mpse)[[group]])){
                    p$data[[group]] %<>% factor(level = input$items1)
                }
                
                ##color model
                color_content <- mpse %>% mp_extract_sample %>%
                    select(!!sym(group)) %>% unique #It is a tibble
                
                if(color_content[[1]] %>% is.numeric) {
                    return(p)
                }else{
                    ncolors <- color_content[[1]] %>% length #length of group 
                    color_input <- lapply(seq(ncolors), function (i){
                        input[[paste0("colors",i)]]
                    }) %>% unlist #calling input color by length of group 
                    
                    if(length(color_input) != ncolors) {
                        p <- p + 
                            scale_color_manual(values = cc(ncolors)) + 
                            scale_fill_manual(values = cc(ncolors)) 
                    }else{
                        p <- p + 
                            scale_color_manual(values = color_input) + 
                            scale_fill_manual(values = color_input)
                    }
                    return(p)
                }
            })
            
            box_leves <- reactive({
                req(mp_rda())
                input$btn
                box_leves <- mp_extract_sample(mp_rda())[[input$group]] %>% unique
                return(box_leves)
            })
            
            #Modify color
            color_list <- reactive({
                req(mp_rda())
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
            
            output$box_order <- renderUI({
                req(mp_rda())
                input$btn
                group <- isolate({
                    input$group
                })
                #Partition type 3
                if(is.character(mp_extract_sample(mpse)[[group]])){
                    orderInput(ns('items1'), 
                               'Order (Drag items below)', 
                               items = box_leves())
                }
                
            })
            
            output$plot <- renderPlot({
                req(p_rda())
                p_rda()
            })
            
            output$color <- renderUI({
                req(mp_rda())
                input$btn
                color_list()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("rda_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_rda())
                    ggsave(file, 
                           plot = p_rda(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "rda_Data.csv" },
                content = function(file){
                    req(mp_rda())
                    table <- mp_rda() %>% mp_extract_sample 
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
           
        }
    )
}