beta_nmds_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "NMDS Analysis",
                       status = "warning",
                       collapsible = TRUE,
                       fluidRow(
                           column(6,
                                  pickerInput(ns("std_method"),
                                              "Standardization method:",
                                              choices = std_method,
                                              selected = "total"
                                  )
                           ),
                           column(6,
                                  pickerInput(ns("dist_method"),
                                              "Adonis distance method:",
                                              choices = dist_method,
                                              selected = "bray"
                                  )
                           )
                       ),
                       pickerInput(ns("group"), "Group:", NULL),
                       actionButton(ns("btn"), "Submit")
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Graphics Options"),
                                   tags$b("Whether to show:"),
                                   fluidRow(
                                       column(4,
                                              prettyCheckbox(
                                                  inputId = ns("btn_ellipse"),
                                                  label = "Ellipse",
                                                  value = TRUE,
                                                  status = "danger",
                                                  shape = "curve"
                                              )
                                       ),
                                       column(4,
                                              prettyCheckbox(
                                                  inputId = ns("ellipse_fill"),
                                                  label = "Fill ellipse",
                                                  value = FALSE,
                                                  status = "danger",
                                                  shape = "curve"
                                              )
                                       ),
                                       column(4,
                                              prettyCheckbox(
                                                  inputId = ns("sample_label"),
                                                  label = "Sample label",
                                                  value = FALSE,
                                                  status = "danger",
                                                  shape = "curve"
                                              )
                                       ),
                                   ),
                                   fluidRow(
                                       column(4,
                                              prettyCheckbox(
                                                  inputId = ns("btn_side"),
                                                  label = "Side box",
                                                  value = FALSE,
                                                  status = "danger",
                                                  shape = "curve"
                                              )
                                       ),
                                       column(4,
                                              prettyCheckbox(
                                                  inputId = ns("btn_adonis"),
                                                  label = "Adonis results",
                                                  value = FALSE,
                                                  status = "danger",
                                                  shape = "curve"
                                              )
                                       )
                                       
                                   ),
                                   fluidRow(
                                       column(6,
                                              numericInput(ns("lwd"), "Line width:", 0.5, 0, 2, 0.1)
                                       ),
                                       column(6,
                                              selectInput(inputId = ns('line_type'),
                                                          label = 'Line types',
                                                          choices = c('Solid line' = "1",
                                                                      "Dotted line" = "2")
                                              )
                                       )
                                   ),
                                   # selectInput(inputId = ns('dim'),
                                   #             label = 'Dimension',
                                   #             choices = c('NMDS1 and NMDS2' = "2",
                                   #                         "NMDS1 and NMDS3" = "3")
                                   # ),
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
                       img(src="nmds_plot.png",
                           align = "center",
                           width = "100%")
                   )
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("nmds_plot"), width = '900px', height = '600px'),
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

beta_nmds_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            treeda <- reactiveVal({
                readRDS("data/treeda.rds")
            })
            #update input
            observe({
                req(inherits(mpse, "MPSE"))
                lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                updatePickerInput(session, "group",
                                  choices = group,
                                  selected = tail(names(lev[lev == 2]), 1)
                )
                if (!is.null(treeda())) {
                    updatePickerInput(session, "dist_method",
                                      choices = c(dist_method, c("unweighted uniFrac" = "unifrac", "weighted uniFrac" = "wunifrac"))
                    )
                }
            })
            
            mp_nmds <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                std <- isolate({
                    input$std_method
                })
                dist <- isolate({
                    input$dist_method
                })
                group <- isolate({input$group})
                if (dist %in% c("unifrac", "wunifrac")) {
                    otutree(mpse) <- treeda()
                }
                nmds_log <- capture.output(
                    res <- mpse %>%
                        mp_decostand(
                            .abundance = Abundance,
                            method = std
                        ) %>%
                        mp_cal_nmds(
                            .abundance = !!std,
                            distmethod = dist,
                            action = "add"
                        )%>%
                        mp_adonis(.abundance = !!std,
                                  .formula = as.formula(paste0("~", group)),
                                  distmethod = dist,
                                  permutations = 999,
                                  action = "add"
                        )
                )
                stress <- tail(nmds_log[grepl("^Run", nmds_log)], 1)
                stress <- gsub("^.+(stress.+$)", "\\1", stress)
                return(list(stress = stress, res = res))
            })
            
            p_nmds <- reactive({
                req(inherits(mp_nmds()$res, "MPSE"))
                input$btn
                group <- isolate({
                    input$group
                })
                
                
                color_content <- mpse %>% 
                    mp_extract_sample %>%
                    select(!!sym(group)) %>% 
                    unique #An single col tibble
                
                if(color_content[[1]] %>% is.numeric) { # continuity 
                    data <- mp_nmds()$res %>% mp_extract_sample
                    
                    x_name <- data[, ncol(data)] %>% names 
                    y_name <- data[, ncol(data) - 1] %>% names 
                    
                    p <- ggplot(data = data, aes(x = !!sym(x_name), 
                                                 y = !!sym(y_name), 
                                                 color = !!sym(group))) +
                        geom_point(size = 3, alpha = 0.7) +
                        labs(x = x_name, y = y_name) +
                        theme_bw() +
                        cmap_theme
                }else {
                    if(input$ellipse_fill){
                        p <- mp_nmds()$res %>%
                            mp_plot_ord(
                                .ord = nmds,
                                #.dim = dim_PC,
                                .group = !!sym(group),
                                .color = !!sym(group),
                                geom = "polygon", 
                                alpha = 0.25,
                                ellipse = input$btn_ellipse,
                                show.side = input$btn_side, 
                                show.sample = input$sample_label,
                                show.legend = FALSE,
                                linetype = (input$line_type %>% as.numeric),
                                lwd = input$lwd
                            ) + 
                            cmap_theme
                    }else{
                        p <- mp_nmds()$res %>%
                            mp_plot_ord(
                                .ord = nmds,
                                #.dim = dim_PC,
                                .group = !!sym(group),
                                .color = !!sym(group),
                                ellipse = input$btn_ellipse,
                                show.side = input$btn_side, 
                                show.sample = input$sample_label,
                                show.legend = FALSE,
                                linetype = (input$line_type %>% as.numeric),
                                lwd = input$lwd
                            ) + 
                            cmap_theme
                    }
                }
                
                #Partition type 1
                if(is.character(mp_extract_sample(mpse)[[group]])){
                    p$data[[group]] %<>% factor(level = input$items1)
                }
                
                if(input$btn_adonis) {
                    adonis_value <- mp_nmds()$res %>% mp_extract_internal_attr(name='adonis')
                    #NEW VERSION OF MP mp_extract_internal_attr()
                    eq <- substitute(expr = italic(R)^2~"="~r2~","~italic(p)~"="~pvalue,
                                     env = list(r2 = adonis_value$R2[1] %>% round(5),
                                                pvalue = adonis_value$`Pr(>F)`[1])
                    ) %>% as.expression
                    
                    #older versionmp_pcoa()$aov.tab
                    # eq <- substitute(expr = italic(R)^2~"="~r2~","~italic(p)~"="~pvalue,
                    #                  env = list(r2 = adonis_value$aov.tab$R2[1] %>% round(5),
                    #                             pvalue = adonis_value$aov.tab$`Pr(>F)`[1])
                    # ) %>% as.expression
                    
                    p <- p + geom_text(aes(x = Inf, y = Inf),
                                       label = eq,
                                       hjust = 1.1,
                                       vjust = 1.1,
                                       check_overlap = TRUE,
                                       inherit.aes = FALSE)
                }
                
                p <- p +
                    ggtitle(mp_nmds()$stress) +
                    theme(plot.title = element_text(hjust = 0.5))
                
                ######################################################
                
                if(color_content[[1]] %>% is.numeric) {
                    if(is.null(input$h_color)) {
                        p <- p + 
                            scale_colour_gradient(low = "#eaf4f4",
                                                  high = "#278173",
                                                  limits = c(min(color_content[[1]]), 
                                                             max(color_content[[1]])),
                                                  breaks = pretty(color_content[[1]], 
                                                                  n = 4, 
                                                                  min.n = 3)
                            )
                        
                    }else{
                        p <- p + 
                            scale_color_gradient(low = input$l_color, 
                                                 high = input$h_color,
                                                 limits = c(min(color_content[[1]]), 
                                                            max(color_content[[1]])),
                                                 breaks = pretty(color_content[[1]], 
                                                                 n = 4, 
                                                                 min.n = 3))
                    }
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
                req(mp_nmds())
                input$btn
                box_leves <- mp_extract_sample(mp_nmds()$res)[[input$group]] %>% unique
                return(box_leves)
            })
            
            #Modify color
            color_list <- reactive({
                req(mp_nmds())
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
                    #return(picks)
                }else {
                    
                    high <- colorPickr(
                        inputId = ns("h_color"),
                        label = "high",
                        selected = "#278173",
                        swatches = cols,
                        theme = "monolith",
                        useAsButton = TRUE
                    )
                    
                    low <- colorPickr(
                        inputId = ns("l_color"),
                        label = "low",
                        selected = "#eaf4f4",
                        swatches = cols,
                        theme = "monolith",
                        useAsButton = TRUE
                    )
                    picks <- list(high, low)
                    #return(picks)
                }

            })
            
            output$nmds_plot <- renderPlot({
                req(p_nmds())
                p_nmds()
            })
            
            output$color <- renderUI(
                #req(color_list)
                color_list()
            )
            
            output$box_order <- renderUI({
                req(mp_nmds())
                input$btn
                group <- isolate({
                    input$group
                })
                if(is.character(mp_extract_sample(mpse)[[group]])){
                    orderInput(ns('items1'), 
                               'Order (Drag items below)', 
                               items = box_leves())
                }
                
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("nmds_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_nmds())
                    ggsave(file, 
                           plot = p_nmds(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "nmda_Data.csv" },
                content = function(file){
                    req(p_nmds())
                    table <- p_nmds()$data 
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
        }
    )
}