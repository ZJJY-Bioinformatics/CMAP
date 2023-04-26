buglist_beta_pca_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        # class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "PCA Analysis",
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
                                                  value = FALSE,
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
                                   selectInput(inputId = ns('dim'),
                                               label = 'Dimension',
                                               choices = c('PC1 and PC2' = "2",
                                                           "PC1 and PC3" = "3")
                                   ),
                                   #tags$b("Order (Drag items below)"),
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
                       img(src="PCA_plot.png",
                           align = "center",
                           width = "100%")
                   )
                   
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("PCA_plot"), width = '900px', height = '600px'),
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

buglist_beta_pca_mod <- function(id, mpse) {
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
            })
            
            mp_pca <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                std <- isolate({input$std_method})
                group <- isolate({input$group})
                dist <- isolate({input$dist_method})
                mpse %>%
                    mp_decostand(.abundance = Abundance, method = std) %>%
                    mp_cal_pca(.abundance = !!std, action = "add")%>%
                    mp_adonis(.abundance = !!std,
                              .formula = as.formula(paste0("~", group)),
                              distmethod = dist,
                              permutations = 999,
                              action = "add"
                    )
            })
            
            p_PCA <- reactive({
                req(inherits(mp_pca(), "MPSE"))
                input$btn
                group <- isolate({
                    input$group
                })
                
                
                dim_PC <- input$dim %>% as.numeric
                dim_PC <- c(1, dim_PC)
                
                color_content <- mpse %>% 
                    mp_extract_sample %>%
                    select(!!sym(group)) %>% 
                    unique #An single col tibble
                
                if(color_content[[1]] %>% is.numeric) { # continuity 
                    data <- mp_pca() %>% mp_extract_sample
                    
                    dim_PC <- input$dim %>% as.numeric #2 or 3
                    x_name <- data[, ncol(data) - 2] %>% names #PC 1
                    y_name <- data[, ncol(data) + dim_PC - 3] %>% names #PC 2/3
                    
                    p <- ggplot(data = data, aes(x = !!sym(x_name), 
                                                 y = !!sym(y_name), 
                                                 color = !!sym(group))) +
                        geom_point(size = 3, alpha = 0.7) +
                        labs(x = x_name, y = y_name) +
                        theme_bw() +
                        cmap_theme
                }else {
                    if(input$ellipse_fill){
                        p <- mp_pca() %>%
                            mp_plot_ord(
                                .ord = pca,
                                .dim = dim_PC,
                                .group = !!sym(group),
                                .color = !!sym(group),
                                geom = "polygon", 
                                alpha = 0.25,
                                #.size = !!s(size),
                                #.alpha = !!s(alpha),
                                ellipse = input$btn_ellipse,
                                #show.side = !is.null(side),
                                show.side = input$btn_side, 
                                show.sample = input$sample_label,
                                show.legend = FALSE,
                                linetype = (input$line_type %>% as.numeric),
                                lwd = input$lwd
                            ) + 
                            cmap_theme
                    }else{
                        p <- mp_pca() %>%
                            mp_plot_ord(
                                .ord = pca,
                                .dim = dim_PC,
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
                
                #Partition type 1 for order module ##change order by order box
                if(is.character(mp_extract_sample(mpse)[[group]])){
                    p$data[[group]] %<>% factor(level = input$items1)
                }
                
                #add adonis values
                if(input$btn_adonis) {
                    adonis_value <- mp_pca() %>% mp_extract_internal_attr(name='adonis')
                    #NEW VERSION OF MP mp_extract_internal_attr()
                    eq <- substitute(expr = italic(R)^2~"="~r2~","~italic(p)~"="~pvalue,
                                     env = list(r2 = adonis_value$R2[1] %>% round(5),
                                                pvalue = adonis_value$`Pr(>F)`[1])
                    ) %>% as.expression
                    
                    p <- p + geom_text(aes(x = Inf, y = Inf),
                                       label = eq,
                                       hjust = 1.1,
                                       vjust = 1.1,
                                       check_overlap = TRUE,
                                       inherit.aes = FALSE)
                }
                
                #Partition type 2
                if(color_content[[1]] %>% is.numeric){
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
                        input[[paste0("colors",i)]] #color_list()
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
            
            #Order
            box_leves <- reactive({
                req(mp_pca())
                input$btn
                box_leves <- mp_extract_sample(mp_pca())[[input$group]] %>% unique
                return(box_leves)
            })
            
            output$box_order <- renderUI({
                req(mp_pca())
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
            
            #Modify color
            color_list <- reactive({
                req(mp_pca())
                input$btn
                group <- isolate({
                    input$group
                })
                ns <- NS(id)
                
                #Partition type 4
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
            
            output$PCA_plot <- renderPlot({
                req(p_PCA())
                p_PCA()
            })
            
            output$color <- renderUI({
                req(mp_pca())
                input$btn
                color_list()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("PCA_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_PCA())
                    ggsave(file, 
                           plot = p_PCA(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "PCA_Data.csv" },
                content = function(file){
                    req(p_PCA())
                    table <- mp_pca() %>% mp_extract_sample 
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
            
            
        }
    )
}








