buglist_alpha_mul_index_ui <- function(id) {
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
                       pickerInput(ns("index"), "Alpha-diversity Index:",
                                   choices = alpha_index, multiple = FALSE,
                                   selected = "Shannon"
                       ),
                       fluidRow(
                           column(6,
                                  pickerInput(ns("group"), "Group:", NULL)
                           ),
                           column(6,
                                  pickerInput(ns("x_group"), "X Group:", NULL)
                           )
                       ),
                       actionButton(ns("btn"), "Submit")
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Graphics Options"),
                                   tags$b("Whether to show:"),
                                   prettyCheckbox(
                                       inputId = ns("point"),
                                       label = "Data Points",
                                       value = FALSE,
                                       status = "danger",
                                       shape = "curve"
                                   ),
                                   numericInput(ns("tip_length"), "Tip length:", 0, 0, 1, 0.01),
                                   fluidRow(
                                       column(6,
                                              numericInput(ns("box_width"), "Box Width:", 0.8, 0, 1, 0.01)
                                       ),
                                       column(6,
                                              numericInput(ns("dodge"), "Dodge:", 0.8, 0, 1, 0.01)
                                       )
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
                   ),
                   shinydashboard::box(
                       title = "Example Plot",
                       width = NULL,
                       img(src="alpha_mul_index_plot.png",
                           align = "center",
                           width = "100%")
                   )
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("p_mul_alpha_index"), width = '400px', height = '600px'),
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


buglist_alpha_mul_index_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            observe({
                req(inherits(mpse, "MPSE")) 
                sample_names <- mp_extract_sample(mpse) %>% names
                division_name <- sample_names[unlist(lapply(mpse %>% mp_extract_sample,class) %in% "character")]
                updatePickerInput(session, "group",
                                  choices = division_name,
                                  selected = tail(division_name, 1)
                )
                updatePickerInput(session, "x_group",
                                  choices = division_name,
                                  selected = tail(division_name, 2)
                )
                
            })
            
            
            mp_mul_alpha <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                #input$submit
                mp_mul_alpha <- mp_cal_alpha(mpse, .abundance = Abundance, force = TRUE)
                return(mp_mul_alpha)
            })
            
            p_mul_alpha_index <- reactive({
                req(inherits(mp_mul_alpha(), "MPSE"))
                input$btn
                group <- isolate({input$group})
                x_group <- isolate({input$x_group})
                index <- isolate({input$index})
                data <- mp_mul_alpha() %>% mp_extract_sample
                
                if (input$point){
                    p <- ggplot(data, aes(x = !!sym(x_group), y = !!sym(index), color = !!sym(group))) + 
                        geom_point(position = position_jitterdodge(jitter.width = 0.2), size = 3) + 
                        geom_boxplot(outlier.shape = NA, size = 1, alpha = 0.7, 
                                     width = input$box_width, 
                                     position = position_dodge(width = input$dodge)) +
                        theme_prism() + 
                        labs(title = paste0(index,"-",group),
                             x = x_group, 
                             y = "Alpha index") + 
                        scale_color_brewer(palette = "Dark2") +
                        theme(plot.title = element_text(hjust = 0))
                }else {
                    p <- ggplot(data, aes(x = !!sym(x_group), y = !!sym(index))) + 
                        geom_boxplot(aes(fill = !!sym(group)),
                                     width = input$box_width,
                                     position = position_dodge(width = input$dodge)
                        ) + 
                        theme_prism() + 
                        labs(title = paste0(index,"-",group),
                             x = x_group, 
                             y = "Alpha index") + 
                        scale_fill_brewer(palette = "Dark2") +
                        theme(plot.title = element_text(hjust = 0))
                }
                
                f <- paste(index, "~", group)
                
                tryCatch(
                    {
                        ano <- data %>% rstatix::group_by(!!sym(x_group)) %>%
                            rstatix::wilcox_test(as.formula(f)) %>%
                            rstatix::add_xy_position(x = x_group, group = group)
                    },
                    error = function(e) {
                        warning("An error occurred while calculating the P value:")
                        message(e)
                    }
                )
                
                #,
                p <- p + geom_signif(annotation = formatC(ano$p, digits = 1),
                                     xmin = ano$xmin, xmax = ano$xmax, y_position = ano$y.position,
                                     tip_length = input$tip_length, color = "black")
                
                
                ########################################################
                ##color model
                color_content <- mpse %>% mp_extract_sample %>%
                    select(!!sym(group)) %>% unique #It is a tibble
                
                ncolors <- color_content[[1]] %>% length #length of group
                color_input <- lapply(seq(ncolors), function (i){
                    input[[paste0("colors",i)]]
                }) %>% unlist #calling input color by length of group
                
                if(length(color_input) != ncolors) {
                    p 
                }else{
                    if(input$point) {
                        p <- p + scale_color_manual(values = color_input) 
                    }else{
                        p <- p +
                            scale_fill_manual(values = color_input)
                    }
                }
                
                
                # }
                
                return(p)
            })
            
            
            
            #Modify color
            color_list <- reactive({
                req(mp_mul_alpha())
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
            
            output$p_mul_alpha_index <- renderPlot({
                req(p_mul_alpha_index())
                p_mul_alpha_index()
            })
            
            output$color <- renderUI(
                #req(color_list)
                color_list()
            )
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("alpha_mul_index_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_mul_alpha_index())
                    ggsave(file, 
                           plot = p_mul_alpha_index(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "Alpha_Data.csv" },
                content = function(file){
                    req(p_mul_alpha_index())
                    table <- mp_mul_alpha() %>% mp_extract_sample 
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
        }
    )
}