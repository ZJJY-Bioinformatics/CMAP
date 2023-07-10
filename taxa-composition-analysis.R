# taxa_level <- c("Kingdom", "Phylum", "Class", "Order",
#                 "Family", "Genus", "Species")
taxa_composition_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Taxonomy composition Analysis",
                       status = "warning",
                       collapsible = TRUE,
                       p("Click below to compute microbial abundance at each level. It's a  time-consuming process, a single calculation should suffice."),
                       #uiOutput(ns("sampele_order")),
                       div(
                           style = "text-align:center; padding-top:10px;",
                           actionBttn(
                               ns("btn"), 
                               "Calculating Abundance", 
                               style = "jelly", 
                               color = "warning"
                           )
                       )
                       # 
                       # div(
                       #     style = "text-align:center; padding-top:10px;", 
                       #     actionBttn(ns("btn"), "Calculating Abundance", 
                       #                  style = "stretch")
                       #                  #style = "font-size:15px; color: white; background-color: #007BFF; height: 50px; width: 200px;")
                       # )
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Graphics Options"),
                                  # tags$b("Whether to show:"),
                                   # prettyCheckbox(
                                   #     inputId = ns("btn_relative"),
                                   #     label = "Relative Abundance",
                                   #     value = TRUE,
                                   #     status = "danger",
                                   #     shape = "curve"
                                   # ),
                                   fluidRow(
                                       column(6,
                                              pickerInput(ns("level"),
                                                          "Taxonomic level:",
                                                          choices = NULL)
                                       ),
                                       column(6,
                                              pickerInput(ns("group"), "Group:", NULL)
                                       )
                                   ),
                                   
                                   fluidRow(
                                       column(6,
                                              numericInput(ns("topn"), "Top most abundant:", value = 10)
                                       ),
                                       column(6,
                                              selectInput(inputId = ns('geom'),
                                                          label = 'Plot style',
                                                          choices = c("flowbar" = "flowbar",
                                                                      'bar' = "bar")
                                              )
                                       )
                                   ),
                          ),#close tabPanel
                          # tabPanel(
                          #     h5("Color"),
                          #     selectInput(inputId = ns('color_pals'),
                          #                 label = 'Color scheme',
                          #                 choices = c('Color scheme 1' = "col2",
                          #                             "Color scheme 2" = "col3",
                          #                             "Color scheme 3" = "col")
                          #     ),
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
                       img(src="taxa_composition_plot.png",
                           align = "center",
                           width = "100%")
                   )
            ),
            column(9,
                   jqui_resizable(
                       plotOutput(ns("taxa_composition_plot"), width = '900px', height = '600px'),
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


taxa_composition_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            observe({
                req(inherits(mpse, "MPSE")) 
                lev <- sapply(mp_extract_sample(mpse), function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                taxa <- names(mp_extract_taxonomy(mpse))[-1]
                updatePickerInput(session, "group", choices = group,
                                  selected = tail(names(lev[lev == 2]), 1))
                updatePickerInput(session, "level", choices = taxa,
                                  selected = taxa[3])
            })
        
            mp_comp <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                mp_composition <- mpse %>%
                    mp_cal_abundance(.abundance = RareAbundance, action = "add")
                
                return(mp_composition)
            })
            
            
            
            
           # mp_comp <- eventReactive(input$btn, {
           #      req(inherits(mpse, "MPSE"))
           #      input$btn
           #      group <- isolate({input$group})
           #      mp_comp <- mpse
           # 
           #      #covert "group" into factor
           #      if(mp_comp@colData[,group] %>% is.character()){
           #          mp_comp@colData[,group] %<>% factor()
           #      }
           #      
           #      order_lev_raw <- mp_comp@colData[,group] %>% levels()
           #      if(is.factor(mp_extract_sample(mp_comp)[[group]])){
           #          if(!identical(order_lev_raw, input$items_group)){
           #              mp_comp@colData[,group] %<>% factor(level = input$items_group)
           #          }#close factor if
           #      }#close identical if
           # 
           #      return(mp_comp)
           #  })
           
########################get taxa names for calling color pickers################
           # taxa_name_factor <- eventReactive(input$btn, {
           #     req(inherits(mpse, "MPSE"))
           #     #call input
           #     #level <- isolate({input$level})
           #     #ytype <- isolate({input$ytype})
           #     #group <- isolate({input$group})
           #     #topn <- isolate({input$topn})
           #     # is.group <- isolate({
           #     #     ifelse(input$xtype == "group", TRUE, FALSE)
           #     # })
           #     plot <- mpse %>%
           #         mp_plot_abundance(.abundance = RareAbundance,
           #                           .group = input$group,
           #                           taxa.class = input$level,
           #                           topn = input$topn,
           #                           relative = TRUE,
           #                           plot.group = TRUE,
           #                           force = TRUE,
           #                           geom = input$geom)
           # 
           #     taxa_name_factor <- plot$data[[1]]
           # 
           #     return(taxa_name_factor)
           # })
           
           
            p_taxa_composition <- reactive({
                req(inherits(mp_comp(), "MPSE"))

                #call input
                # level <- isolate({input$level})
                # #ytype <- isolate({input$ytype})
                # group <- isolate({input$group})
                # topn <- isolate({input$topn})
                # is.group <- isolate({
                #     ifelse(input$xtype == "group", TRUE, FALSE)
                # })
                mp <- mp_comp()
                
                #plot
                p <- mp %>%
                    mp_plot_abundance(.abundance = RareAbundance,
                                  .group = !!sym(input$group),
                                      taxa.class = !!sym(input$level),
                                      topn = input$topn,
                                      relative = TRUE,
                                      plot.group = TRUE,
                                      force = TRUE,
                                      geom = input$geom) +
                    theme(
                        text = element_text(size = 18),
                        axis.text.x = element_text(size = 11),
                        axis.text.y = element_text(size = 16),
                        legend.text = element_text(size = 16)
                    ) #+ scale_fill_manual(values = color_input)

                #########################colors###############################
                # #color model
                # color_content <- taxa_name_factor()%>% levels
                # 
                # #add color for group
                # ncolors <- color_content %>% length #length of group
                # color_input <- lapply(seq(ncolors), function (i){
                #     input[[paste0("colors",i)]]
                # }) %>% unlist #calling input color
                # 
                # #When the Panel is not in color, the value of 'color_input' is 0, 
                # #because colorpickr has not started running
                # if(length(color_input) != ncolors) {
                #     p
                # }else{
                #     p <- p +
                #         scale_fill_manual(values = color_input)
                # }#close else 

                return(p)
            })

            #Modify color
            # color_list <- reactive({
            #     req(taxa_name_factor())
            #     #group <- isolate({input$group})
            #     ns <- NS(id)
            #     #if(!is.numeric(mp_extract_sample(mpse)[[group]])){
            #         name_colors <- taxa_name_factor() %>% levels  #getting chr. of group
            #         pal <- get_cols(length(name_colors), input$color_pals) %>% rev #calling color palette:"get_cols"
            #         names(pal) <- name_colors #mapping names to colors 
            #         #print()
            #         picks <- lapply(seq(pal), function(i) {#building multiple color pickers
            #             colorPickr(
            #                 inputId = ns(paste0("colors",i)),
            #                 label = names(pal[i]),
            #                 selected = pal[[i]],
            #                 swatches = cols, #Optional color 
            #                 theme = "monolith",
            #                 useAsButton = TRUE
            #             )#close colorPickr
            #         })#close lapply
            #         
            #         return(picks)
            #     #}#close if
            # })
            
            # output$color <- renderUI(
            #     #req(color_list)
            #     color_list()
            # )
            
            
            output$taxa_composition_plot <- renderPlot({
                req(p_taxa_composition())
                p_taxa_composition()
            })
         
            
            # output$sampele_order <-  renderUI({ #order UI
            #     req(!is.null(input$group))
            #     group <- isolate({input$group})
            #     group_contents <- mp_extract_sample(mpse)[[input$group]] %>% unique
            #     # if(input$xtype == "sample") {
            #     #     NULL
            #     #     }else{
            #         orderInput(ns('items_group'), 
            #                    'Boxes order (Drag items below)', 
            #                    items = group_contents
            #                     )
            #     #}
            # })
         
           output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("taxa_composition_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_taxa_composition())
                    ggsave(file, 
                           plot = p_taxa_composition(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "taxa_composition_Data.csv" },
                content = function(file){
                    req(p_taxa_composition())
                    table <- p_taxa_composition()$data 
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
        }
    )
}

###################The abundance of single bacteria was compared################

# mpse %>%
#     mp_extract_tree() %>%
#     tidyr::unnest(RareAbundanceBySample) %>%
#     dplyr::filter(nodeClass == "Phylum") %>%
#     ggplot(mapping = aes(x = strain, y = RelRareAbundanceBySample, fill = strain)) +
#     geom_boxplot() +
#     facet_wrap(facets = vars(label), scales = "free_y") +
#     geom_signif(comparisons = list(c("mutant", "wildtype")))


feature_composition_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Taxonomy composition Analysis",
                       status = "warning",
                       collapsible = TRUE,
                       fluidRow(
                           column(6,
                                  pickerInput(ns("level"),
                                              "Taxonomic level:",
                                              choices = NULL),
                           ),
                           column(6,
                                  pickerInput(ns("feature"), 
                                              "Details:", 
                                              NULL,
                                              options = pickerOptions(liveSearch=T), 
                                              multiple = TRUE),
                           )
                       ),
                       pickerInput(ns("group"), "Group:", NULL),
                       actionButton(ns("btn"), "Submit")
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Graphics Options"),
                                   prettyCheckbox(
                                       inputId = ns("results.subtitle"),
                                       label = "results subtitle",
                                       value = FALSE,
                                       status = "danger",
                                       shape = "curve"
                                   ),
                                   selectInput(inputId = ns('plot.type'),
                                               label = 'Plot type',
                                               choices = c('only boxplots' = "box",
                                                           "only violin" = "violin",
                                                           "boxviolin" = "boxviolin")
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
                       img(src="./feature_composition.png",
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
                   )#close jqui
                   )#close column
        )
    )
    return(res)
}


feature_composition_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            observe({
                req(inherits(mpse, "MPSE"))
                lev <- sapply(mp_extract_sample(mpse), function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                taxa <- names(mp_extract_taxonomy(mpse))[-1]
                updatePickerInput(session, "group",
                    choices = group,
                    selected = tail(names(lev[lev == 2]), 1)
                )
                updatePickerInput(session, "level",
                    choices = taxa,
                    selected = taxa[3]
                )
                
            })
            observe({
                req(inherits(mpse, "MPSE"), input$level)
                updatePickerInput(session, "feature",
                    choices = unique(mp_extract_taxonomy(mpse)[[input$level]])
                )
            })
            
            mp_feature <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                level <- isolate({input$level})
                group <- isolate({input$group})
                feature <- isolate({input$feature})
                mpse %>%
                    mp_cal_abundance(.abundance=RareAbundance, action="add", force = FALSE) %>%
                    mp_extract_tree() %>%
                    tidyr::unnest(RareAbundanceBySample) %>%
                    dplyr::filter(nodeClass == level) %>%
                    dplyr::filter(label %in% feature) #%>%
                    #grouped_ggbetweenstats(x = !!sym(group), y = RelAbundanceBySample, grouping.var = label, type = "robust")
            })

            p_single_bacteria <- reactive({
                req(mp_feature())
                input$btn
                group <- isolate({input$group})
                 print(mp_feature())
                if (is.numeric(mp_feature()[[group]])) {
                    #stop("Does not apply to continuous variables")
                    if(is.null(input$line_color)) {#For continuous variables, there is no input$line_color when plotting for the first time
                        p <- ggplot(mp_feature(), aes(x = !!sym(group), y = RareAbundance)) +
                            geom_point(color = "#2c3e50", size = 3, alpha = 0.6) +
                            geom_smooth(method = "lm", se = T, color = "red",  size = 1.) +
                            facet_wrap(~ label, scales = "free_y") + 
                            theme_minimal() +
                            theme(
                                text = element_text(family = "Arial"),
                                plot.title = element_text(hjust = 0.5, face = "bold"),
                                plot.subtitle = element_text(hjust = 0.5),
                                axis.title.x = element_text(face = "bold", size = 12),
                                axis.title.y = element_text(face = "bold", size = 12),
                                legend.position = "bottom",
                                legend.title = element_text(face = "bold"),
                                strip.text = element_text(size = 14, face = "bold")
                            )#close theme
                    }else{#Line colors specified by the color palette.
                        p <- ggplot(mp_feature(), aes(x = !!sym(group), y = RareAbundance)) +
                            geom_point(color = "#2c3e50", size = 3, alpha = 0.6) +
                            geom_smooth(method = "lm", se = T, color = input$line_color,  size = 1.) +
                            facet_wrap(~ label, scales = "free_y") + 
                            theme_minimal() +
                            theme(
                                text = element_text(family = "Arial"),
                                plot.title = element_text(hjust = 0.5, face = "bold"),
                                plot.subtitle = element_text(hjust = 0.5),
                                axis.title.x = element_text(face = "bold", size = 12),
                                axis.title.y = element_text(face = "bold", size = 12),
                                legend.position = "bottom",
                                legend.title = element_text(face = "bold"),
                                strip.text = element_text(size = 14, face = "bold")
                            )#close theme
                    }#close else
                }else{#Color palette for discrete variables.
                    color_content <- mpse %>% mp_extract_sample %>%
                        select(!!sym(group)) %>% unique #It is a tibble
                    
                    ncolors <- color_content[[1]] %>% length #length of group 
                    color_input <- lapply(seq(ncolors), function (i){
                        input[[paste0("colors",i)]]
                    }) %>% unlist #calling input color by length of group 
                    
                    if(length(color_input) != ncolors) {
                        p <- grouped_ggbetweenstats(mp_feature(),
                                                    plot.type = input$plot.type,
                                                    bf.message = FALSE,
                                                    results.subtitle = input$results.subtitle,
                                                    x = !!sym(group), 
                                                    y = RareAbundance, 
                                                    grouping.var = label, 
                                                    type = "robust")
                        
                    }else{
                        p <- grouped_ggbetweenstats(mp_feature(),
                                                    plot.type = input$plot.type,
                                                    bf.message = FALSE,
                                                    results.subtitle = input$results.subtitle,
                                                    x = !!sym(group), 
                                                    y = RareAbundance, 
                                                    grouping.var = label, 
                                                    type = "robust",
                                                    ggplot.component = list(
                                                        scale_color_manual(values = color_input)
                                                    ))
                    }
                }
                return(p)
            })
          
            
            #Modify color
            color_list <- reactive({
                req(mp_feature())
                input$btn
                group <- isolate({
                    input$group
                })
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
                        )#close colorPickr
                    })#close lapply
                }else{
                    picks <- colorPickr(
                        inputId = ns("line_color"),
                        label = "Line color",
                        selected = "red",
                        swatches = cols,
                        theme = "monolith",
                        useAsButton = TRUE
                    )
                }#close else
                return(picks)
            })
            
            output$plot <- renderPlot({
                req(inherits(p_single_bacteria(), "ggplot"))
                p_single_bacteria()
            })
            
            output$color <- renderUI(
                #req(color_list)
                color_list()
            )
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("feature_composition", input$extPlot, sep='')},
                content = function(file){
                    req(mp_feature())
                    ggsave(file, 
                           plot = p_single_bacteria(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "feature_composition_Data.csv" },
                content = function(file){
                    req(mp_feature())
                    table <- mp_feature()
                    #n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table, 
                              file,
                              row.names = FALSE)
                })
        }
    )
}
