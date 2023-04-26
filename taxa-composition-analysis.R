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
                                  pickerInput(ns("ytype"), "Y axis:", c("relative", "count"))
                           ),
                           column(6,
                                  pickerInput(ns("xtype"), "X axis:", c("group","sample"))
                           )
                       ),
                       numericInput(ns("topn"), "Top most abundant:", value = 10),
                       actionButton(ns("btn"), "Submit")
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Graphics Options"),
                                   uiOutput(ns("sampele_order"))
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

        # shinydashboardPlus::box(
        #     width = 12, title = "Taxonomy composition Analysis",
        #     status = "warning", collapsible = TRUE,
        #     pickerInput(ns("level"),
        #                 "Taxonomic level:",
        #                 choices = NULL),
        #     pickerInput(ns("group"), "Group:", NULL),
        #     pickerInput(ns("ytype"), "Y axis:", c("relative", "count")),
        #     pickerInput(ns("xtype"), "X axis:", c("sample", "group")),
        #     numericInput(ns("topn"), "Top most abundant:", value = 10),
        #     actionButton(ns("btn"), "Submit")
        # ),
        # shinydashboardPlus::box(
        #     width = 12,
        #     title = "Plot Download",
        #     status = "success",
        #     solidHeader = FALSE,
        #     collapsible = TRUE,
        #     plotOutput(ns("taxa_composition_plot")),
        #     numericInput(ns("width_slider"), "width:", 10,1, 20),
        #     numericInput(ns("height_slider"), "height:", 8, 1, 20),
        #     radioButtons(inputId = ns('extPlot'),
        #                  label = 'Output format',
        #                  choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
        #                  inline = TRUE),
        #     downloadButton(ns("downloadPlot"), "Download Plot"),
        #     downloadButton(ns("downloadTable"), "Download Table")
        # )
        # fluidRow(),
        # jqui_resizable(
        #     plotOutput(ns("plot"), width = "900px"),
        #     operation = c("enable", "disable", "destroy", "save", "load"),
        #     options = list(
        #         minHeight = 300, maxHeight = 900,
        #         minWidth = 600, maxWidth = 1200
        #     )
        # )
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
                input$btn
                # level <- isolate({input$level})
                # is.relative <- isolate({
                #     ifelse(input$ytype == "relative", TRUE, FALSE)
                # })
                group <- isolate({input$group})
                # topn <- isolate({input$topn})
                # is.group <- isolate({
                #     ifelse(input$xtype == "group", TRUE, FALSE)
                # })
                
                mp_comp <- mpse %>%
                    mp_cal_abundance( # for each samples
                        .abundance = RareAbundance
                    ) %>%
                    mp_cal_abundance( # for each groups 
                        .abundance=RareAbundance,
                        .group= group
                    )
                return(mp_comp)
            })
                # p <- mpse %>%
                # mp_plot_abundance(.abundance = Abundance,
                #                   .group = !!sym(group),
                #                   taxa.class = !!sym(level),
                #                   topn = topn,
                #                   relative = is.relative,
                #                   plot.group = is.group,
                #                   force = TRUE) #+ 
                    # theme(
                    #     text = element_text(size = 18, family = "serif"),
                    #     axis.text.x = element_text(size = 11, family = "serif"),
                    #     axis.text.y = element_text(size = 16, family = "serif"),
                    #     legend.text = element_text(size = 16, family = "serif")
                    # )
                
                # x_level <- lapply(seq(sampele_level()), function (i){
                #     input[[paste0("items",i)]]
                # }) %>% unlist
                # 
                # p$data %<>% mutate(Sample = factor(Sample, levels = x_level))
            #      return(p)
            # })
            
            p_taxa_composition <- reactive({
                req(inherits(mp_comp(), "MPSE"))
                
                level <- isolate({input$level})
                is.relative <- isolate({
                    ifelse(input$ytype == "relative", TRUE, FALSE)
                })
                group <- isolate({input$group})
                topn <- isolate({input$topn})
                is.group <- isolate({
                    ifelse(input$xtype == "group", TRUE, FALSE)
                })
                
                p <- mp_comp() %>%
                    mp_plot_abundance(.abundance = RareAbundance,
                                      .group = !!sym(group),
                                      taxa.class = !!sym(level),
                                      topn = topn,
                                      relative = is.relative,
                                      plot.group = is.group,
                                      force = TRUE) +
                    theme(
                        text = element_text(size = 18, family = "serif"),
                        axis.text.x = element_text(size = 11, family = "serif"),
                        axis.text.y = element_text(size = 16, family = "serif"),
                        legend.text = element_text(size = 16, family = "serif")
                    )
                # print(p$data[[input$group]])
                # print(input$items_group)
                
                if(is.character(mp_extract_sample(mpse)[[group]])){
                    if(!is.null(input$items_group)){
                        p$data[[input$group]] %<>% factor(level = input$items_group)
                    }
                }
                
                # if(input$xtype == "sample") {
                #     x_level <- lapply(seq(sampele_level()), function (i){
                #         input[[paste0("items",i)]]
                #     }) %>% unlist
                #     
                #     p$data %<>% mutate(Sample = factor(Sample, levels = x_level))
                # }else {
                #     p$data[[group]] %<>% factor(level = input$items_group)
                # }

                return(p)
            })
            
            # observeEvent(input$go, {
            #     values$myresults <- sample(1:20, 5, T)
            # })  
            
            
            # sampele_level <- reactive({ #get sample level from mpse
            #     req(p_taxa_composition())
            #     group <- isolate({input$group})
            #     #group_contents <- mpse %>% select(!!sym(group)) %>% unique
            #     group_contents <-  p_taxa_composition()$data %>% select(!!sym(group)) %>% unique
            #     group_contents <- group_contents[[1]]
            #     
            #     # if(input$xtype == "sample") {
            #     #     sample2group <- lapply(group_contents, function(i) {
            #     #         mpse %>% filter(!!sym(group) == i) %>% colnames %>% sort
            #     #     })
            #     #     names(sample2group) <- group_contents
            #     #     return(sample2group)
            #    # }else {
            #         return(group_contents)
            #     #}
            # })
            
            output$taxa_composition_plot <- renderPlot({
                req(p_taxa_composition())
                p_taxa_composition()
            })
         
            
            output$sampele_order <-  renderUI({ #order UI
                req(mp_comp())
                group <- isolate({input$group})
                group_contents <- mp_extract_sample(mpse)[[input$group]] %>% unique
                if(input$xtype == "sample") {
                    # lapply(seq(sampele_level()), function(i) {
                    #     orderInput(inputId = ns(paste0("items",i)),
                    #                label = paste0("Order of ", group, "--",
                    #                               names(sampele_level())[i]),
                    #                items = sampele_level()[[i]] )
                    # 
                    # })
                    NULL
                    }else{
                    orderInput(ns('items_group'), 
                               'Boxes order (Drag items below)', 
                               items = group_contents
                                   #sampele_level()
                               )
                }
            })
         

            
            
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
                   ))
        )
        # shinydashboardPlus::box(
        #     width = 12, title = "Taxonomy composition Analysis",
        #     status = "warning", collapsible = TRUE,
        #     pickerInput(ns("level"),
        #                 "Taxonomic level:",
        #                 choices = NULL),
        #     pickerInput(ns("feature"), NULL, NULL,
        #     options = list(`actions-box` = TRUE), multiple = TRUE),
        #     pickerInput(ns("group"), "Group:", NULL),
        #     actionButton(ns("btn"), "Submit")
        # ),
        # shinydashboardPlus::box(
        #     width = 12,
        #     title = "Plot Download",
        #     status = "success",
        #     solidHeader = FALSE,
        #     collapsible = TRUE,
        #     plotOutput(ns("plot")),
        #     numericInput(ns("width_slider"), "width:", 10,1, 20),
        #     numericInput(ns("height_slider"), "height:", 8, 1, 20),
        #     radioButtons(inputId = ns('extPlot'),
        #                  label = 'Output format',
        #                  choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
        #                  inline = TRUE),
        #     downloadButton(ns("downloadPlot"), "Download Plot"),
        #     downloadButton(ns("downloadTable"), "Download Table")
        # )
        # fluidRow(),
        # jqui_resizable(
        #     plotOutput(ns("plot"), width = "900px"),
        #     operation = c("enable", "disable", "destroy", "save", "load"),
        #     options = list(
        #         minHeight = 300, maxHeight = 900,
        #         minWidth = 600, maxWidth = 1200
        #     )
        # )
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
            
            mp_comp <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                level <- isolate({input$level})
                group <- isolate({input$group})
                feature <- isolate({input$feature})
                mpse %>%
                    mp_cal_abundance(.abundance=Abundance, action="add", force = T) %>%
                    mp_extract_tree() %>%
                    tidyr::unnest(AbundanceBySample) %>%
                    dplyr::filter(nodeClass == level) %>%
                    dplyr::filter(label %in% feature) #%>%
                    #grouped_ggbetweenstats(x = !!sym(group), y = RelAbundanceBySample, grouping.var = label, type = "robust")
            })

            p_single_bacteria <- reactive({
                req(mp_comp())
                input$btn
                group <- isolate({input$group})

                if (is.numeric(mp_comp()[[group]]) && type == "continuous") {
                    stop("Does not apply to continuous variables")
                } else {
                    color_content <- mpse %>% mp_extract_sample %>%
                        select(!!sym(group)) %>% unique #It is a tibble
                    
                    ncolors <- color_content[[1]] %>% length #length of group 
                    color_input <- lapply(seq(ncolors), function (i){
                        input[[paste0("colors",i)]]
                    }) %>% unlist #calling input color by length of group 
                    
                    if(length(color_input) != ncolors) {
                        p <- grouped_ggbetweenstats(mp_comp(),
                                                    x = !!sym(group), 
                                                    y = RelAbundanceBySample, 
                                                    grouping.var = label, 
                                                    type = "robust")
                        
                    }else{
                        p <- grouped_ggbetweenstats(mp_comp(),
                                                    x = !!sym(group), 
                                                    y = RelAbundanceBySample, 
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
                req(mp_comp())
                input$btn
                group <- isolate({
                    input$group
                })
                ns <- NS(id)
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
                    req(mp_comp())
                    ggsave(file, 
                           plot = p_single_bacteria(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "feature_composition_Data.csv" },
                content = function(file){
                    req(mp_comp())
                    table <- mp_comp()$data
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
        }
    )
}
