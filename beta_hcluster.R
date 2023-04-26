beta_hcluster_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "hcluster-body",
        fluidRow(
            column(
                width = 3,
                shinydashboardPlus::box(
                    width = NULL,
                    title = "Hierarchical clustering analysis",
                    status = "warning",
                    collapsible = TRUE,
                    fluidRow(
                        column(width = 6,
                               style=list("padding-right: 5px;"),
                               pickerInput(ns("std_method"),
                                           "Standardization method:",
                                           choices = std_method,
                                           selected = "hellinger"
                               )),
                        column(width = 6,
                               style=list("padding-left: 5px;"),
                               pickerInput(ns("dist_method"),
                                           "Distance method:",
                                           choices = dist_method,
                                           selected = "bray"
                               ))
                    ),
                    fluidRow(
                        column(width = 6,
                               style=list("padding-right: 5px;"),
                               pickerInput(ns("hclust_method"),
                                           "Clustering method:",
                                           choices = c(
                                               "average",
                                               "single",
                                               "complete")
                               )
                        ),
                        column(width = 6,
                               style=list("padding-left: 5px;"),
                               pickerInput(ns("group"), "Group:", NULL)
                        )
                    ),
                    fluidRow(
                        column(width = 6,
                               style=list("padding-right: 5px;"),
                               pickerInput(ns("taxonomy"), "taxonomy:", NULL)
                        ),
                        column(width = 6,
                               style=list("padding-left: 5px;"),
                               numericInput(ns("topn"), "Abundance top N:", 15, 5, 50)
                        )
                    ),
                   
                    actionButton(ns("btn"), "Submit")
                ),
                tabBox(width = NULL,
                       tabPanel(
                           h5("Color"),
                           fluidRow(
                               column(6,
                                      tags$b("Group color:"),
                                      uiOutput(ns("color"))),
                               column(6,
                                      tags$b("Taxonomy color:"),
                                      uiOutput(ns("fill")))
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
                    img(src="hcluster_plot.png",
                        align = "center",
                        width = "100%")
                )
            ),
            column(width = 9,
                   jqui_resizable(
                       plotOutput(ns("hcluster_plot"), width = '450px', height = '600px'),
                       operation = c("enable", "disable", "destroy", "save", "load"),
                       options = list(
                           minHeight = 300, maxHeight = 900,
                           minWidth = 300, maxWidth = 1200
                       )
                   )
            )
        )
    )
    return(res)
}

beta_hcluster_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            treeda <- reactiveVal({
                readRDS("data/treeda.rds")
            })
            
            observe({
                req(inherits(mpse, "MPSE"))
                lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
                group <- names(lev[lev > 1])
                tax <- mp_extract_taxonomy(mpse) %>% names()
                updatePickerInput(session, "group",
                                  choices = group,
                                  selected = tail(names(lev[lev == 2]), 1)
                )
                updatePickerInput(session, "taxonomy",
                                  choices = tax,
                                  selected = tail(tax)
                )
                if (!is.null(treeda())) {
                    updatePickerInput(session, "dist_method",
                                      choices = c(dist_method,
                                                  c("unweighted uniFrac" = "unifrac", 
                                                    "weighted uniFrac" = "wunifrac"))
                    )
                }
            })
            
            mp_hcl <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                
                std <- isolate({
                    input$std_method
                })
                dist <- isolate({
                    input$dist_method
                })
                if (dist %in% c("unifrac", "wunifrac")) {
                    otutree(mpse) <- treeda()
                }
                
                mpse %>%
                    mp_decostand(.abundance = RareAbundance, method = std) %>%
                    mp_cal_clust(.abundance = !!std, distmethod = dist, action = "get") #note: action = "get", tree data
            })
            
            tax.tb <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                input$submit
                
                group <- isolate({
                    input$group
                })
                
                taxonomy <- isolate({
                    input$taxonomy
                })
                # topn <- isolate({
                #     input$topn
                # })
                
                
                tb <- mpse %>%
                    mp_cal_abundance( # for each samples
                        .abundance = RareAbundance
                    )%>%
                    mp_cal_abundance( # for each groups
                        .abundance = RareAbundance,
                        .group = group
                    ) %>% 
                    mp_extract_abundance(
                        taxa.class = !!sym(taxonomy),
                        topn = input$topn
                    ) %>%
                    tidyr::unnest(cols = RareAbundanceBySample) #%>%
                    #dplyr::rename(!!sym(taxonomy) := "label")
                
                tb <- tb %>%  filter(label != "Others") %>%
                    dplyr::rename(!!sym(taxonomy) := "label")
                
                return(tb)
            })
            
            
            p_hcluster <- reactive({
                req(inherits(mp_hcl(), "treedata"))
                input$btn
                group <- isolate({
                    input$group
                })
                # layout <- isolate({
                #     input$layout
                # })
                taxonomy <- isolate({
                    input$taxonomy
                })
                dist <- isolate({
                    input$dist_method
                })
               
                p <- ggtree(mp_hcl(), layout = "rectangular") +
                    geom_treescale(fontsize = 2) +
                    geom_tippoint(mapping=aes(color = !!sym(group))) +
                    geom_fruit(
                        data = tax.tb(),
                        geom = geom_col,
                        mapping = aes(x = RelRareAbundanceBySample, y = Sample, fill = !!sym(taxonomy)),
                        orientation = "y",
                        offset = 0.08,
                        pwidth = 3,
                        width = .6
                        # axis.params = list(
                        #     axis = "x",
                        #     #title = paste0("The relative abundance of ",taxonomy," (%)"),
                        #     #title.size = 3,
                        #     title.height = 0.04,
                        #     text.size = 2,
                        #     vjust = 1
                        # )
                    ) +
                    geom_tiplab(as_ylab = TRUE) +
                    scale_x_continuous(expand = c(0, 0.01))

                #add color and fill
                color_content <- mpse %>% mp_extract_sample %>%
                    select(!!sym(group)) %>% unique #It is a tibble, "Group" content

                taxonomy_content <- mpse %>%
                    mp_extract_taxonomy %>%
                    select(!!sym(taxonomy)) %>%
                    unique

                if(color_content[[1]] %>% is.numeric) {
                    return(p)
                }

                ncolors <- color_content[[1]] %>% length
                color_input <- lapply(seq(ncolors), function (i){
                    input[[paste0("colors",i)]]
                }) %>% unlist #calling input color

                ntax <- taxonomy_content[[1]] %>% length
                fill_input <- lapply(seq(ntax), function (i){
                    input[[paste0("fill",i)]]
                }) %>% unlist #calling fill cols from "fill_list"


                # if(length(color_input) != ncolors) {
                #     p <- p +
                #         scale_color_manual(values = cc(ncolors)) +
                #         scale_fill_manual(values = cols2(ntax))
                # }else{
                    p <- p +
                        scale_color_manual(values = color_input,
                                           guide = guide_legend(
                                               keywidth = .5,
                                               keyheight = .5,
                                               title.theme = element_text(size = 8),
                                               label.theme = element_text(size = 6)
                                           )) +
                        scale_fill_manual(values = fill_input,
                                          guide = guide_legend(
                                              keywidth = .5,
                                              keyheight = .5,
                                              title.theme = element_text(size = 8),
                                              label.theme = element_text(size = 6)
                                          ))
                #}

                return(p)
            })
            
            output$hcluster_plot <- renderPlot({
                req(p_hcluster())
                p_hcluster()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("hcluster_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_hcluster())
                    ggsave(file, 
                           plot = p_hcluster(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "hcluster_Data.csv" },
                content = function(file){
                    req(p_hcluster())
                    table <- p_hcluster()$data
                    n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table %>% select(-c(n)), 
                              file,
                              row.names = FALSE)
                })
            
            #Modify color
            color_list <- reactive({
                req(mp_hcl())
                input$btn
                group <- isolate({
                    input$group
                })

                ns <- NS(id)
                color_content <- mpse %>% mp_extract_sample %>% 
                    select(!!sym(group)) %>% unique #It is a tibble
                name_colors <- color_content[[1]] %>% sort #getting  chr.
                pal <- cc(length(name_colors)) #calling color palette
                names(pal) <- name_colors #mapping names to colors 
            
                picks <- lapply(seq(pal), function(i) {#building multiple color pickers
                    colorPickr(
                        inputId = ns(paste0("colors",i)),
                        label = names(pal[i]),
                        #swatches = scales::viridis_pal()(10),
                        selected = pal[[i]],
                        swatches = cols,
                        theme = "monolith",
                        useAsButton = TRUE
                    )
                })
           return(picks)
            })
            
            #Modify color
            fill_list <- reactive({
                req(mp_hcl())
                input$btn
                taxonomy <- isolate({
                    input$taxonomy
                })
                taxonomy_content <- mpse %>% 
                    mp_extract_taxonomy %>% 
                    select(!!sym(taxonomy)) %>% 
                    unique

               # name_fills <- taxonomy_content[[1]] %>% sort #getting  chr.
                name_fills <- tax.tb() %>% select(!!sym(taxonomy))
                name_fills <- name_fills[[1]] %>% levels
                name_fills <- name_fills[name_fills != "Others"]
                
                pal <- cols2(length(name_fills)) #calling fill palette 2
                names(pal) <- name_fills #mapping names to colors 
                
                ns <- NS(id)
                picks <- lapply(seq(pal), function(i) {#building multiple color pickers
                    colorPickr(
                        inputId = ns(paste0("fill",i)),
                        label = names(pal[i]),
                        #swatches = scales::viridis_pal()(10),
                        selected = pal[[i]],
                        swatches = cols,
                        theme = "monolith",
                        useAsButton = TRUE
                    )
                })
                return(picks)
            })
            
            output$color <- renderUI(
                #req(color_list)
                color_list()
                )
            
            output$fill<- renderUI(
                #req(color_list)
                fill_list()
            )
            
        }
    )
}

