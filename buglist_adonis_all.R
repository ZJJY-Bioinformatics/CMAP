buglist_adonis_all_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Adonis Analysis",
                       status = "warning",
                       collapsible = TRUE,
                       pickerInput(ns("group"),
                                   "Group:",
                                   NULL,
                                   multiple = TRUE,
                                   options = pickerOptions(actionsBox = TRUE,
                                                           multipleSeparator =" | ",
                                                           liveSearch = TRUE
                                   )
                                   
                       ),
                       actionButton(ns("btn"), "Submit")
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Graphics Options"),
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
                       img(src="Adonis_plot.png",
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

buglist_adonis_all_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            
            observe({
                req(inherits(mpse, "MPSE"))
                lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n[!is.na(n)])))
                max_nrow <- mp_extract_sample(mpse)[-1] %>% nrow
                group <- names(lev[lev > 1 & lev < max_nrow])
                
                updatePickerInput(session, "group",
                                  choices = group,
                                  selected = tail(names(lev[lev == 2]), 1)
                )
                
            })
            mp_all_adonis <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                
                group <- isolate({input$group})
                mpse %<>%
                    mp_decostand(
                        .abundance=Abundance, 
                        method="hellinger")
                
                
                res <- lapply(seq_along(group), function(i) {
                    adonis <- mpse %>% 
                        filter(!is.na(!!sym(group[i]))) %>% 
                        mp_adonis(.abundance=hellinger, 
                                  .formula = as.formula(paste0("~", group[i])), 
                                  distmethod = "bray", 
                                  permutations = 999, 
                                  action = "get")
                    
                    result <- data.frame(var = group[i], R2 = adonis$R2[1], p_value = adonis$`Pr(>F)`[1])
                    return(result)
                    
                }) %>% do.call(rbind, .)
                
                res %>%
                    mutate(pif = case_when(
                        p_value>0.05 ~ "ns",
                        p_value<=0.001 ~ "***",
                        p_value<0.01 ~ "**",
                        p_value<= 0.05 ~ "*",
                    )) -> res
                
                res_ranked <- res %>%
                    arrange(R2) %>%
                    mutate(rank = row_number())
                
                res_ranked$var <- reorder(res_ranked$var, res_ranked$rank)
                #print("1")
                return(res_ranked)
            })
            
            p_all_adonis <- reactive({
                req(mp_all_adonis())
                input$btn
                group <- isolate({
                    input$group
                })
                
                res_ranked <- mp_all_adonis()
                
                if(max(res_ranked$R2) > signif(max(res_ranked$R2), digits = 1)) {
                    lim_max <- (max(res_ranked$R2) %>% signif(digits = 1)) + .05
                }else{
                    lim_max <- (max(res_ranked$R2) %>% signif(digits = 1))
                }
                
                
                p <- ggplot(res_ranked, aes(x = R2, y = var, fill = R2)) +
                    geom_col(width = 0.7, color = "white") +
                    geom_text(aes(label = pif, hjust = 0), size = 5, color = "black") +
                    theme_minimal() +
                    theme(axis.title.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.y = element_text(size = 12, color = "black"),
                          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                          legend.text = element_text(size = 14),
                          legend.title = element_text(size = 16),
                          legend.position = "top") 
                
                
                if(is.null(input$h_color)) {
                    p <- p + 
                        scale_fill_gradient(low = 'navyblue',
                                            high = 'darkorange1',#'darkmagenta',
                                            name = "R2", 
                                            limits = c(0, lim_max),
                                            breaks = c(0,  lim_max), #lim_max/2,
                                            labels = c("Low",  "High"), #"Medium",
                                            guide = guide_colorbar(title = NULL,
                                                                   label.position = "top",
                                                                   barwidth = 10, barheight = 0.5, ticks = FALSE))
                    
                }else{
                    p <- p + 
                        scale_fill_gradient(low = input$l_color,
                                            high = input$h_color,#'darkmagenta',
                                            name = "R2", 
                                            limits = c(0, lim_max),
                                            breaks = c(0,  lim_max), #lim_max/2,
                                            labels = c("Low",  "High"), #"Medium",
                                            guide = guide_colorbar(title = NULL,
                                                                   label.position = "top",
                                                                   barwidth = 10, barheight = 0.5, ticks = FALSE))
                    
                }
                
                return(p)
                
                
            })
            
            output$plot <- renderPlot({
                req(p_all_adonis())
                p_all_adonis()
            })
            
            #Modify color
            color_list <- reactive({
                req(mp_all_adonis())
                input$btn
                
                ns <- NS(id)
                
                high <- colorPickr(
                    inputId = ns("h_color"),
                    label = "High",
                    selected = "#f68521",
                    swatches = cols,
                    theme = "monolith",
                    useAsButton = TRUE
                )
                
                low <- colorPickr(
                    inputId = ns("l_color"),
                    label = "Low ",
                    selected = "#12007e",
                    swatches = cols,
                    theme = "monolith",
                    useAsButton = TRUE
                )
                picks <- list(high, low)
                return(picks)
                
            })
            
            output$color <- renderUI({
                req(mp_all_adonis())
                input$btn
                color_list()
            })
            
            output$downloadPlot <- downloadHandler(
                filename = function(){
                    paste("Adonis_plot", input$extPlot, sep='')},
                content = function(file){
                    req(p_all_adonis())
                    ggsave(file, 
                           plot = p_all_adonis(), 
                           width = input$width_slider, 
                           height = input$height_slider,
                           dpi = input$dpi)
                })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "Adonis.csv" },
                content = function(file){
                    req(mp_all_adonis())
                    table <- mp_all_adonis() 
                    #n <- names(table)[sapply(table, class) == "list"] 
                    write.csv(table, 
                              file,
                              row.names = FALSE)
                })
            
            
            
        }
    )
}


