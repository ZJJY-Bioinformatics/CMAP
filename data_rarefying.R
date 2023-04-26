linebreaks <- function(n){HTML(strrep(br(), n))}
error_info <- "Invalid operation: Attempt to delete all data"

data_rarefy_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = 12,
                       title = "Data rarefying",
                       status = "primary",
                       collapsible = TRUE,
                       p("Rarefied species richness for community ecologists."),
                       # materialSwitch(ns("rarefying"), 
                       #                value = TRUE,
                       #                label = "Data rarefying",
                       #                status = "primary"),
                       numericInput(ns("raresize"), "Rare Size", 0, 0, 500000, 5000),
                       br(),
                       fluidRow(
                           column(6,
                                  prettyCheckbox(
                                      inputId = ns("trimOTU"),
                                      label = "trim OTU",
                                      value = TRUE,
                                      status = "danger",
                                      shape = "curve"
                                  )
                           ),
                           column(6,
                                  prettyCheckbox(
                                      inputId = ns("trimSample"),
                                      label = "trim Sample",
                                      value = TRUE,
                                      status = "danger",
                                      shape = "curve"
                                  )
                           )
                           
                       ),
                       actionButton(ns("feature"), "Submit")

                       # br(),
                       # br(),
                       # downloadButton(ns("downloadAbu"), "Abundance Table"),
                       # downloadButton(ns("downloadTax"), "Taxonomy Table")
                   ),
                   uiOutput(ns("status_summary"))
            ),
            column(9,
                   uiOutput(ns("overview"))
            )
        )
    )
    return(res)
}

data_rarefy_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            # res <- eventReactive(input$feature, {
            #     req(inherits(mpse(), "MPSE"))
            #     if(input$rarefying){
            #        res <- mpse() %>% mp_rrarefy
            #     }else{
            #         res <- mpse()
            #     }
            #     return(res)
            # })

            
            mp_rare <- eventReactive(input$feature, {
                req(inherits(mpse(), "MPSE"))
                
                raresize <- isolate({
                    input$raresize
                })
                
                if(raresize == 0){
                    raresize <- NULL
                }
                
                mp_rare <- mpse() %>% mp_rrarefy(raresize = raresize,
                                                trimOTU = input$trimOTU,
                                                trimSample = input$trimSample)
                return(mp_rare)
            })
            
            mp_info <- reactive({
                req(inherits(mp_rare(), "MPSE"))
                x <- rare_info(mpse(),mp_rare())
                return(x)
            })
            
            mp_before <- reactive({
                req(inherits(mp_rare(), "MPSE"))
                x_a <- mpse() %>% mp_extract_assays(.abundance = Abundance) %>% colSums() %>% as.data.frame
                #x_r <- mp_rare() %>% mp_extract_assays(.abundance = RareAbundance) %>% colSums() %>% as.data.frame
                return(x_a)
                
            })
            
            mp_after <- reactive({
                req(inherits(mp_rare(), "MPSE"))
                #x_a <- mp_rare() %>% mp_extract_assays(.abundance = Abundance) %>% colSums() %>% as.data.frame
                x_r <- mp_rare() %>% mp_extract_assays(.abundance = RareAbundance) %>% colSums() %>% as.data.frame
                return(x_r)

            })
            
            
            # mpsum_int <- reactive({
            #     req(inherits(mpse(), "MPSE"))
            #     x <- mpse() %>%
            #         mp_extract_assays(.abundance=Abundance) %>% colSums() %>% as.data.frame
            #     colnames(x) <- "OTU"
            #     x$Sample <- rownames(x)
            #     return(x)
            # })
            # 
            # 
            # 
            # 
            # mpsum_btn <- eventReactive(input$feature, {
            #     req(inherits(mpse(), "MPSE"))
            # 
            #     if(input$rarefying){
            #         abun <- "RareAbundance"
            #     }else{
            #         abun <- "Abundance"
            #     }
            #     x <- mpse() %>%
            #         mp_extract_assays(.abundance=!!sym(abun)) %>% colSums() %>% as.data.frame
            #     colnames(x) <- "OTU"
            #     x$Sample <- rownames(x)
            #     return(x)
            # })
            # 
            # mpsum <- reactive({
            #     req(inherits(mpse(), "MPSE"))
            # if(input$feature) {
            #     return(mpsum_btn())
            # }else{
            #     return(mpsum_int())
            # }
            # })
            
            
            # #Render overview
            output$overview <- renderUI({
                req(inherits(mpse(), "MPSE"))
                fluidRow(
                    shinydashboardPlus::box(
                        width = 12,
                        title = "Library size (before)",
                        status = "success",
                        collapsible = TRUE,
                        div(
                            class = "overview_plot",
                            style = "height:600px; width:95%; overflow:scroll;",
                            plotOutput(ns("plot_before"), width = "95%", height = paste0(nrow(mp_before()) * 20, "px"))
                        )
                    ),
                    shinydashboardPlus::box(
                        width = 12,
                        title = "Library size (after)",
                        status = "success",
                        collapsible = TRUE,
                        div(
                            class = "overview_plot",
                            style = "height:600px; width:95%; overflow:scroll;",
                            plotOutput(ns("plot_after"), width = "95%", height = paste0(nrow(mp_after()) * 20, "px"))
                        )
                    )

                )
            })
            output$plot_after <- renderPlot(res = 90, {
                req(inherits(mp_after(), "data.frame"))
                p <- plot_x(mp_after())
                return(p)
            })
            
            output$plot_before <- renderPlot(res = 90, {
                req(inherits(mp_before(), "data.frame"))
                p <- plot_x(mp_before())
                return(p)
            })
            
            output$status_summary <- renderUI({
                req(inherits(mp_rare(), "MPSE"))
                fluidRow(
                    shinydashboardPlus::box(
                        solidHeader = FALSE,
                        title = "Status summary",
                        background = NULL,
                        width = 12,
                        status = "danger",
                        fluidRow(
                            column(
                                width = 6,
                                descriptionBlock(
                                    #number = paste0(mp_info()[5],"%"), 
                                    #numberColor = "red", 
                                    #numberIcon = icon("caret-down"),
                                    header = mp_info()[1], 
                                    text = "Samples Size", 
                                    rightBorder = TRUE,
                                    marginBottom = FALSE
                                )
                            ),
                            column(
                                width = 6,
                                descriptionBlock(
                                    #number = paste0(mp_info()[6],"%"), 
                                    #numberColor = "red", 
                                    #numberIcon = icon("caret-down"),
                                    header =  mp_info()[2], 
                                    text = "OTU Size", 
                                    rightBorder = FALSE,
                                    marginBottom = FALSE
                                )
                            )
                        ),
                        br(),
                        tags$b("Rare Abundance:"),
                        fluidRow(
                            column(
                                width = 6,
                                descriptionBlock(
                                    number = paste0(mp_info()[5],"%"), 
                                    numberColor = "red", 
                                    numberIcon = icon("caret-down"),
                                    header = mp_info()[3], 
                                    text = "Samples Size", 
                                    rightBorder = TRUE,
                                    marginBottom = FALSE
                                )
                            ),
                            column(
                                width = 6,
                                descriptionBlock(
                                    number = paste0(mp_info()[6],"%"), 
                                    numberColor = "red", 
                                    numberIcon = icon("caret-down"),
                                    header =  mp_info()[4], 
                                    text = "OTU Size", 
                                    rightBorder = FALSE,
                                    marginBottom = FALSE
                                )
                            )
                        )
                    )
                    
                )
            })
            
            # output$downloadAbu <- downloadHandler(
            #     filename = function(){
            #         if(input$rarefying){
            #             "RareAbundance.csv"
            #         }else{
            #             "Abundance.csv"
            #         }
            #         },
            #    content = function(file){
            #        req(inherits(mpse(), "MPSE"))
            #        if(input$rarefying){
            #            abun <- "RareAbundance"
            #        }else{
            #            abun <- "Abundance"
            #        }
            #        
            #         table <- mpse() %>% mp_extract_assays(.abundance = !!sym(abun))
            #         write.csv(table, 
            #                   file)
            #     })
            
            # output$downloadTax <- downloadHandler(
            #     filename = function(){"Taxonomy_annotation.csv"},
            #     content = function(file){
            #         req(inherits(mpse(), "MPSE"))
            #         table <- mpse() %>% mp_extract_taxonomy
            #         write.csv(table, 
            #                   file,
            #                   row.names = FALSE)
            #     })
            
            return(mp_rare)
        })
}