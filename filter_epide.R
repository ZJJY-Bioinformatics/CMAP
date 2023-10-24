filter_epide_ui <- function(id) {
    ns <- NS(id)
    fluidPage(
        fluidRow(
            column(
                width = 3,
                shinydashboardPlus::box(
                    width = NULL,
                    title = "Filter data",
                    status = "primary",
                    collapsible = TRUE,
                    datamods::filter_data_ui(ns("epide_filter"))
                )
            ),
            column(
                width = 9,
                
                tags$h4("Preview Filter:"),
                shinyWidgets::progressBar(
                    id = ns("pbar"), value = 100,
                    total = 100, display_pct = TRUE
                ),
                reactable::reactableOutput(outputId = ns("table")),
                br(),
                actionButton(ns("saveFilterButton"),
                             "Save Filter Values", 
                             icon = icon("fa-regular fa-pen-to-square"),
                             class = "btn-primary"),
                br(),
                tags$h4("Filtered data:"),
                verbatimTextOutput(outputId = ns("res_str"))
            )
        )
        
    )
    
}

filter_epide_mod <- function(id, epide) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            epide_filter <- reactiveValues(epide = NULL)
            
            observe({
                req(epide)
                data <- reactive({
                    epide
                })
                
                res_filter <- filter_data_server(
                    id = "epide_filter",
                    data = data,
                    name = reactive("metaData"),
                    defaults = reactive(NULL),
                    widget_char = "picker",
                    drop_ids = FALSE,
                    vars = reactive(NULL)
                )
                
                epide_filter$values <<- res_filter$values #a named list
                epide_filter$filtered <<- res_filter$filtered #the data filtered
                
            })
            
            observeEvent(epide_filter$filtered(), {
                updateProgressBar(
                    session = session, id = ns("pbar"),
                    value = nrow(epide_filter$filtered()), total = nrow(epide)
                )
            })
            
            observeEvent(epide_filter$filtered, {

                output$table <- reactable::renderReactable({
                    reactable::reactable(epide_filter$filtered())
                })
                
            })
            
            filtered_epide <- eventReactive(input$saveFilterButton,{
                epide_filter$filtered()
            })
            
            observeEvent(input$saveFilterButton,{
                data_check <- epide_filter$values()
                output$res_str <- renderPrint({
                    str(data_check)
                })
                
            })
            return(filtered_epide)
            
        })
}



