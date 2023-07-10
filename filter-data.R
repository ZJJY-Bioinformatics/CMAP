filter_ui <- function(id) {
    ns <- NS(id)
    fluidPage(
        fluidRow(
            column(
                width = 3,
                shinydashboardPlus::box(
                    width = NULL,
                    title = "Filter meta data",
                    status = "primary",
                    collapsible = TRUE,
                    datamods::filter_data_ui(ns("mp_filter"))
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

filter_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            mpse_filter <- reactiveValues(mpse = NULL)
            
            observe({
                req(inherits(mpse, "MPSE"))
                metaData <- mp_extract_sample(mpse)
                
                data <- reactive({
                    metaData
                })
         
            res_filter <- filter_data_server(
                id = "mp_filter",
                data = data,
                name = reactive("metaData"),
                defaults = reactive(NULL),
                widget_char = "picker",
                drop_ids = FALSE,
                vars = reactive(NULL)
            )
                
            mpse_filter$values <<- res_filter$values
            mpse_filter$filtered <<- res_filter$filtered
                
            })
            
            observeEvent(mpse_filter$filtered(), {
                updateProgressBar(
                    session = session, id = ns("pbar"),
                    value = nrow(mpse_filter$filtered()), total = nrow(mp_extract_sample(mpse))
                )
            })
            
            observeEvent(mpse_filter$filtered, {
                req(inherits(mpse, "MPSE"))
                output$table <- reactable::renderReactable({
                    reactable::reactable(mpse_filter$filtered())
                })

            })

            filtered_mp <- eventReactive(input$saveFilterButton,{
                req(inherits(mpse, "MPSE"))

                x <- (mpse_filter$filtered())[["Sample"]]
                #x <- mpse %>% filter(Sample %in% x)
                return(x)
            })
            
            observeEvent(input$saveFilterButton,{
                req(inherits(mpse, "MPSE"))
                data_check <- mpse_filter$values()
                output$res_str <- renderPrint({
                    str(data_check)
                    })
             
            })
            return(filtered_mp)

        })
}



