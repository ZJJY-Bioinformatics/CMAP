descriptive_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Descriptive Statistics",
                       status = "warning",
                       collapsible = TRUE,
                       #uiOutput(ns("picker1")),
                       pickerInput(ns("treatment"),
                                   "Continuous Variable:",
                                   NULL,
                                   multiple = TRUE
                       ) %>%
                           shinyInput_label_embed(
                               icon("circle-question") %>%
                                   bs_embed_tooltip(title = "Continuous variables only")
                           ),
                       pickerInput(ns("discrete"),
                                   "Discrete Variable:",
                                   NULL,
                                   multiple = TRUE
                       ) %>%
                           shinyInput_label_embed(
                               icon("circle-question") %>%
                                   bs_embed_tooltip(title = "Discrete variables only")
                           ),
                       actionButton(ns("btn"), "Submit")
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Download"),
                                   downloadButton(ns("downloadTable"), "Download Table")
                          )
                   )
            ), #close column 3,
            column(9,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Continuous variables",
                       status = "warning",
                       collapsible = TRUE,
                       reactable::reactableOutput(outputId = ns("data"))
                   ),
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Discrete variables",
                       status = "warning",
                       collapsible = TRUE,
                       reactable::reactableOutput(outputId = ns("data2"))
                   )
            )#close column 9
        )#close fluidRow
    )#close div
    
    return(res)
}


descriptive_mod <- function(id, epide) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            
            observe({
                req(epide)
                df <- epide
                sample_na <- df %>% names
                treatment_name <- sample_na[unlist(lapply(df,class) %in% "numeric")]
                discrete_name <- sample_na[!unlist(lapply(df,class) %in% "numeric")]
                updatePickerInput(session, "discrete", choices = discrete_name,
                                  selected = NULL)
                
                updatePickerInput(session, "treatment", choices = treatment_name,
                                  selected = NULL)
            })
            
            summary_stats <- eventReactive(input$btn, {
                req(epide)
                data <- epide
                summary_stats <- calculate_summary_stats(epide, input$treatment)
                return(summary_stats)
            })
            
            observeEvent(summary_stats(), {
                output$data <- reactable::renderReactable({
                    req(summary_stats())
                    reactable::reactable(summary_stats())
                })          
            })
            
            summary_stats2 <- eventReactive(input$btn, {
                req(epide)
                data <- epide
                summary_stats <- calculate_summary_stats2(epide, input$discrete)
                return(summary_stats)
            })
            
            observeEvent(summary_stats2(), {
                output$data2 <- reactable::renderReactable({
                    req(summary_stats2())
                    reactable::reactable(summary_stats2())
                })          
            })
            
            
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "summary_stats.csv" },
                content = function(file){
                    req(summary_stats())
                    table <- summary_stats()
                    write.csv(table,
                              file,
                              row.names = FALSE)
                })
        }
    )
}