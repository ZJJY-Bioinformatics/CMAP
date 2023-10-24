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
                       uiOutput(ns("picker1")),
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
                       title = "Table",
                       status = "warning",
                       collapsible = TRUE,
                       reactable::reactableOutput(outputId = ns("data"))
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
            
                #update pickerInput
                output$picker1 <- renderUI({
                    req(epide)
                    df <- epide
                    sample_na <- df %>% names
                    treatment_name <- sample_na[unlist(lapply(df,class) %in% "numeric")]
                    
                    pickerInput(ns("treatment"), "Variable:", 

                                choices = treatment_name,
                                options = list(`actions-box` = TRUE)) %>%
                        shinyInput_label_embed(
                            icon("circle-question") %>%
                                bs_embed_tooltip(title = "Continuous variables only")
                        )
                })
                
                # output$picker2 <- renderUI({
                #     req(epide)
                #     pickerInput(ns("covariates"), "Covariates:", 
                #                 choices = colnames(epide), 
                #                 multiple = TRUE,
                #                 options = list(`actions-box` = TRUE, `multiple-separator` = " | ")) %>% 
                #         shinyInput_label_embed(
                #             icon("circle-question") %>%
                #                 bs_embed_tooltip(title = "Covariates are other variables that may influence the relationship between the treatment variable and the outcome variable.")
                #         )
                # })
                
                
                
                # output$data <- reactable::renderReactable({
                #     req(epide)
                #     reactable::reactable(epide)
                # })
                
            #})#close observeEvent
            
    
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