abu_table_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        fluidRow(
            column(
                width = 3,
                shinydashboardPlus::box(
                    width = NULL,
                    title = "Abundance Extraction",
                    status = "warning",
                    collapsible = TRUE,
                    p("Select Taxonomic Level, Extract Microbiota Abundance"),
                    fluidRow(
                        column(width = 6,
                               style=list("padding-right: 5px;"),
                               pickerInput(ns("taxonomy"), "taxonomy:", NULL)
                        )
                    ),
                    actionButton(ns("btn"), "Extract")
                ),
                tabBox(width = NULL,
                       tabPanel(h5("Download"),
                                fluidRow(
                                    column(width = 6,
                                           downloadButton(ns("download"), "Download Table"))
                                )#close fluidRow
                       )#close tabPanel
                )#close tabBox
            ),
            column(width = 9,
                   reactable::reactableOutput(outputId = ns("table"))
            )
        )
    )
    return(res)
}

abu_table_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
            observe({
                req(inherits(mpse, "MPSE"))
                tax <- mp_extract_taxonomy(mpse) %>% names()
                updatePickerInput(session, "taxonomy",
                                  choices = tax,
                                  selected = tail(tax)
                )
            })
            
            mp_cal_abu <- eventReactive(input$btn, {
                req(inherits(mpse, "MPSE"))
                
                mpse %<>%
                    mp_cal_abundance( # for each samples
                        .abundance = RareAbundance
                    )
                return(mpse)

            })
            
            tax_table <- reactive({
                req(inherits(mp_cal_abu(), "MPSE"))

                mp <- mp_cal_abu()
                table <- mp %>%
                mp_extract_abundance(taxa.class = !!sym(input$taxonomy)) %>%
                tidyr::unnest(cols = RareAbundanceBySample)
                return(table)
            })
            
            
            observeEvent(tax_table, {
                output$table <- reactable::renderReactable({
                    reactable::reactable(tax_table())
                })
            })
            
            output$download <- downloadHandler(
                filename = function(){"tax_abundance.csv"},
                content = function(file){
                    req(inherits(mpse, "MPSE"))
                    write.csv(tax_table(), 
                              file,
                              row.names = FALSE)
                })
       
        }
    )#close moduleServer
}










