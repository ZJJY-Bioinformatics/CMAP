upload_buglist_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "upload-body",
        fluidRow(
            column(4,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Upload data",
                       status = "primary",
                       collapsible = TRUE,
                       fileInput(ns("otuda_metaG"),
                                 label = "Abundance profile (tab-separated file):",
                                 accept = c(".txt", "tsv", "xls")
                       ),
                       fileInput(ns("metada_metaG"),
                                 label = "Metadata file (.txt):",
                                 accept = c(".txt")
                       ),
                       fluidRow(
                           column(4, 
                                  actionButton(ns("btn"), "Submit")),
                           column(4,
                                  downloadButton(ns("example"), "Example Data")),
                           column(4,
                                  downloadButton(ns("tutorial"), "Tutorial"))
                       )
                   )
                   
            ),
            column(8,
                   shinyjs::hidden(
                       div(id = "hiddenbox",
                           shinydashboardPlus::box(
                               width = NULL,
                               title = "Data preview",
                               status = "success",
                               solidHeader = FALSE,
                               collapsible = TRUE,
                               verbatimTextOutput(ns("mp_print"))
                           )
                       )
                   )
            )
        )
        
    )
    return(res)
}


upload_buglist_mod <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            #download example data 
            output$example <- downloadHandler(
                filename = function() {
                    "CMAP_example_data.zip"
                },
                content = function(filename) {
                    zip(filename, "data/example_data_20211016", flags = "-r9Xj")
                },
                contentType = "application/zip"
            )
            
            #download tutorial
            output$tutorial <- downloadHandler(
                filename = function() {
                    "CMAP_Tutorial.zip"
                },
                content = function(filename) {
                    zip(filename, "data/CMAP_Tutorial.pdf", flags = "-r9Xj")
                },
                contentType = "application/zip"
            )

            #covert data to a MPSE class
            mpse <- eventReactive(input$btn, {
                req(input$otuda_metaG, input$metada_metaG)
                mpse <- upload_buglist(input$otuda_metaG$datapath, 
                                       input$metada_metaG$datapath)
                if (!inherits(mpse, "MPSE")) {
                    showNotification("An error occurred while parsing the file",
                                     type = "error"
                    )
                }
                return(mpse)
            })
            
            # ----show hiddenbox----
            observeEvent(input$btn, {
                shinyjs::show(id = "hiddenbox")
            })
            
            output$mp_print <- renderPrint({
                mpse() %>% as.tibble()
                })
 
            
            return(mpse)
        }
    )
}
