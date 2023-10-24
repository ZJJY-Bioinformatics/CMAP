epide_upload_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Upload Data",
                       status = "warning",
                       collapsible = TRUE,
                       fileInput(ns("meta"), "Upload meta file (.txt |.csv |.xls |.xlsx)",
                                 accept = c(
                                     ".txt",
                                     "text/csv",
                                     "text/tab-separated-values",
                                     "application/vnd.ms-excel",
                                     "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                                 )
                       ),
                       actionButton(ns("btn"), "Submit")
                   )
            ), #close column 3,
            column(9,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Table Preview",
                       status = "warning",
                       collapsible = TRUE,
                       reactable::reactableOutput(outputId = ns("data"))
                   ),
                   br(),
                   textOutput(ns("warning_msg")),
                   jqui_resizable(
                       plotOutput(ns("plot"), width = '600px', height = '400px'),
                       operation = c("enable", "disable", "destroy", "save", "load"),
                       options = list(
                           minHeight = 300, maxHeight = 900,
                           minWidth = 300, maxWidth = 1200)
                   )#close jqui
            )#close column 9
        )#close fluidRow
    )#close div
    
    return(res)
}





epide_upload_mod <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            df <- eventReactive(input$btn, {
                req(input$meta$datapath)

                file_ext <- tools::file_ext(input$meta$datapath)
                
                switch(file_ext,
                       "csv" = {
                           df <- read_csv(input$meta$datapath)
                           output$warning_msg <- renderText(NULL)
                       },
                       "txt" = {
                           df <- read_delim(input$meta$datapath, delim = "\t")
                           output$warning_msg <- renderText(NULL)
                       },
                       "xls" = {
                           df <- read_excel(input$meta$datapath)
                           output$warning_msg <- renderText(NULL)
                       },
                       "xlsx" = {
                           df <- read_excel(input$meta$datapath)
                           output$warning_msg <- renderText(NULL)
                       },
                       {
                           output$warning_msg <- renderText("Unsupported file format!")
                           stop("Unsupported file format!")
                       }
                )
                
                return(df)
            })
            
            observeEvent(df(), {
                output$data <- reactable::renderReactable({
                    req(df())
                    reactable::reactable(df())
                })          
            })
            return(df)
            

               
            #})#close observeEvent
        }#close function
    )#close moduleServer
}#close mode