psm_method <- c("nearest", "optimal", "full", "genetic", "cem", "cardinality", "subclass")
psm_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        #class = "tab-body",
        fluidRow(
            column(3,
                   shinydashboardPlus::box(
                       width = NULL,
                       title = "Propensity Score Matching",
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
                       uiOutput(ns("picker1")),
                       uiOutput(ns("picker2")),
                       fluidRow(
                           column(6, 
                                  pickerInput(ns("method"),
                                              "Method",
                                              choices = psm_method,
                                              selected = "nearest"
                                  )%>% 
                                      shinyInput_label_embed(
                                          icon("circle-question") %>%
                                              bs_embed_tooltip(title = "The matching method to be used.")
                                      )
                           ),
                           column(6,
                                  numericInput(ns("ratio"), "ratio:", 1, 1, 5, 1)  %>% 
                                      shinyInput_label_embed(
                                          icon("circle-question") %>%
                                              bs_embed_tooltip(title = "for methods that allow it, how many control units should be matched to each treated unit in k:1 matching. Should be a single integer value. See the individual methods pages for information on whether and how this argument is used. The default is 1 for 1:1 matching.")
                                      )
                           ),
                       ),
                       actionButton(ns("btn"), "Submit")
                   ),
                   tabBox(width = NULL,
                          tabPanel(h5("Download"),
                                   downloadButton(ns("downloadTable"), "Download Table")
                          )
                   ),
                   shinydashboard::box(
                       title = "About",
                       width = NULL,
                       collapsible = TRUE,
                       status = "primary",
                       div(
                           class = "about-text scrollable-text",
                           style = "text-align: justify; font-size: 1em;",
                           p(HTML("Please note that when assessing the balance of covariates between the treated and control groups after Propensity Score Matching,<strong> an Absolute Standardized Mean Difference (ASMD) value of less than 0.1 is generally considered acceptable.</strong> This indicates a good balance between the groups and suggests that the PSM has performed well in reducing potential selection bias.")),
                           br(),
                           p("Propensity Score Matching (PSM) is to estimate the causal effect of a treatment variable on an outcome variable by reducing potential selection bias and confounding, by balancing the distribution of observed covariates between the treatment and control groups. This creates comparable groups that mimic the characteristics of a randomized controlled trial, thereby allowing for more reliable causal inference in observational studies."),
                           br(),
                           p(HTML("<em><strong>Cite:</strong> MatchIt: Nonparametric Preprocessing for Parametric Causal Inference. Journal of Statistical Software, 42(8). <a href='https://doi.org/10.18637/jss.v042.i08' target='_blank'>doi:10.18637/jss.v042.i08</a></em>"))
                       )
                   )#close About box
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





psm_mod <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            
            #add PSM-20230506
            require(MatchIt)
            require(optmatch)
            require(Matching)
            require(rgenoud)
            require(Rglpk)
            require(readxl)
            require(readr)
            
            data <- reactiveVal(NULL)
            observeEvent(input$meta, {
                req(input$meta)
                
                file_ext <- tools::file_ext(input$meta$datapath)
                
                switch(file_ext,
                       "csv" = {
                           data(read_csv(input$meta$datapath))
                           output$warning_msg <- renderText(NULL)
                       },
                       "txt" = {
                           data(read_delim(input$meta$datapath, delim = "\t"))
                           output$warning_msg <- renderText(NULL)
                       },
                       "xls" = {
                           data(read_excel(input$meta$datapath))
                           output$warning_msg <- renderText(NULL)
                       },
                       "xlsx" = {
                           data(read_excel(input$meta$datapath))
                           output$warning_msg <- renderText(NULL)
                       },
                       {
                           output$warning_msg <- renderText("Unsupported file format!")
                           stop("Unsupported file format!")
                       }
                )
                
                #update pickerInput
                output$picker1 <- renderUI({
                    req(data())
                    df <- data()
                    sample_na <- df %>% names
                    treatment_name <- sample_na[unlist(lapply(df,class) %in% "numeric")]
                    
                    pickerInput(ns("treatment"), "Treatment variable:", 
                                #choices = colnames(data()), 
                                choices = treatment_name,
                                options = list(`actions-box` = TRUE)) %>%
                        shinyInput_label_embed(
                            icon("circle-question") %>%
                                bs_embed_tooltip(title = "A binary parameter that accepts only numeric values of either 0 or 1, where 0 indicates the absence of a specific intervention or treatment, and 1 indicates the presence of the intervention or treatment being applied to the subject.")
                        )
                })
                
                output$picker2 <- renderUI({
                    req(data())
                    pickerInput(ns("covariates"), "Covariates:", 
                                choices = colnames(data()), 
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE, `multiple-separator` = " | ")) %>% 
                        shinyInput_label_embed(
                                    icon("circle-question") %>%
                                        bs_embed_tooltip(title = "Covariates are other variables that may influence the relationship between the treatment variable and the outcome variable.")
                                )
                })
                
                output$data <- reactable::renderReactable({
                    req(data())
                    reactable::reactable(data())
                })
                
            })#close observeEvent

            
            # observe({
            #     req(inherits(mpse, "MPSE"))
            #     
            #     lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
            #     group <- names(lev[lev > 1])
            #     
            #     sample_na <- mpse %>% mp_extract_sample %>% names
            #     treatment_name <- sample_na[unlist(!lapply(mpse %>% mp_extract_sample,class) %in% "numeric")]
            #     
            #     updatePickerInput(session, "covariates",
            #                       choices = group,
            #                       selected = tail(names(lev[lev == 2]), 1)
            #     )
            #     updatePickerInput(session, "treatment",
            #                       choices = treatment_name,
            #                       selected = tail(treatment_name, 1)
            #     )
            #     
            # })
            
            
            psm_matched <- eventReactive(input$btn, {
                req(data())

                #metada <- mpse %>% mp_extract_sample
                #print(data())
                metada <- data()
                treatment <- isolate({input$treatment})
                covariates <- isolate({input$covariates})
                method <- isolate({input$method})
                ratio <- isolate({input$ratio})

                #remove NA
                meta2 <- metada[!is.na(metada[,treatment]),]

                #convert to numeric [0,1]
                # meta2[meta2[,treatment] == "y",treatment] <- 1
                # meta2[meta2[,treatment] == "n",treatment] <- 0
                # print(meta2[,treatment])
                # meta2[,treatment] <- as.numeric(meta2[,treatment])

                #make formula
                formula <- as.formula(paste(treatment,"~", paste(covariates, collapse="+")))

                #Match It
                match.it <- matchit(formula, data = meta2, method= method,ratio = ratio)

                return(match.it)
            })
            
            p_psm <- reactive({
                req(psm_matched())
                match.it <- psm_matched()
                p <- plot(summary(match.it), main = "Evaluate the matching effect")
                return(p)
            })

            output$plot <- renderPlot({
                req(p_psm())
                p_psm()
            })
            
            output$downloadTable <- downloadHandler(
                filename = function(){ "Matched_Data.csv" },
                content = function(file){
                    req(psm_matched())
                    table <- match.data(psm_matched())
                    write.csv(table,
                              file,
                              row.names = FALSE)
                })
        }
    )
}