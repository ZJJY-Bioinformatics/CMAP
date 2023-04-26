maaslin_ui <- function(id) {
    ns <- NS(id)
    res <- div(
        class = "maaslin-body",
        fluidRow(
            column(5,
                   shinydashboardPlus::box(
                       width = 12, title = "Maaslin2 Analysis",
                       status = "warning",
                       collapsible = TRUE,
                       # column(
                       #     width = 5, class = "maaslin2",
                       fluidRow(
                           column(width = 6,
                                  style=list("padding-right: 5px;"),
                                  numericInput(
                                      ns("min_abundance"),
                                      "Min abundance:",
                                      min = 0,
                                      max = 1,
                                      value = 0
                                  )
                           ),
                           column(width = 6,
                                  style=list("padding-left: 5px;"),
                                  numericInput(
                                      ns("min_prevalence"),
                                      "Min prevalence:",
                                      value = 0.1
                                  )
                           )
                       ),
                       
                       fluidRow(
                           column(width = 6,
                                  style=list("padding-right: 5px;"),
                                  numericInput(
                                      ns("min_variance"),
                                      "Min variance:",
                                      value = 0.0
                                  )
                           ),
                           column(width = 6,
                                  style=list("padding-left: 5px;"),
                                  numericInput(
                                      ns("max_significance"),
                                      "Q-value cutoff:",
                                      value = 0.25
                                  )
                           )
                       ),
                       
                       fluidRow(
                           column(width = 6,
                                  style=list("padding-right: 5px;"),
                                  numericInput(
                                      ns("topn"),
                                      "Top features",
                                      min = 1,
                                      value = 50,
                                      step = 1
                                  )
                           ),
                           column(width = 6,
                                  style=list("padding-left: 5px;"),
                                  pickerInput(
                                      ns("level"),
                                      "Taxonomic level:",
                                      choices = NULL
                                  )
                           )
                       ),
                       fluidRow(
                           column(width = 6,
                                  style=list("padding-right: 5px;"),
                                  pickerInput(
                                      ns("method"),
                                      "Analysis method:",
                                      c("LM", "CPLM", "NEGBIN", "ZINB")
                                  )
                           ),
                           column(width = 6,
                                  style=list("padding-left: 5px;"),
                                  pickerInput(
                                      ns("standardize"),
                                      "Standardize(or not):",
                                      choices = c("TRUE", "FALSE")
                                  )
                           )
                       ),
                       fluidRow(
                           column(width = 6,
                                  style=list("padding-right: 5px;"),
                                  pickerInput(
                                      ns("normalization"),
                                      label = "Normalization method:",
                                      choices = c("TSS", "CLR", "CSS", "NONE", "TMM"),
                                      selected = "NONE"
                                  )
                           ),
                           column(width = 6,
                                  style=list("padding-left: 5px;"),
                                  pickerInput(
                                      ns("transform"),
                                      label = "Transform method:",
                                      choices = c("LOG", "LOGIT", "AST", "NONE"),
                                      selected = "AST"
                                  )
                           )
                       ),
                       actionButton(ns("btn"), "Submit"),
                       tri_select_input_ui("maaslin", NULL)

                   )
                   ),
            column(7,
                   jqui_resizable(
                       plotOutput(ns("plot"), width = "800px"),
                       operation = c("enable", "disable", "destroy", "save", "load"),
                       options = list(
                           minHeight = 100, maxHeight = 900,
                           minWidth = 300, maxWidth = 1200
                       )
                   )
                   #tri_select_input_ui("maaslin", NULL)
            )
        )
        #tri_select_input_ui("maaslin", NULL)

        #)


        # shinydashboardPlus::box(
        #     width = 12, title = "Maaslin2 Analysis",
        #     status = "warning",
        #     collapsible = TRUE,
        #     # column(
        #     #     width = 5, class = "maaslin2",
        #         fluidRow(
        #             column(width = 6,
        #                    style=list("padding-right: 5px;"),
        #                    numericInput(
        #                        ns("min_abundance"),
        #                        "Min abundance:",
        #                        min = 0,
        #                        max = 1,
        #                        value = 0
        #                    )
        #             ),
        #             column(width = 6,
        #                    style=list("padding-left: 5px;"),
        #                    numericInput(
        #                        ns("min_prevalence"),
        #                        "Min prevalence:",
        #                        value = 0.1
        #                    )
        #             )
        #         ),
        #         
        #         fluidRow(
        #             column(width = 6,
        #                    style=list("padding-right: 5px;"),
        #                    numericInput(
        #                        ns("min_variance"),
        #                        "Min variance:",
        #                        value = 0.0
        #                    )
        #             ),
        #             column(width = 6,
        #                    style=list("padding-left: 5px;"),
        #                    numericInput(
        #                        ns("max_significance"),
        #                        "Q-value cutoff:",
        #                        value = 0.25
        #                    )
        #             )
        #         ),
        #         
        #         fluidRow(
        #             column(width = 6,
        #                    style=list("padding-right: 5px;"),
        #                    numericInput(
        #                        ns("topn"),
        #                        "Top features",
        #                        min = 1,
        #                        value = 50,
        #                        step = 1
        #                    )
        #             ),
        #             column(width = 6,
        #                    style=list("padding-left: 5px;"),
        #                    pickerInput(
        #                        ns("level"),
        #                        "Taxonomic level:",
        #                        choices = NULL
        #                    )
        #             )
        #         ),
        #         fluidRow(
        #             column(width = 6,
        #                    style=list("padding-right: 5px;"),
        #                    pickerInput(
        #                        ns("method"),
        #                        "Analysis method:",
        #                        c("LM", "CPLM", "NEGBIN", "ZINB")
        #                    )
        #             ),
        #             column(width = 6,
        #                    style=list("padding-left: 5px;"),
        #                    pickerInput(
        #                        ns("standardize"),
        #                        "Standardize(or not):",
        #                        choices = c("TRUE", "FALSE")
        #                    )
        #             )
        #         ),
        #         fluidRow(
        #             column(width = 6,
        #                    style=list("padding-right: 5px;"),
        #                    pickerInput(
        #                        ns("normalization"),
        #                        label = "Normalization method:",
        #                        choices = c("TSS", "CLR", "CSS", "NONE", "TMM"),
        #                        selected = "NONE"
        #                    )
        #             ),
        #             column(width = 6,
        #                    style=list("padding-left: 5px;"),
        #                    pickerInput(
        #                        ns("transform"),
        #                        label = "Transform method:",
        #                        choices = c("LOG", "LOGIT", "AST", "NONE"),
        #                        selected = "AST"
        #                    )
        #             )
        #         )
        #     # numericInput(
        #     #     ns("min_abundance"),
        #     #     "Min abundance:",
        #     #     min = 0,
        #     #     max = 1,
        #     #     value = 0
        #     # ),
        #     # numericInput(
        #     #     ns("min_prevalence"),
        #     #     "Min prevalence:",
        #     #     value = 0.1
        #     # ),
        #     # numericInput(
        #     #     ns("min_variance"),
        #     #     "Min variance:",
        #     #     value = 0.0
        #     # ),
        #     # numericInput(
        #     #     ns("max_significance"),
        #     #     "Q-value cutoff:",
        #     #     value = 0.25
        #     # ),
        #     # numericInput(
        #     #     ns("topn"),
        #     #     "Top features",
        #     #     min = 1,
        #     #     value = 50,
        #     #     step = 1
        #     # ),
        #     # pickerInput(
        #     #     ns("level"),
        #     #     "Taxonomic level:",
        #     #     choices = NULL
        #     # ),
        #     # pickerInput(
        #     #     ns("method"),
        #     #     "Analysis method:",
        #     #     c("LM", "CPLM", "NEGBIN", "ZINB")
        #     # ),
        #     # pickerInput(
        #     #     ns("standardize"),
        #     #     "Standardize(or not):",
        #     #     choices = c("TRUE", "FALSE")
        #     # ),
        #     # pickerInput(
        #     #     ns("normalization"),
        #     #     label = "Normalization method:",
        #     #     choices = c("TSS", "CLR", "CSS", "NONE", "TMM"),
        #     #     selected = "NONE"
        #     # ),
        #     # pickerInput(
        #     #     ns("transform"),
        #     #     label = "Transform method:",
        #     #     choices = c("LOG", "LOGIT", "AST", "NONE"),
        #     #     selected = "AST"
        #     # )
        #     #),
        #     # column(
        #     #     width = 7,
        #         div(
        #             style = "margin-top: 25px",
        #             tri_select_input_ui("maaslin", NULL)
        #         )
        #     #),
        #     fluidRow(),
        #     actionButton(ns("btn"), "Submit")
        # ),
        # shinydashboardPlus::box(
        #     width = 12,
        #     title = "Plot Download",
        #     status = "success",
        #     solidHeader = FALSE,
        #     collapsible = TRUE,
        #     plotOutput(ns("plot")),
        #     numericInput(ns("width_slider"), "width:", 10,1, 20),
        #     numericInput(ns("height_slider"), "height:", 8, 1, 20),
        #     radioButtons(inputId = ns('extPlot'),
        #                  label = 'Output format',
        #                  choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff'),
        #                  inline = TRUE),
        #     downloadButton(ns("downloadPlot"), "Download Plot"),
        #     downloadButton(ns("downloadTable"), "Download Table")
        # ),
        # fluidRow(),
        # jqui_resizable(
        #     plotOutput(ns("plot"), width = "800px"),
        #     operation = c("enable", "disable", "destroy", "save", "load"),
        #     options = list(
        #         minHeight = 100, maxHeight = 900,
        #         minWidth = 300, maxWidth = 1200
        #     )
        # ),
        #DTOutput(ns("tbl"), width = "500px", height = "400px")
    )
}


maaslin_mod <- function(id, mpse) {
    moduleServer(
        id,
        function(input, output, session) {
          effects <- reactiveValues(
            total = NULL,
            fixed = NULL,
            random = NULL
          )
      observe({
        req(inherits(mpse, "MPSE"))
        lev <- sapply(mp_extract_sample(mpse)[, -1], function(n) length(unique(n)))
        group <- names(lev[lev > 1])
        taxa <- names(mp_extract_taxonomy(mpse))[-1]
        effects$total <- group
        updatePickerInput(session, "level",
          choices = taxa,
          selected = taxa[3]
        )
        updateSelectInput(session, "total_effects", choices = group)
      })
      observeEvent(input$total_to_fixed, {
        effects$fixed <- c(effects$fixed, input$total_effects)
        effects$total <- effects$total[!effects$total %in% input$total_effects]
        updateSelectInput(session, "fixed_effects", choices = effects$fixed)
        updateSelectInput(session, "total_effects", choices = effects$total)
      })
      observeEvent(input$fixed_to_total, {
        effects$total <- c(effects$total, input$fixed_effects)
        effects$fixed <- effects$fixed[!effects$fixed %in% input$fixed_effects]
        updateSelectInput(session, "fixed_effects", choices = effects$fixed)
        updateSelectInput(session, "total_effects", choices = effects$total)
      })
      observeEvent(input$total_to_random, {
        effects$random <- c(effects$random, input$total_effects)
        effects$total <- effects$total[!effects$total %in% input$total_effects]
        updateSelectInput(session, "random_effects", choices = effects$random)
        updateSelectInput(session, "total_effects", choices = effects$total)
      })
      observeEvent(input$random_to_total, {
        effects$total <- c(effects$total, input$random_effects)
        effects$random <- effects$random[!effects$random %in% input$random_effects]
        updateSelectInput(session, "random_effects", choices = effects$random)
        updateSelectInput(session, "total_effects", choices = effects$total)
      })
      
      observeEvent(input$fixed_to_random, {
        effects$random <- c(effects$random, input$fixed_effects)
        effects$fixed <- effects$fixed[!effects$fixed %in% input$fixed_effects]
        updateSelectInput(session, "fixed_effects", choices = effects$fixed)
        updateSelectInput(session, "random_effects", choices = effects$random)
      })
      observeEvent(input$random_to_fixed, {
        effects$fixed <- c(effects$fixed, input$random_effects)
        effects$random<- effects$random[!effects$random %in% input$random_effects]
        updateSelectInput(session, "fixed_effects", choices = effects$fixed)
        updateSelectInput(session, "random_effects", choices = effects$random)
      })
        res <- eventReactive(input$btn, {
            req(inherits(mpse, "MPSE"))
            input$btn
            level <- isolate({
              input$level
            })
            min_abundance <- isolate({
              input$min_abundance
            })
            min_prevalence <- isolate({
              input$min_prevalence
            })
            min_prevalence <- isolate({
              input$min_prevalence
            })
            min_variance <- isolate({
              input$min_variance
            })
            topn <- isolate({
              input$topn
            })
            max_significance <- isolate({
              input$max_significance
            })
            transform <- isolate({input$transform})
            standardize <- isolate({as.logical(input$standardize)})
            normalization <- isolate({
              input$normalization
            })
            method <- isolate({
              input$method
            })
            fixed_effects <- isolate({
              effects$fixed
            })
            random_effects <- isolate({
              effects$random
            })
            rdfn <- randomFile()
            abuda <- maaslin2_input_abuda(mpse, level)
            metada <- mp_extract_sample(mpse)
            print(rdfn)
            reference <- lapply(effects$fixed, function(n) {
              x <- metada[[n]]
              if (is.character(x) && length(unique(x)) > 2) {
                return(paste(n, x[1], sep = ","))
              }
            })
            reference <- paste0(unlist(reference), collapse = ";")
      
            fit_data <- Maaslin2(
              input_data = abuda,
              input_metadata = metada,
              output = rdfn,
              min_abundance = min_abundance,
              min_prevalence = min_prevalence,
              min_variance = min_variance,
              max_significance = max_significance,
              heatmap_first_n = topn,
              standardize = standardize,
              normalization = normalization,
              transform = transform,
              analysis_method = method,
              # fixed_effects = c("age", "BMI", "gender"),
              fixed_effects = effects$fixed,
              random_effects = effects$random,
              reference = reference,
              plot_heatmap = F,
              plot_scatter = F
            )
            sign_res <- paste(rdfn, "significant_results.tsv", sep = "/")
            p <- Maaslin2:::maaslin2_heatmap(sign_res, title = NULL)
            return(list(plot = p, tbl = fit_data$results))
          })
          output$plot <- renderPlot(res = 200, {
            req(inherits(res(), "list"))
              res()$plot
          })
          
          output$downloadPlot <- downloadHandler(
              filename = function(){
                  paste("plot", input$extPlot, sep='')},
              content = function(file){
                  req(inherits(res(), "list"))
                  ggsave(file, 
                         plot = res()$plot, 
                         width = input$width_slider, 
                         height = input$height_slider,
                         dpi = 300)
              })
          
          # output$downloadTable <- downloadHandler(
          #     filename = function(){ "MP_Data.csv" },
          #     content = function(file){
          #         req(p_PCA())
          #         table <- mp_pcoa() %>% mp_extract_sample 
          #         n <- names(table)[sapply(table, class) == "list"] 
          #         write.csv(table %>% select(-c(n)), 
          #                   file,
          #                   row.names = FALSE)
          #     })
          
          
          output$tbl<- renderDT({
            req(inherits(res(), "list"))
              res()$tbl
          })
        }
    )
}

maaslin2_input_abuda <- function(mpse, taxa.level) {
  abuda <- mpse %>%
    mp_cal_abundance(Abundance, force = TRUE) %>%
    mp_extract_abundance(taxa.class = !!sym(taxa.level)) %>%
    tidyr::unnest(AbundanceBySample) %>%
    select(Sample, label, RelAbundanceBySample) %>%
    tidyr::pivot_wider(id_cols = Sample, names_from = label, values_from = RelAbundanceBySample)
    abuda[, -1] <- abuda[, -1] / 100
  return(abuda)
}


randomFile <- function(n = 1) {
    a <- do.call(paste0, replicate(5, sample(c(LETTERS, letters), n, TRUE), FALSE))
    filename <- paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
    paste(tempdir(), filename, sep = "/")
}


total_effects_box <- function(id, choices) {
  ns <- NS(id)
  res <- div(
    style = "float: left;",
    # style = "display: inline-block;",
    div(
      class = "sel-header",
      style = "text-align: center; width: 200px; height: 30px; background: #3c8dbc;",
      tags$b("Groups:", style = "dispaly: block; line-height: 30px")
    ),
    div(
      tags$select(
        id = ns("total_effects"),
        multiple = "multiple",
        style = "width: 200px; height: 400px",
        lapply(choices, function(i) {
          tags$option(i, value = i)
        })
      )
    )
  )
  return(res)
}

trans_btn_box <- function(id) {
  ns <- NS(id)
  res <-
    div(
      style = "float: left; text-align: center; width: 80px; height: 430px;",
      
      actionButton(ns("total_to_fixed"), NULL, icon("arrow-right"),
                   style = "width: 60px; height: 30px; margin-top: 60px"),
      actionButton(ns("fixed_to_total"), NULL, icon("arrow-left"),
                   style = "width: 60px; height: 30px; margin-top: 3px"),
      actionButton(ns("total_to_random"), NULL, icon("arrow-right"),
                   style = "width: 60px; height: 30px; margin-top: 180px"),
      actionButton(ns("random_to_total"), NULL, icon("arrow-left"),
                   style = "width: 60px; height: 30px; margin-top: 3px")
    )
  return(res)
}


sel_effects_box <- function(id) {
  ns <- NS(id)
  res <- div(
    style = "float: left;",
    # style = "display: inline-block;",
    div(
      class = "sel-header",
      style = "text-align: center; width: 200px; height: 30px; background: #00a65a;",
      tags$b("Fixed effects", style = "dispaly: block; line-height: 30px")
    ),
    div(
      tags$select(
        id = ns("fixed_effects"),
        multiple = "multiple",
        style = "width: 200px; height: 150px;"
      )
    ),
    div(
      style = "text-align: center; width: 200px; height: 69px;",
      actionButton(ns("fixed_to_random"), NULL, icon("arrow-down"), style = "width: 30px; height: 50px; margin-top: 10px"),
      actionButton(ns("random_to_fixed"), NULL, icon("arrow-up"), style = "width: 30px; height: 50px; margin-top: 10px; text-align: center;")
      
    ),
    div(
      style = "text-align: center; width: 200px; height: 30px; background: #f39c12;",
      tags$b("Random effects", style = "dispaly: block; line-height: 30px")
    ),
    div(
      tags$select(
        id = ns("random_effects"),
        multiple = "multiple",
        style = "width: 200px; height: 150px"
      )
    )
  )
  return(res)
}


tri_select_input_ui <- function(id, choices) {
  div(
    total_effects_box(id, choices),
    trans_btn_box(id),
    sel_effects_box(id)
  )
}

tri_select_input_mod <- function(id, choices) {
  moduleServer(
    id,
    function(input, output, session) {
      effects <- reactiveValues(
        total = choices,
        fixed = NULL,
        random = NULL
      )
      observe({
        updateSelectInput(session, "total_effects", choices = choices)
      })
      observeEvent(input$total_to_fixed, {
        effects$fixed <- c(effects$fixed, input$total_effects)
        effects$total <- effects$total[!effects$total %in% input$total_effects]
        updateSelectInput(session, "fixed_effects", choices = effects$fixed)
        updateSelectInput(session, "total_effects", choices = effects$total)
      })
      observeEvent(input$fixed_to_total, {
        effects$total <- c(effects$total, input$fixed_effects)
        effects$fixed <- effects$fixed[!effects$fixed %in% input$fixed_effects]
        updateSelectInput(session, "fixed_effects", choices = effects$fixed)
        updateSelectInput(session, "total_effects", choices = effects$total)
      })
      observeEvent(input$total_to_random, {
        effects$random <- c(effects$random, input$total_effects)
        effects$total <- effects$total[!effects$total %in% input$total_effects]
        updateSelectInput(session, "random_effects", choices = effects$random)
        updateSelectInput(session, "total_effects", choices = effects$total)
      })
      observeEvent(input$random_to_total, {
        effects$total <- c(effects$total, input$random_effects)
        effects$random <- effects$random[!effects$random %in% input$random_effects]
        updateSelectInput(session, "random_effects", choices = effects$random)
        updateSelectInput(session, "total_effects", choices = effects$total)
      })
      
      observeEvent(input$fixed_to_random, {
        effects$random <- c(effects$random, input$fixed_effects)
        effects$fixed <- effects$fixed[!effects$fixed %in% input$fixed_effects]
        updateSelectInput(session, "fixed_effects", choices = effects$fixed)
        updateSelectInput(session, "random_effects", choices = effects$random)
      })
      observeEvent(input$random_to_fixed, {
        effects$fixed <- c(effects$fixed, input$random_effects)
        effects$random<- effects$random[!effects$random %in% input$random_effects]
        updateSelectInput(session, "fixed_effects", choices = effects$fixed)
        updateSelectInput(session, "random_effects", choices = effects$random)
      })
      return(effects)
    }
  )
}




