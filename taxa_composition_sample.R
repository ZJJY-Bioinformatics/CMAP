taxa_composition_sample_ui <- function(id) {
  ns <- NS(id)
  res <- div(
    #class = "tab-body",
    fluidRow(
      column(3,
             shinydashboardPlus::box(
               width = NULL,
               collapsible = TRUE,
               title = "Taxonomy composition Analysis",
               status = "warning",
               p("Click below to compute microbial abundance at each level. It's a  time-consuming process, a single calculation should suffice."),
               div(
                   style = "text-align:center; padding-top:10px;",
                   actionBttn(
                       ns("btn"), 
                       "Calculating Abundance", 
                       style = "jelly", 
                       color = "warning"
                   )
               )#close div
             ),
             tabBox(width = NULL,
                    tabPanel(h5("Graphics Options"),
                             fluidRow(
                                 column(6,
                                        pickerInput(ns("level"),
                                                    "Taxonomic level:",
                                                    choices = NULL)
                                 ),
                                 column(6,
                                        pickerInput(ns("feature"), 
                                                    "Sort by:", 
                                                    NULL,
                                                    options = pickerOptions(liveSearch=T)) %>% 
                                            shinyInput_label_embed(
                                                icon("circle-question") %>%
                                                    bs_embed_tooltip(title = "Sort according to the selected species.")
                                            ),
                                 )
                             ),
                             fluidRow(
                                 column(6,
                                        pickerInput(ns("group"),
                                                    "Group",
                                                    NULL,
                                                    options = pickerOptions(noneSelectedText = "NULL"))
                                 ),
                                 column(6,
                                        selectInput(inputId = ns('geom'),
                                                    label = 'Plot style',
                                                    choices = c('bar' = "bar",
                                                                "flowbar" = "flowbar")),
                                 )
                             ),
                             numericInput(ns("topn"), "Top most abundant:", value = 10),
                             tags$b("Whether to show:"), 
                             prettyCheckbox(
                                 inputId = ns("Samples_names"),
                                 label = "Sample names",
                                 value = FALSE,
                                 status = "danger",
                                 shape = "curve"
                             )
                    ),#close tabPanel
                    tabPanel(h5("Download"),
                             fluidRow(
                               column(width = 6,
                                      style=list("padding-right: 5px;"),
                                      numericInput(ns("width_slider"), "Width:", 10,1, 20)
                               ),
                               column(width = 6,
                                      style=list("padding-left: 5px;"),
                                      numericInput(ns("height_slider"), "Height:", 8, 1, 20)
                               )
                             ),
                             fluidRow(
                               column(width = 6,
                                      style=list("padding-right: 5px;"),
                                      selectInput(inputId = ns('extPlot'),
                                                  label = 'Output format',
                                                  choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff',"SVG" = '.svg')
                                      ),
                               ),
                               column(width = 6,
                                      style=list("padding-left: 5px;"),
                                      numericInput(ns("dpi"), "DPI:", 300, 100, 600)
                               )
                             ),
                             fluidRow(
                               column(width = 6,
                                      downloadButton(ns("downloadPlot"), "Download Plot")),
                               column(width = 6,
                                      downloadButton(ns("downloadTable"), "Download Table"))
                             )
                    )
             ),
             shinydashboard::box(
               title = "Example Plot",
               width = NULL,
               img(src="taxa_composition_samples.png",
                   align = "center",
                   width = "100%")
             )
      ),
      column(9,
             plotOutput(ns("message"),height = "30px"),
             jqui_resizable(
                 plotOutput(ns("taxa_composition_plot"), width = '900px', height = '600px'),
                 operation = c("enable", "disable", "destroy", "save", "load"),
                 options = list(
                     minHeight = 300, maxHeight = 900,
                     minWidth = 300, maxWidth = 1200
                 )
             )#close jqui_resizable
      )#close column
    )
  )
  return(res)
}


taxa_composition_sample_mod <- function(id, mpse) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        req(inherits(mpse, "MPSE")) 
          lev <- sapply(mp_extract_sample(mpse), function(n) length(unique(n)))
          group <- names(lev[lev > 1])
          group <- c("NULL",group) 
          updatePickerInput(session, "group", choices = group,
                            selected = "NULL")
        taxa <- names(mp_extract_taxonomy(mpse))[-1]
        updatePickerInput(session, "level", choices = taxa,
                          selected = taxa[3])
      })
      
      observe({
          req(inherits(mpse, "MPSE"), input$level)
          updatePickerInput(session, "feature",
                            choices = unique(mp_extract_taxonomy(mpse)[[input$level]])
          )
      })
      
      mp_composition_sample <- eventReactive(input$btn, {
          req(inherits(mpse, "MPSE"))
          
          mp_composition_sample <- mpse %>%
              mp_cal_abundance(.abundance = RareAbundance, action = "add")
          
          return(mp_composition_sample)
      })
      
      p_taxa_composition <- reactive({
        req(mp_composition_sample())
        # level <- isolate({input$level})
        # topn <- isolate({input$topn})
        # feature <- isolate({input$feature}) 
        # group <- isolate({input$group}) 

        #plot
        if(input$group == "NULL") {
            p <- mp_composition_sample() %>%
                mp_plot_abundance(.abundance=RareAbundance, 
                                  .group= NULL, 
                                  taxa.class = !!sym(input$level),
                                  topn = input$topn,
                                  force = TRUE,
                                  geom = input$geom) +
                theme(
                    axis.text.y = element_text(size = 16),
                    text = element_text(size = 18),
                    legend.text = element_text(size = 16)
                ) 
        }else{
            p <- mp_composition_sample() %>%
                mp_plot_abundance(.abundance=RareAbundance, 
                                  .group= !!sym(input$group), 
                                  taxa.class = !!sym(input$level),
                                  topn = input$topn,
                                  force = TRUE,
                                  geom = input$geom) +
                theme(
                    axis.text.y = element_text(size = 16),
                    text = element_text(size = 18),
                    legend.text = element_text(size = 16)
                ) 
        }

        
        if(!input$Samples_names){
         p <-  p + theme(axis.title.x = element_blank(), 
                     axis.text.x = element_blank(), 
                     axis.ticks.x = element_blank())
        }

        p_resort <- resort_p(p, input$feature)
        return(p_resort) 
      })
      
      output$message <- renderPlot({
          req(p_taxa_composition())
          feature <- isolate({input$feature}) 
          data <- p_taxa_composition()$data
          tax_names <- data[[1]] %>% levels
          validate(
              need(feature %in% tax_names, "The selected taxonomy belongs to the 'Others' category and cannot be sorted."
              )#close need
          )#close validate
      })
      
      output$taxa_composition_plot <- renderPlot({
        req(p_taxa_composition())
        p_taxa_composition()
      })
      
      output$downloadPlot <- downloadHandler(
        filename = function(){
          paste("taxa_composition_Sample", input$extPlot, sep='')},
        content = function(file){
          req(p_taxa_composition())
          ggsave(file, 
                 plot = p_taxa_composition(), 
                 width = input$width_slider, 
                 height = input$height_slider,
                 dpi = input$dpi)
        })
      
      output$downloadTable <- downloadHandler(
        filename = function(){ "taxa_composition_Data.csv" },
        content = function(file){
          req(p_taxa_composition())
          table <- p_taxa_composition()$data 
          n <- names(table)[sapply(table, class) == "list"] 
          write.csv(table %>% select(-c(n)), 
                    file,
                    row.names = FALSE)
        })
    }
  )
}