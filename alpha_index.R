alpha_index_ui <- function(id) {
  ns <- NS(id)
  res <- div(
    #class = "tab-body",
    fluidRow(
      column(3,
             shinydashboardPlus::box(
               width = NULL,
               title = "Alpha-diversity Index",
               status = "warning",
               collapsible = TRUE,
               fluidRow(
                 column(6,
                        pickerInput(ns("index"), "Alpha-diversity Index:",
                                    options = list(`actions-box` = TRUE),
                                    choices = alpha_index, multiple = T,
                                    selected = c("Observe", "ACE", "Chao1")
                        )
                 ),
                 column(6,
                        pickerInput(
                          ns("test"), "Statistical method:",
                          choices = c("t.test", "wilcox.test"),
                          selected = "t.test"
                        )
                 )
               ),
               pickerInput(ns("group"), "Group:", NULL),
               pickerInput(ns("P_group"),
                           "Test group",
                           NULL,
                           multiple = TRUE,
                           options = pickerOptions(noneSelectedText = "Default")
               ),
               
               actionButton(ns("btn"), "Submit")
             ),
             tabBox(width = NULL,
                    tabPanel(h5("Graphics Options"),
                             tags$b("Whether to show:"),
                             prettyCheckbox(
                               inputId = ns("map_signif_level"),
                               label = "Asterisks (***=0.001, **=0.01, *=0.05)",
                               value = FALSE,
                               status = "danger",
                               shape = "curve"
                             ),
                             
                             fluidRow(
                               column(6,
                                      numericInput(ns("textsize"), "P Text size:", 3.88, 0, 50, 0.5)
                               ),
                               column(6,
                                      numericInput(ns("y_position"), "P Text Height:", 0.5, 0, 100, 0.1)
                               )
                             ),
                             fluidRow(
                               column(6,
                                      numericInput(ns("step_increase"), "Step increase:", 0.05, 0, 1, 0.01)
                               ),
                               column(6,
                                      numericInput(ns("tip_length"), "Tip length:", 0.03, 0, 1, 0.01)
                               )
                             )
                             # radioGroupButtons(
                             #     inputId = ns("box_style"),
                             #     label = "Plot type",
                             #     choices = c("box", 
                             #                 "violin", 
                             #                 "boxviolin"),
                             #     checkIcon = list(
                             #         yes = tags$i(class = "fa fa-check-square", 
                             #                      style = "color: steelblue"),
                             #         no = tags$i(class = "fa fa-square-o", 
                             #                     style = "color: steelblue"))
                             # )
                    ),
                    tabPanel(
                      h5("Color"),
                      fluidRow(
                        column(6,
                               uiOutput(ns("color")))
                      )
                    ),
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
                                                  choices = c('PDF' = '.pdf',"PNG" = '.png','TIFF'='.tiff')
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
               img(src="alpha_index_plot.png",
                   align = "center",
                   width = "100%")
             )
      ),
      column(9,
             jqui_resizable(
               plotOutput(ns("alpha_index_plot"), width = '900px', height = '600px'),
               operation = c("enable", "disable", "destroy", "save", "load"),
               options = list(
                 minHeight = 100, maxHeight = 900,
                 minWidth = 300, maxWidth = 1200
               )
             )
      )
    )
  )
  return(res)
}


alpha_index_mod <- function(id, mpse) {
  moduleServer(
    id,
    function(input, output, session) {
      treeda <- reactiveVal({readRDS("data/treeda.rds")})
      observe({
        req(inherits(mpse, "MPSE")) 
        lev <- sapply(mp_extract_sample(mpse)[-1], function(n) length(unique(n)))
        group <- names(lev[lev > 1])
        updatePickerInput(session, "group", choices = group,
                          selected = tail(names(lev[lev == 2]), 1))
        if (!is.null(treeda())) {
          updatePickerInput(session, "index",
                            choices = c(alpha_index, c("PD Whole Tree" = "PD", "Species Richness" = "SR")),
                            selected = c("Observe", "ACE", "Chao1")
          )
        }
        
      })
      
      observe({
        req(!is.null(input$group))
        group_element <- mpse %>% mp_extract_sample %>% select(!!sym(input$group)) %>% unique
        group_element <- group_element[[1]]
        #print(group_element)
        if(is.character(group_element)) {
          x1 <- combn(group_element, 2, simplify = T, FUN = function(x) paste(x, collapse = " | "))
          #x2 <- lapply(strsplit(x1, "\\|"), function(x) trimws(x))
          updatePickerInput(session, "P_group",
                            choices = x1,
                            selected = "Default"
          )
        }
      })
      
      mp_alpha <- eventReactive(input$btn, {
        req(inherits(mpse, "MPSE"))
        input$submit
        mp_alpha <- mp_cal_alpha(mpse, .abundance = RareAbundance, force = T)
        if (!is.null(treeda())) {
          pd_a_index <- pd(
            t(mp_extract_assays(mpse, Abundance)),
            treeda(), ape::is.rooted(treeda())
          )
          pd_a_index <- data.frame(
            Sample = row.names(pd_a_index),
            pd_a_index, row.names = NULL
          )
          mp_alpha <- mp_alpha %>%
            left_join(y = pd_a_index, by = "Sample")
        }
        return(mp_alpha)
      })
      
      p_alpha_index <- reactive({
        req(inherits(mp_alpha(), "MPSE"))
        input$btn
        group <- isolate({input$group})
        index <- isolate({input$index})
        test <- isolate({input$test})
        #y_position <- isolate({input$y_position})
        #step_increase <- isolate({input$step_increase})
        #tip_length <- isolate({input$tip_length})
        P_group <- isolate({input$P_group})
        # print(P_group)
        if(!is.null(P_group)){ #P_group defaults to NULL
          P_group <- lapply(strsplit(P_group, "\\|"), function(x) trimws(x))
        }
        
        # if(is.na(y_position)) {
        #     y_position <- NULL
        # }
        
        
        #print(index)
        #print(input$y_position)
        p <- mp_alpha() %>% 
          mp_plot_alpha(.group=!!sym(group), 
                        .alpha= !!index,
                        map_signif_level = input$map_signif_level, 
                        test = test,
                        step_increase = input$step_increase,
                        textsize = input$textsize,
                        #y_position = y_position,
                        margin_top = input$y_position,
                        tip_length = input$tip_length,
                        comparisons = P_group) +
          scale_fill_brewer(palette="Dark2") +
          scale_color_brewer(palette="Dark2") 
        
        ########################################################
        ##color model
        color_content <- mpse %>% mp_extract_sample %>%
          select(!!sym(group)) %>% unique #It is a tibble
        
        #Partition type 2
        if(color_content[[1]] %>% is.numeric) {#continuous vector don't call color pal.
          return(p)
        } else {
          # color_content <- mpse %>% mp_extract_sample %>%
          #     select(!!sym(group)) %>% unique #It is a tibble
          
          ncolors <- color_content[[1]] %>% length #length of group
          color_input <- lapply(seq(ncolors), function (i){
            input[[paste0("colors",i)]]
          }) %>% unlist #calling input color by length of group
          
          ##########################################################
          
          
          if(length(color_input) != ncolors) {
            p #<- p +
            #scale_color_manual(values = cc(ncolors)) +
            #scale_fill_manual(values = cc(ncolors))
          }else{
            p <- p +
              scale_color_manual(values = color_input) +
              scale_fill_manual(values = color_input)
            
          }#close else 2
        }#close else 1
        return(p)
      })#reactive
      
      #Modify color
      color_list <- reactive({
        req(mp_alpha())
        input$btn
        group <- isolate({input$group})
        ns <- NS(id)
        if(!is.numeric(mp_extract_sample(mpse)[[group]])){
          color_content <- mpse %>% mp_extract_sample %>% 
            select(!!sym(group)) %>% unique #It is a tibble
          name_colors <- color_content[[1]] %>% sort #getting chr.
          pal <- pattle_drak2(length(name_colors)) #calling color palette:"pattle_drak2"
          names(pal) <- name_colors #mapping names to colors 
          
          picks <- lapply(seq(pal), function(i) {#building multiple color pickers
            colorPickr(
              inputId = ns(paste0("colors",i)),
              label = names(pal[i]),
              selected = pal[[i]],
              swatches = cols,
              theme = "monolith",
              useAsButton = TRUE
            )
          })
          return(picks)
        }
      })
      
      output$alpha_index_plot <- renderPlot({
        req(p_alpha_index())
        p_alpha_index()
      })
      
      output$color <- renderUI(
        #req(color_list)
        color_list()
      )
      
      output$downloadPlot <- downloadHandler(
        filename = function(){
          paste("alpha_index_plot", input$extPlot, sep='')},
        content = function(file){
          req(p_alpha_index())
          ggsave(file, 
                 plot = p_alpha_index(), 
                 width = input$width_slider, 
                 height = input$height_slider,
                 dpi = input$dpi)
        })
      
      output$downloadTable <- downloadHandler(
        filename = function(){ "Alpha_Data.csv" },
        content = function(file){
          req(p_alpha_index())
          table <- mp_alpha() %>% mp_extract_sample 
          n <- names(table)[sapply(table, class) == "list"] 
          write.csv(table %>% select(-c(n)), 
                    file,
                    row.names = FALSE)
        })
    }
  )
}