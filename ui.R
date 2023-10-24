source("ui_16S.R")
# source("global.R")
# source("upload-data.R")
# source("filter-data.R")
# source("alpha_index.R")
# source("alpha_rare_curve.R")
# source("beta-analysis.R")
# source("beta_PCoA.R")
# source("beta_nmds.R")
# source("beta_anosim.R")
# source("beta_hcluster.R")
# source("taxa-composition-analysis.R")
# source("taxa-diff-analysis.R")
# source("taxa_diff_tree.R")
# source("maaslin.R")
# source("analysis-overview.R")
# source("data_rarefying.R")
# source("cca.R")
# source("rda.R")
# source("CST.R")
# source("tsne.R")
# source("diff_barplot.R")
# source("alpha_mul_index.R")
# source("adonis.R")
# source("psm.R")
# source("taxa_composition_sample.R")
# source("extraction_abu.R") #Extraction Tax Abu
# source("BiotypeR.R") #Enterotypes
# source("enterotypes.R") #Enterotypes
#######################################mateG buglist files######################
source("buglist_tSNE.R")
source("buglist_beta_hcluster.R")
source("buglist_adonis_all.R")
source("buglist_beta_anosim.R")
source("buglist_beta_nmds.R")
source("buglist_beta_pcoa.R")
source("buglist_beta_pca.R")
source("buglist_alphaIndex.R")
source("buglist_alpha_mul_index.R")
source("upload_buglist.R")
source("ui_METAG.R")
#######################################mateG buglist files######################
source("ui_epide.R")



# Dashboard Header-------------------------------------------------------
# header <- dashboardHeader(title = "Microbiome Analysis Platform",
#                           titleWidth = 450,
#                           fixed = FALSE)

 
ui <- tagList(
    useShinyjs(),
    #includeCSS("www/spinner_style.css"),
    
    add_busy_spinner(spin = "fading-circle",
                     color = '#2980b9',
                     position = "bottom-right",
                     onstart = FALSE,
                     margins = c(70, 120)
                     #position = "top-right", 
                     #margins = c(450,950)
                     ),
    
    navbarPage(title = "CALM-based Microbiome Analysis Platform",
               header = tags$head(
                   # sourcing css style sheet 
                   includeCSS("www/styles.css"),
                   
                   # include scotpho icon in web browser
                   HTML("<html lang='en'>")
                   # tags$link(rel = "shortcut icon", 
                   #           href = "favicon_scotpho.ico"), 
                   # 
                   # # include google analytics scripts
                   # includeScript("google-analytics.js"), # GA 3 
                   # HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=G-KE1C59RLNS"></script>'),
                   # includeScript("gtag.js") # Google analytics 4 
                   
               ),
               
               
               tags$style(HTML("
               .navbar .navbar-header {float: left;}
               .navbar .navbar-nav {float: right;}
               .navbar-default .navbar-brand {color:white;} 
               .navbar-default .navbar-brand:hover {color:white;}
               .navbar { background-color:#7697c9;}
               .navbar-default .navbar-nav > li > a {color:white;}
               .navbar-default .navbar-nav > .active > a,
               .navbar-default .navbar-nav > .active > a:focus,
               .navbar-default .navbar-nav > .active > a:hover {color:white;background-color:#5171af;}
               .navbar-default .navbar-nav > li > a:hover {color:white;background-color:#5171af;text-decoration}
               .dropdown-menu > .active > a:hover {background-color:#7697c9;}
               .navbar {margin-bottom: 0;}
                  ")),
               windowTitle = "CMAP-Microbiome Analysis Platform",
               collapsible = TRUE,
               tabPanel(
                   div(
                       div(class="fa fa-home", role = "navigation"), "Home"), # wrap in div for screenreader / accessibility purposes 
                   value = "home", # tab ID
                   #use_cicerone(), # guided tour
                   htmlTemplate("landing-page.html" # html file containing landing page contents
                                # variables defined in landing-page.html file to be built in Rshiny.
                                #latest_updates_button = actionButton('btn_indicator_updates', "View recent updates", class = "button") #,
                                #see_more_button = actionButton("jump_to_life_exp", "View life expectancy by SIMD", class = "button"),
                                #guide_button = actionButton("guide", "Need help? Take a guided tour", class = "hero-button")
                   )),
               tabPanel(title = "16S rRNA", 
                        page_16S,
                        icon = icon("bacterium")),
               tabPanel(title = "Epidemiology",
                        page_epide,
                        icon = icon("shield-virus")),
               navbarMenu("Metagenomics",
                          tabPanel("Taxonomy Profiler", page_metagenomics),
                          tabPanel("Genome Profiler"),
                          icon = icon("dna")
               ),
               tabPanel(title = "About")
               
               
    )
)

shinyUI(ui)
