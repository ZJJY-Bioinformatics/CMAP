source("global.R")
source("upload-data.R")
source("filter-data.R")
source("alpha_index.R")
source("alpha_rare_curve.R")
source("beta-analysis.R")
source("beta_PCoA.R")
source("beta_nmds.R")
source("beta_anosim.R")
source("beta_hcluster.R")
source("taxa-composition-analysis.R")
source("taxa-diff-analysis.R")
source("taxa_diff_tree.R")
source("maaslin.R")
source("analysis-overview.R")
source("data_rarefying.R")
source("cca.R")
source("rda.R")
source("CST.R")
source("tsne.R")
source("diff_barplot.R")
source("alpha_mul_index.R")
source("adonis.R")
#mateG buglist files----------------------
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





# Dashboard Header-------------------------------------------------------
# header <- dashboardHeader(title = "Microbiome Analysis Platform",
#                           titleWidth = 450,
#                           fixed = FALSE)

# Dashboard Sidebar -------------------------------------------------------
sidebar <- dashboardSidebar(
    width = 300,
    sidebarMenu(
        
        #div(tags$img(src = "logo1.jpg", width = "100px", height = "100px")),
        HTML(paste0(
            "<br>",
            "<a href='https://github.com/ZJJY-Bioinformatics' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logo1.png' width = '186'></a>",
            "<br>",
            "<a href='https://github.com/ZJJY-Bioinformatics' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logo2.png' width = '190'></a>",
           
            "<br>"
        )),
        id = "tab",
        menuItem(
            text = "Home",
            tabName = "home",
            icon = icon("fas fa-house")
        ),
        menuItem(
            text = "Data Processing",
            icon = icon("fas fa-file"),
            menuSubItem("Upload Data", tabName = "upload",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Rarefaction", tabName = "rarefy",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Meta Data Filter", tabName = "filter",icon = icon("angle-double-right", verify_fa = FALSE))
        ),
        menuItem(
            "Alpha(α) Diversity",
            icon = icon("a"),
            menuSubItem("Alpha index", tabName = "a_index",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Multi-group Comparison", tabName = "a_mul_index",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Rarefaction Curve", tabName = "rare_curve",icon = icon("angle-double-right", verify_fa = FALSE))
        ),
        menuItem(
            "Beta(β) Diversity",
            icon = icon("b"),
            menuSubItem("PCA", tabName = "pca",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("PCoA", tabName = "pcoa",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("NMDS", tabName = "nmds",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Adonis", tabName = "adonis",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Anosim", tabName = "anosim",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Hierarchical Clustering", tabName = "hcluster",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("t-SNE", tabName = "tsne",icon = icon("angle-double-right", verify_fa = FALSE))
        ),
        menuItem(
            "Microbiomes Composition",
            icon = icon("bars-staggered"),
            menuSubItem("Compositions", tabName = "taxa_diff",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Abundance Comparision", tabName = "feature_diff",icon = icon("angle-double-right", verify_fa = FALSE))
            # menuSubItem("Venn/Upset图", tabName = "taxa_diff_venn"),
            # menuSubItem("Heatmap图", tabName = "taxa_diff_heatmap")
        ),
        menuItem(
            "Biomarker Discovery",
            icon = icon("star"),
            # menuSubItem("ANCOM分析", tabName = "ancom"),
            menuSubItem("LEfSe", tabName = "lefse",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Diff-bar plot", tabName = "diff_bar",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Diff-Abundance plot", tabName = "diff_clade",icon = icon("angle-double-right", verify_fa = FALSE))
        ),
        menuItem(
            "Multivariable Association",
            icon = icon("layer-group"),
            menuSubItem("CCA", tabName = "cca",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("RDA", tabName = "rda",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Maaslin2", tabName = "maaslin",icon = icon("angle-double-right", verify_fa = FALSE))

        ),
        menuItem("Vaginal CSTs", tabName = "cst", icon = icon("person-dress")),
        menuItem("Releases", tabName = "releases", icon = icon("info"))
    )
)

# Dashboard Body ----------------------------------------------------------
body <- dashboardBody(
    use_theme(mytheme), # <-- use the theme
    
    tabItems(
        #tabItem("overview",  analysis_overview_ui()),
        tabItem("home", includeMarkdown("www/home.md")),
        tabItem("a_index", alpha_index_ui("a_index")),
        tabItem("rare_curve", rare_curve_ui("rare")),
        tabItem("filter", filter_ui("filter")),
        tabItem("rarefy", data_rarefy_ui("rarefy")),
        tabItem("upload", upload_data_ui("upload")),
        tabItem("pca", beta_pca_ui("pca")),
        tabItem("pcoa", beta_pcoa_ui("pcoa")),
        tabItem("nmds", beta_nmds_ui("nmds")),
        tabItem("cca", cca_ui("cca")),
        tabItem("rda", rda_ui("rda")),
        tabItem("hcluster", beta_hcluster_ui("hcluster")),
        #tabItem("adonis", beta_adonis_ui("adonis")),
        tabItem("anosim", beta_anosim_ui("anosim")),
        tabItem("taxa_diff", taxa_composition_ui("taxa_diff")),
        tabItem("feature_diff", feature_composition_ui("feature_diff")),
        tabItem("lefse", lefse_ui("lefse")),
        tabItem("diff_clade", diff_clade_ui("diff_clade")),
        tabItem("maaslin", maaslin_ui("maaslin")),
        tabItem("cst", cst_ui("cst")),
        tabItem(tabName = "releases", includeMarkdown("www/NEWS.md")),
        tabItem(tabName = "tsne", tSNE_ui("tsne")),
        tabItem("diff_bar", diff_barplot_ui("diff_bar")),
        tabItem("a_mul_index", alpha_mul_index_ui("a_mul_index")),
        tabItem("adonis", adonis_all_ui("adonis"))
    )
)

page_16S <-  shinydashboardPlus::dashboardPage(
    skin = "blue-light",
    scrollToTop = TRUE,
    #title = "CMAP",
    header = header,
    sidebar = sidebar,
    body = body
)




#——————————————————————————————————————————————————————————————————#    
ui <- tagList(
    # useShinyjs(),
    #includeCSS("www/CMAP.css"),
    add_busy_spinner(spin = "fulfilling-bouncing-circle",
                     color = '#2980b9',
                     position = "top-right", 
                     margins = c(450,950)),
    
    navbarPage("Microbiome Analysis Platform",
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
               tabPanel(title = "16S rRNA", 
                        page_16S,
                        icon = icon("bacterium")),
               navbarMenu("Metagenomics",
                          tabPanel("Taxonomy Profiler", page_metagenomics),
                          tabPanel("Genome Profiler"),
                          icon = icon("dna")
               ),
               tabPanel(title = "About")
               
               
    )
)

shinyUI(ui)
