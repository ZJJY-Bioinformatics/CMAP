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
source("taxa_composition_sample.R")
source("extraction_abu.R") #Extraction Tax Abu
source("BiotypeR.R") #Enterotypes
source("enterotypes.R") #Enterotypes
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
            text = "Tutorial",
            tabName = "home",
            icon = icon("fas fa-house")
        ),
        menuItem(
            text = "Data Processing",
            icon = icon("fas fa-file"),
            menuSubItem("Upload Data (EssentialStep1)", tabName = "upload",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Rarefaction (EssentialStep2)", tabName = "rarefy",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Meta Data Filter (EssentialStep3)", tabName = "filter",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("PSM (Skippable)", tabName = "psm",icon = icon("angle-double-right", verify_fa = FALSE))
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
            menuSubItem("Compositions-Group", tabName = "taxa_diff",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Compositions-Samples", tabName = "taxa_composition_sample",icon = icon("angle-double-right", verify_fa = FALSE)),
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
        
        menuItem(
            "Microbiome Typing",
            icon = icon("layer-group"),
            menuItem("Vaginal CSTs", tabName = "cst", icon = icon("person-dress")),
            menuItem("Enterotypes", tabName = "biotype", icon = icon("medium"))
        ),
        menuItem(
            "Tools",
            icon = icon("screwdriver-wrench"),
            menuItem("Abundance Extraction", tabName = "extraction_abu", icon = icon("table"))
        )
        #menuItem("Releases", tabName = "releases", icon = icon("info"))
    )
)

# Dashboard Body ----------------------------------------------------------
body <- dashboardBody(
    use_theme(mytheme), # <-- use the theme
    tabItems(
        #tabItem("overview",  analysis_overview_ui()),
        #tabItem("home", includeMarkdown("www/home1.md")),
        tabItem("home",
                div(class = "scrollable",
                    htmlTemplate("www/home1.html")
                )
        ),
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
        tabItem("adonis", adonis_all_ui("adonis")),
        tabItem("taxa_composition_sample", taxa_composition_sample_ui("taxa_composition_sample")),
        tabItem("extraction_abu", abu_table_ui("extraction_abu")),
        tabItem("biotype", biotype_ui("biotype"))
        
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


