sidebar_metagenomics <- dashboardSidebar(
    width = 300,
    sidebarMenu(
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
            tabName = "home_meta",
            icon = icon("fas fa-house")
        ),
        menuItem(
            text = "Data Processing",
            icon = icon("fas fa-file"),
            menuSubItem("Upload Data", tabName = "upload_buglist",icon = icon("angle-double-right", verify_fa = FALSE))
        ),
        menuItem(
            "Alpha(α) Diversity",
            icon = icon("a"),
            menuSubItem("Alpha index", tabName = "alphaindex_buglist",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Multi-group Comparison", tabName = "buglist_a_mul_index",icon = icon("angle-double-right", verify_fa = FALSE))
        ),
        menuItem(
            "Beta(β) Diversity",
            icon = icon("b"),
            menuSubItem("PCA", tabName = "buglist_pca",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("PCoA", tabName = "buglist_pcoa",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("NMDS", tabName = "buglist_nmds",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Adonis", tabName = "buglist_adonis",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Anosim", tabName = "buglist_anosim",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Hierarchical Clustering", tabName = "buglist_hcluster",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("t-SNE", tabName = "buglist_tsne",icon = icon("angle-double-right", verify_fa = FALSE))
        )
    )
)


body_metagenomics <- dashboardBody(
    use_theme(mytheme), # <-- use the theme
    tabItems(
        tabItem(tabName = "home_meta", includeMarkdown("www/home.md")),
        tabItem(tabName = "upload_buglist",upload_buglist_ui("upload_buglist")),
        tabItem(tabName = "alphaindex_buglist",buglist_alpha_index_ui("alphaindex_buglist")),
        tabItem(tabName = "buglist_a_mul_index",buglist_alpha_mul_index_ui("buglist_a_mul_index")),
        tabItem(tabName = "buglist_pca",buglist_beta_pca_ui("buglist_pca")),
        tabItem(tabName = "buglist_pcoa",buglist_beta_pcoa_ui("buglist_pcoa")),
        tabItem(tabName = "buglist_nmds",buglist_beta_nmds_ui("buglist_nmd")),
        tabItem(tabName = "buglist_anosim",buglist_beta_anosim_ui("buglist_anosim")),
        tabItem(tabName = "buglist_adonis",buglist_adonis_all_ui("buglist_adonis")),
        tabItem(tabName = "buglist_hcluster",buglist_beta_hcluster_ui("buglist_hcluster")),
        tabItem(tabName = "buglist_tsne",buglist_tSNE_ui("buglist_tsne"))
        
        
    )
)

page_metagenomics <-  shinydashboardPlus::dashboardPage(
    skin = "blue-light",
    scrollToTop = TRUE,
    header = header,
    sidebar = sidebar_metagenomics,
    body = body_metagenomics
)
