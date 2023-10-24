source("psm.R")
source("upload_epide.R")
source("filter_epide.R")
source("function_to_epide.R")
source("epide_descriptive.R")

sidebar_epide <- dashboardSidebar(
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
            text = "Data Processing",
            icon = icon("fas fa-file"),
            menuSubItem("Upload Data", tabName = "upload_epide",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("Data Filter", tabName = "filter_epide",icon = icon("angle-double-right", verify_fa = FALSE)),
            menuSubItem("PSM", tabName = "psm",icon = icon("angle-double-right", verify_fa = FALSE))
        ),
        menuItem(
            text = "Baseline Analysis",
            icon = icon("fas fa-file"),
            menuSubItem("Descriptive Statistics", tabName = "descriptive_epide",icon = icon("angle-double-right", verify_fa = FALSE))
        )
    )
)


body_epide <- dashboardBody(
    use_theme(mytheme), # <-- use the theme
    tabItems(
        tabItem("upload_epide", epide_upload_ui("upload_epide")),
        tabItem("filter_epide", filter_epide_ui("filter_epide")),
        tabItem("psm", psm_ui("psm")),
        tabItem("descriptive_epide", descriptive_ui("descriptive_epide"))
    )
)

page_epide <-  shinydashboardPlus::dashboardPage(
    skin = "blue-light",
    scrollToTop = TRUE,
    header = header,
    sidebar = sidebar_epide,
    body = body_epide
)
