# analysis_overview_ui <- function() {
#     res <- fixedPage(
#         fixedRow(
#             column(
#                 width = 6, offset = 3,
#                 div(
#                     class = "panel-page-head", style = "Width: 100%; text-align: center;",
#                     h3(class = "panel-page-title", strong("分析总览"))
#                 )
#             )
#         ),
#         fixedRow(
#             column(
#                 width = 2, offset = 3,
#                 box(
#                     title = "分析前准备", status = "primary", solidHeader = TRUE,
#                     width = 12, height = "300px",
#                     tags$b("数据上传"),
#                     tags$b("数据筛选")
#                 )
#             ),
#             column(
#                 width = 2,
#                 box(
#                     title = "a多样性分析", status = "primary", solidHeader = TRUE,
#                     width = 12, height = "300px", tags$b("Alpha多样性指数比较"),
#                     tags$b("稀释曲线")
#                 )
#             ),
#             column(
#                 width = 2,
#                 box(
#                     title = "β多样性分析", status = "primary", solidHeader = TRUE,
#                     width = 12, height = "300px", tags$b("PCA分析"), tags$b("PCoA分析"),
#                     tags$b("NMDS分析"), tags$b("样本层级聚类分析"), tags$b("Adonis分析"),
#                     tags$b("Anosim分析")
#                 )
#             )
#         ),
#         fixedRow(
#             column(
#                 width = 2, offset = 3,
#                 box(
#                     title = "群落组成分析", status = "primary", solidHeader = TRUE,
#                     width = 12, height = "250px",
#                     tags$b("Bar图"), tags$b("Venn/Upset图")
#                 )
#             ),
#             column(
#                 width = 2,
#                 box(
#                     title = "环境因子分析", status = "primary", solidHeader = TRUE,
#                     width = 12, height = "250px",
#                     tags$b("CCA/RDA分析"), tags$b("Maaslin2分析")
#                 )
#             ),
#             column(
#                 width = 2, offset = 3,
#                 box(
#                     title = "差异物种分析", status = "primary", solidHeader = TRUE,
#                     width = 12, height = "250px", tags$b("LEfse分析")
#                 )
#             )
#         )
#     )
#     return(res)
# }




analysis_overview_ui <- function() {
    res <- fixedPage(
        div(
            class = "panel-page-head",
            h3(class = "panel-page-title", strong("分析总览"), style="text-align: center;")
        ),
        fixedRow(
            column(
                width = 10, offset = 1,
                column(
                    width = 4,
                    box(
                        title = "分析前准备", status = "primary", solidHeader = TRUE,
                        width = 12, height = "400px",
                        tags$p("数据上传"),
                        tags$p("数据总览"),
                        tags$p("数据筛选")
                    )
                ),
                column(
                    width = 4,
                    box(
                        title = "a多样性分析", status = "primary", solidHeader = TRUE,
                        width = 12, height = "400px", 
                        tags$p("Alpha多样性指数"), 
                        tags$p("Alpha多样性组间比较"),
                        tags$p("稀释曲线")
                    )
                ),
                column(
                    width = 4,
                    box(
                        title = "β多样性分析", status = "primary", solidHeader = TRUE,
                        width = 12, height = "400px", 
                        tags$p("PCA分析"), 
                        tags$p("PCoA分析"),
                        tags$p("NMDS分析"), 
                        tags$p("Adonis分析"), 
                        tags$p("Anosim分析"),
                        tags$p("样本层级聚类分析")
                    )
                )
            ),
            column(
                width = 10, offset = 1,
                column(
                width = 4,
                box(
                    title = "群落组成分析", status = "primary", solidHeader = TRUE,
                    width = 12, height = "250px",
                    tags$p("Bar图")
                    
                )
            ),
            column(
                width = 4,
                box(
                    title = "环境因子分析", status = "primary", solidHeader = TRUE,
                    width = 12, height = "250px",
                    tags$p("Maaslin分析")
                    # tags$p("CCA/RDA分析")
                )
            ),
            column(
                width = 4,
                box(
                    title = "差异物种分析", status = "primary", solidHeader = TRUE,
                    width = 12, height = "250px", tags$p("LEfse分析")
                )
            )
            ) 
        )
    )
    return(res)
}



# ,
#             column(
#                 width = 2,
#                 box(
#                     title = "进化分析", status = "primary", solidHeader = TRUE,
#                     width = 12, height = "250px", tags$p("系统发育树")
#                 )
#             ),
#             column(
#                 width = 2,
#                 box(
#                     title = "功能预测", status = "primary", solidHeader = TRUE,
#                     width = 12, height = "250px",
#                     tags$p("PICRUst")
#                 )
#             )
