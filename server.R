server <- function(input, output, session) {
    mpse <- upload_data_mod("upload")
    observe({
        req(mpse)
        mpse_rare <- data_rarefy_mod("rarefy", mpse)
        observeEvent(mpse_rare(),{
            mpse_filter <- filter_mod("filter",mpse_rare())
            observeEvent(mpse_filter(),{
                #print(mpse_filter())
                alpha_index_mod("a_index",mpse_filter())
                alpha_mul_index_mod("a_mul_index", mpse_filter())
                rare_curve_mod("rare", mpse_filter())
                beta_pca_mod("pca", mpse_filter())
                beta_pcoa_mod("pcoa", mpse_filter())
                beta_nmds_mod("nmds", mpse_filter())
                cca_mod("cca", mpse_filter())
                rda_mod("rda", mpse_filter())
                beta_hcluster_mod("hcluster", mpse_filter())
                beta_anosim_mod("anosim", mpse_filter())
                taxa_composition_mod("taxa_diff", mpse_filter())
                feature_composition_mod("feature_diff", mpse_filter())
                lefse_mod("lefse", mpse_filter())
                diff_clade_mod("diff_clade", mpse_filter())
                maaslin_mod("maaslin", mpse_filter())
                cst_mod("cst", mpse_filter())
                tSNE_mod("tsne", mpse_filter())
                diff_barplot_mod("diff_bar", mpse_filter())
                adonis_all_mod("adonis", mpse_filter())
                
            })
        })
    })
    buglist_mpse <- upload_buglist_mod("upload_buglist")
    observe({
        req(buglist_mpse)
       
        buglist_alpha_index_mod("alphaindex_buglist",buglist_mpse())
        buglist_alpha_mul_index_mod("buglist_a_mul_index",buglist_mpse())
        buglist_beta_pca_mod("buglist_pca",buglist_mpse())
        buglist_beta_pcoa_mod("buglist_pcoa",buglist_mpse())
        buglist_beta_nmds_mod("buglist_nmds",buglist_mpse())
        buglist_beta_anosim_mod("buglist_anosim",buglist_mpse())
        buglist_adonis_all_mod("buglist_adonis",buglist_mpse())
        buglist_beta_hcluster_mod("buglist_hcluster",buglist_mpse())
        buglist_tSNE_mod("buglist_tsne",buglist_mpse())
    })
}

shinyServer(server)








