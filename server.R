server <- function(input, output, session) {
  
  #16S
  mpse <- upload_data_mod("upload")
  observe({
    req(mpse)
    mpse_rare <- data_rarefy_mod("rarefy", mpse)
    observeEvent(mpse_rare(),{
      mpse_filter <- filter_mod("filter",mpse_rare())
      observeEvent(mpse_filter(),{
        #print(mpse_filter())
        # meta <- mpse_rare() %>% mp_extract_sample()
        # print(meta)
        #mpse_rared <- mpse_rare()
        mpse_filtered <- mpse_rare() %>% filter(Sample %in% mpse_filter()) 
        #x <- meta %>% filter(Sample %in% mpse_filter()) 
        #print(x)
        alpha_index_mod("a_index",mpse_filtered)
        alpha_mul_index_mod("a_mul_index", mpse_filtered)
        rare_curve_mod("rare", mpse_filtered)
        beta_pca_mod("pca", mpse_filtered)
        beta_pcoa_mod("pcoa", mpse_filtered)
        beta_nmds_mod("nmds", mpse_filtered)
        cca_mod("cca", mpse_filtered)
        rda_mod("rda", mpse_filtered)
        beta_hcluster_mod("hcluster", mpse_filtered)
        beta_anosim_mod("anosim", mpse_filtered)
        taxa_composition_mod("taxa_diff", mpse_filtered)
        feature_composition_mod("feature_diff", mpse_filtered)
        lefse_mod("lefse", mpse_filtered)
        diff_clade_mod("diff_clade", mpse_filtered)
        maaslin_mod("maaslin", mpse_filtered)
        cst_mod("cst", mpse_filtered)
        tSNE_mod("tsne", mpse_filtered)
        diff_barplot_mod("diff_bar", mpse_filtered)
        adonis_all_mod("adonis", mpse_filtered)
        taxa_composition_sample_mod("taxa_composition_sample", mpse_filtered)
      })
    })
  })
  
  #
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
  #other tools
  psm_mod("psm")
}

shinyServer(server)







