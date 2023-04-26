library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(shinyjqui)
library(shinydashboardPlus)
library(datamods)
library(fresh)
library(DT)
library(ggplot2)
library(cowplot)
library(shinybusy)
library(bsplus)
library(markdown)
library(reticulate)
library(ggprism)
library(ggsignif)
library(rstatix)
#add
library(tidyverse)

#library(bs4Dash)
#library(shinyBS)
#library(MicrobiotaProcess)
#library(phyloseq)
#library(gghalves)
#library(dplyr)
#library(picante)
#library(ggside)
#library(Maaslin2) 
#library(ggupset) 
#library(ggtree)

options(shiny.maxRequestSize = 30*1024^2)
alpha_index <- c("Observe", "Chao1", "ACE", "Shannon", "Simpson", "Pielou")
s <- function(x) {
    if(is.null(x)) {
        return(x)
    }else {
        return(sym(x))
    }
}

# Create the metaG UI theme----------------------------
mytheme <- create_theme(
    adminlte_color(
        light_blue = "#6f98ca"
    ),
    adminlte_sidebar(
        width = "300px",
        light_bg = "#f9fafb",
        light_hover_bg = "#dadee9",
        light_color = "#515b66",
        light_hover_color = "#515b66",
        light_submenu_bg = "#f9fafb",
        light_submenu_color = "#515b66",
        light_submenu_hover_color = "#000000"	
        
    )
)
header <- dashboardHeader(disable = TRUE)
# metaG tax load data————————————————————————————————————
upload_buglist <- function(bug_list, meta_list) {
    
    if (!grepl("\\.(tsv|xls|txt)$", bug_list)) {
        stop("The input file format must be tsv, xls, or txt.")
    }
    
    bug_all <- switch(tolower(tools::file_ext(bug_list)),
                      "tsv" = read_tsv(bug_list, comment = "#"),
                      "xls" = read_tsv(bug_list, comment = "#"),
                      "txt" = read_tsv(bug_list, comment = "#"),
                      stop("Unsupported file format"))
    
    bug_all_fix <- bug_all %>%
        filter(clade_name != "#SampleID") %>%
        select(-clade_name, clade_name) %>%
        mutate(taxonomy = gsub("\\|", ";", clade_name)) %>%
        select(-clade_name) %>%
        add_column(OUT_ID = paste0("Sp", seq(1:nrow(.))), .before = 1)
    
    bug_all_fix[is.na(bug_all_fix)] <- "0"
    #print(bug_all_fix)
    write_tsv(bug_all_fix,"bugs_CMAP_input.txt")
    mpse <- mp_import_qiime("bugs_CMAP_input.txt", meta_list)
    
    # Remove temporary file
    file.remove("bugs_CMAP_input.txt")
    return(mpse)
    
}
#----------------------------------------------------------------



cmap_theme <- theme(
    text = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.text = element_text(size = 16),#, family = "Arial"),
    panel.grid = element_blank()
)

anosim_theme <- theme(
    text = element_text(
        size = 8, 
        color = "black"),
    plot.title = element_text(
        size = 20,
        face = "bold",
        color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
        size = 12, 
        face = "bold",
        color="#1b2838"
    ),
    plot.title.position = "plot",# slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed")
)


calm_upload_plain <- function(otufilename, mapfilename, otutree = NULL) {
    mpseda <- mp_import_qiime(otufilename, mapfilename)
    return(mpseda)
}

calm_upload_biom <- function(BIOMfilename, mapfilename, otutree = NULL) {
    biomda <- biomformat::read_biom(BIOMfilename)
    mpseda <- as.MPSE(biomda)
    metada <- read.table(mapfilename, header = TRUE, sep = "\t",
        comment.char = "", check.names = FALSE, quote = ""
    )
    #y.by <- colnames(metada)[1]
    colnames(metada)[1] <- "Sample"

    ##get intersection between MP samples and meta samples
    mp_sample <- mpseda %>% mp_extract_sample
    meta_sample <- metada[,1]
    sample_intersect <- intersect(mp_sample[[1]], meta_sample)
    mpseda %<>% filter(Sample %in% sample_intersect)
    metada %<>% filter(Sample %in% sample_intersect)
    #mpseda %<>% left_join(metada, by = c("Sample" = y.by))
    mpseda %<>% left_join(metada, by = c("Sample" = "Sample"))
    if (!is.null(otutree)) {
        treeda <- ape::read.tree(otutree)
        otutree(mpseda) <- treeda
    }
    return(mpseda)
}



cols <- toupper(c(
    "#fb8072","#80b1d3","#bebada","#fdb462","#b3de69","#fccde5","#FDBF6F","#A6CEE3",
    "#56B4E9","#B2DF8A","#FB9A99","#CAB2D6","#A9C4E2","#79C360","#FDB762","#9471B4",
    "#A4A4A4","#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc","#e5d8bd",
    "#fddaec","#f2f2f2","#8dd3c7","#d9d9d9"))

Set2 <- RColorBrewer::brewer.pal(8,"Set2")
Dark2 <- RColorBrewer::brewer.pal(8,"Dark2")

cc <- function(length) {

    if(length < length(cols)) {
        return(cols[1:length])
    }else{
        cRP_cols <- colorRampPalette(cols)
        return(cRP_cols(length))
        #stop(paste0("Length out of limit:", length(cols)))
    }
    
}

cols2 <- function(length) {
    if(length > length(Set2)) {
        cRP_set2 <- colorRampPalette(Set2)
        return(cRP_set2(length))
    }else{
        return(Set2[1:length])
    }
    
}

pattle_drak2 <- function(length) {
    if(length > length(Dark2)) {
        cc <- colorRampPalette(Dark2)
        return(cc(length))
    }else{
        return(Dark2[1:length])
    }
    
}

#############t-SNE Theme
define_theme <- theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(), 
    axis.title = element_text(color='black', size=18),
    axis.ticks.length = unit(0.4, "lines"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_text(hjust = .05),
    axis.title.y = element_text(hjust = .05),
    legend.title = element_blank(),        
    legend.text = element_text(size=18),
    legend.key = element_blank(),
    legend.key.size = unit(1, 'cm')
) 

define_color2 <- c("#DC050C","#FB8072","#1965B0","#7BAFDE","#882E72",
                   "#B17BA6","#FF7F00","#FDB462","#E7298A","#E78AC3",
                   "#33A02C","#B2DF8A","#55A1B1","#8DD3C7","#A6761D",
                   "#E6AB02","#7570B3","#BEAED4","#666666","#999999",
                   "#aa8282","#d4b7b7","#8600bf","#ba5ce3","#808000",
                   "#aeae5c","#1e90ff","#00bfff","#56ff0d","#ffff00")
#############rare data

plot_x <- function(x) {
    colnames(x) <- "OTU"
    x$Sample <- rownames(x)
    x_end <- max(x$OTU) * 1.2
    ggplot(x, aes(y = reorder(Sample, OTU), x = OTU)) +
        geom_point(color = "#FD9347", size = 3) +
        geom_text(aes(label = OTU), hjust = -0.2,  size = 5) +
        ylab("Sample") +
        xlab("Sequence Count") +
        scale_x_continuous(limits = c(0, x_end)) +
        theme_classic() +
        theme(text = element_text(size = 16, family = "serif"))
}

rare_info <- function(mpse_before, mpse_after) {

    sample_before <- mpse_before %>% mp_extract_sample() %>% nrow()
    otu_before <- mpse_before %>% mp_extract_feature() %>% unique() %>% nrow()
    
    sample_after <- mpse_after %>% mp_extract_sample() %>% nrow()
    otu_after <- mpse_after %>% mp_extract_feature() %>% unique() %>% nrow()
    
    sample_prec <- ((sample_before - sample_after) / sample_before)*100
    otu_prec <- ((otu_before - otu_after) / otu_before)*100
    x <- c(sample_before, otu_before, 
           sample_after, otu_after,  
           round(sample_prec, 2), 
           round(otu_prec, 2)
           )
    # x <- paste(sample_before - sample_after, "samples and",otu_before - otu_after, "OTUs", 
    #            "were removed, if you want to keep them you can uncheck 'trim Sample' and 'trim OTU' !")
    names(x) <- c("sample_before", "otu_before", "sample_after", "otu_after", "sample_prec","otu_prec")
    return(x)
}

###########################cladogram color
define_color3 <- c('deepskyblue', 'orange', define_color2)

