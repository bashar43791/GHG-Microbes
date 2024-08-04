#Creating qiime-R interface
install.packages("qiime2R")
pip install q2r
phyloseq_obj <- phyloseq(otu_table(filtered_table, taxa_are_rows = TRUE),
                         tax_table(taxonomy),
                         phy_tree(unrooted_tree),
                         sample_data(metadata))
physeq<-qza_to_phyloseq(features = "filtered_table", taxonomy = "taxonomy", tree = "unrooted_tree", metadata = "metadata")
setwd("C:/Users/User/Downloads/")phyloseq<-qza_to_phyloseq(features = "filtered_table.qza", taxonomy = "taxonomy.qza", tree = "unrooted_tree

#Species Richness
install.packages("remotes") 
remotes::install_github("jbisanz/qiime2R") 
library(qiime2R) 
library(phyloseq)

physeq <- estimate_richness(physeq, split = TRUE, measures 
= c("Observed", "Chao1")

my_comparison <- list (c("high saline, "low saline", "medium saline"))

p <- ggboxplot(physeq_meta, x = "Salinity_Gradient", y = "observed", color = "Salinity_Gradient",
               +                palette = c("blue", "red", "green"), add = "jitter", shape = "Salinity_Gradient")

p <- ggboxplot(physeq_meta, x = "Salinity_Gradient", y = "Chao1", color = "Salinity_Gradient",
               +                palette = c("blue", "red", "green"), add = "jitter", shape = "Salinity_Gradient")


my_comparison <- list (c("Aerobic", "Aneroic"))

p <- ggboxplot(physeq_meta, x = "Site", y = "observed", color = "Site",
               +                palette = c("blue", "red"), add = "jitter", shape = "Site")

p <- ggboxplot(physeq_meta, x = "Site", y = "Chao1", color = "Site",
               +                palette = c("blue", "red"), add = "jitter", shape = "Site")


#Aplha diversity analysis
shannon_amp <- amp_alphadiv(ds, 
+ measure = "shannon", 
+ richness = FALSE, 
+ rarefy = NULL)

p <- ggboxplot(physeq_meta,
               +                x = "Salinity_Gradient",
               +                y = "shannon",
               +                fill = "Salinity_Gradient",
               +                palette = c("blue", "red", "green"),
               +                add = "jitter",
               +                shape = "Salinity_Gradient")+
  +     theme(legend.position = "none")+ theme(axis.text.x = element_blank(),
                                               +                                            axis.title.x = element_blank())
p +
  stat_compare_means(comparisons = my_comparison, method = "t.test", hide.ns = TRUE, label = "p.signif") +
  stat_compare_means(label.y = 5.5, method = "t.test", hide.ns = TRUE, label = "p.signif") +
  theme(legend.position = "none")


p <- ggboxplot(physeq_meta,
               +                x = "Site",
               +                y = "shannon",
               +                fill = "Site",
               +                palette = c("blue", "red"),
               +                add = "jitter",
               +                shape = "Site")+
  +     theme(legend.position = "none")+ theme(axis.text.x = element_blank(),
                                               +                                            axis.title.x = element_blank())

p +
  stat_compare_means(comparisons = my_comparison, method = "t.test", hide.ns = TRUE, label = "p.signif") +
  stat_compare_means(label.y = 5.5, method = "t.test", hide.ns = TRUE, label = "p.signif") +
  theme(legend.position = "none")



#Beta diversity analysis
library(vegan) 
BC <-vegdist(abund_tab_t, "bray") 

library(ampvis2)
amp_ordinate(ds, 
+ type = "pcoa", 
+ distmeasure = "bray", 
+ transform = "hellinger", 
+ sample_color_by = "Salinity Gradient", 
+ sample_colorframe = TRUE, 
+ sample_colorframe_label = "Salinity Gradient") + theme(legend.position = "blank")

amp_ordinate(ds, 
+ type = "pcoa", 
+ distmeasure = "bray", 
+ transform = "hellinger", 
+ sample_color_by = "Site", 
+ sample_colorframe = TRUE, 
+ sample_colorframe_label = "Site") + theme(legend.position = "blank")

amp_ordinate(ds, 
+ type = "nmds", 
+ distmeasure = "jaccard", binary=TRUE, 
+ transform = "none", 
+ sample_color_by = "Salinity Gradient", 
+ sample_colorframe = TRUE, 
+ sample_colorframe_label = "Salinity Gradient") 

amp_ordinate(ds, 
+ type = "nmds", 
+ distmeasure = "jaccard", binary=TRUE, 
+ transform = "none", 
+ sample_color_by = "Site", 
+ sample_colorframe = TRUE, 
+ sample_colorframe_label = "Site") 