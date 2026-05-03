#Creating qiime-R interface

phyloseq_obj <- phyloseq
                         sample_data(metadata))

#Species Richness
library(phyloseq)

physeq <- estimate_richness(physeq, split = TRUE, measures 
= c(index)

p <- ggboxplot(physeq_meta, x = parameters,
               +                palette = c(col), add = "jitter", shape = "parameters")

p <- ggboxplot(physeq_meta, x = "parameters", y = "index, color = "parameters",
               +                palette = c("Col"), add = "jitter", shape = "parmeters")



#Aplha diversity analysis
shannon_amp <- alphadiv(ds, 
+ measure = "index", 
+ richness = FALSE, 
+ rarefy = NULL)

p <- ggboxplot(physeq_meta,
               +                x = "parmeters",
               +                y = "index",
               +                fill = "parameters",
               +                palette = c("col"),
               +                add = "jitter",
               +                shape = "parameters")+
  +     theme(legend.position = "none")+ theme(axis.text.x = element_blank(),
                                               +                                            axis.title.x = element_blank())
p +
 


#Beta diversity analysis 
BC <-vegdist(abund_tab_t,

amp_ordinate(ds, 
+ type = "index", 
+ distmeasure = "", 
+ transform = "hellinger", 
+ sample_color_by = "parameters", 
+ sample_colorframe = TRUE, 
+ sample_colorframe_label = "parameters") + theme(legend.position = "blank")


+ sample_colorframe_label = "Site") 
