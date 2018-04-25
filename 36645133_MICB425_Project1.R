
library("tidyverse")

source("https://bioconductor.org/biocLite.R")

biocLite('phyloseq')
library("phyloseq")

library("magrittr")

set.seed(4832)

m.norm = rarefy_even_depth(qiime2, sample.size=100000)


m.perc = transform_sample_counts(m.norm, function(x) 100 * x/sum(x))


m.alpha = estimate_richness(m.norm, measures = c("Chao1", "Shannon"))

m.meta.alpha = full_join(rownames_to_column(m.alpha), rownames_to_column(data.frame(m.perc@sam_data)), by = "rowname")

#subsettaxa only works with mothur & not Qime
m.perc %>% 
  subset_taxa(Phylum=="Cyanobacteria") %>% 
  psmelt() %>% 
  group_by(Sample) %>% 
  summarize(Abundance_sum=sum(Abundance), Depth_m=mean(Depth_m)) %>% 
  
  ggplot() +
  geom_point(aes(x=Depth_m, y=Abundance_sum)) +
  geom_smooth(method='lm', aes(x=as.numeric(Depth_m), y=Abundance_sum)) +
  labs(title="Abundance of Cyanobacteria across oxygen concentrations")





m.meta.alpha %>% 
  
  ggplot() +
  geom_point(aes(x=Depth_m, y=Shannon)) +
  geom_smooth(method='auto', aes(x=as.numeric(Depth_m), y=Shannon)) +
  labs(title="Example 1: Alpha-diversity across depth", y="Shannon's diversity index", x="Depth (m)")



m.perc %>% 
  
  plot_bar(fill="Phylum") + 
  geom_bar(aes(fill=Phylum), stat="identity") +
  labs(title="Example 4: Phylum across samples")



#calculaing significance with linear model for detph

m.norm %>% 
  subset_taxa(Phylum=="Cyanobacteria") %>% 
  tax_glom(taxrank = 'Phylum') %>%
  psmelt() %>%
  
  lm(Abundance ~ Depth_m, .) %>% 
  summary()

#p-value is 0.01358
#calculating significance with linear model for oxygen

m.norm %>% 
  subset_taxa(Phylum=="Cyanobacteria") %>% 
  tax_glom(taxrank = 'Phylum') %>% 
  psmelt() %>% 
  
  lm(Abundance ~ O2_uM, .) %>% 
  summary()

#p-value is 0.0001194



#subsettaxa only works with mothur & not Qime
m.perc %>% 
  subset_taxa(Phylum=="Cyanobacteria") %>% 
  psmelt() %>% 
  group_by(Sample) %>% 
  summarize(Abundance_sum=sum(Abundance), Depth_m=mean(Depth_m)) %>% 
  
  ggplot() +
  geom_point(aes(x=Depth_m, y=Abundance_sum)) +
  geom_smooth(method='lm', aes(x=as.numeric(Depth_m), y=Abundance_sum)) +
  labs(title="Abundance of Cyanobacteria across oxygen concentrations")




set.seed(4832)
q.norm = rarefy_even_depth(qiime2, sample.size=100000)

dim(otu_table(q.norm))

dim(otu_table(m.norm))

