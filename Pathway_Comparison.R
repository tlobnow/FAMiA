# LAMP Comparison

# libraries
library(tidyverse)
library(data.table)
#if (!require(devtools)) install.packages("devtools")
#devtools::install_github("gaospecial/ggVennDiagram")
library(ggVennDiagram)
library(RColorBrewer)
library(paletteer)

# laod data

setwd("/Users/finnlo/Documents/CellDesigner/csv_files/")
Apicomplexa <- read.csv('comparison_of_apicomplexan_and_human_metabolic_capabilities_release2.csv', na.strings = c('', ' ', 'NA', NA))
Apicomplexa_subset <- Apicomplexa %>% select(Pathway.name, Enzyme.name, T..gondii, P..falciparum, C..parvum, Absent.in.Human.)


################################################################################
# subset Enzyme.name per Parasite
Toxo    <- Apicomplexa_subset %>% 
  filter(!is.na(T..gondii), Pathway.name %in% c('Fatty acid biosynthesis in the apicoplast (FAS II system)', 'Fatty acid elongation in the cytosol (FAS I system)', 'Fatty acid elongation via elongase pathway of ER', 'Fatty acid recycling and degradation')) %>% 
  mutate(T.gondii = Enzyme.name) %>% select(Enzyme.name, T.gondii) %>%  distinct()
Plasmo  <- Apicomplexa_subset %>% 
  filter(!is.na(P..falciparum), Pathway.name %in% c('Fatty acid biosynthesis in the apicoplast (FAS II system)', 'Fatty acid elongation in the cytosol (FAS I system)', 'Fatty acid elongation via elongase pathway of ER', 'Fatty acid recycling and degradation')) %>% 
  mutate(P.falciparum = Enzyme.name) %>% select(Enzyme.name, P.falciparum) %>% distinct()
Crypto  <- Apicomplexa_subset %>% 
  filter(!is.na(C..parvum), Pathway.name %in% c('Fatty acid biosynthesis in the apicoplast (FAS II system)', 'Fatty acid elongation in the cytosol (FAS I system)', 'Fatty acid elongation via elongase pathway of ER', 'Fatty acid recycling and degradation')) %>% 
  mutate(C.parvum = Enzyme.name)     %>% select(Enzyme.name, C.parvum) %>% distinct()
Human   <- Apicomplexa_subset %>% 
  filter(!is.na(Absent.in.Human.), Pathway.name %in% c('Fatty acid biosynthesis in the apicoplast (FAS II system)', 'Fatty acid elongation in the cytosol (FAS I system)', 'Fatty acid elongation via elongase pathway of ER', 'Fatty acid recycling and degradation')) %>% 
  mutate(Absent.in.Humans = Enzyme.name) %>% select(Enzyme.name, Absent.in.Humans) %>% distinct()

# join
FA_Pathways <- full_join(Toxo, Plasmo)
FA_Pathways <- full_join(FA_Pathways, Crypto)
FA_Pathways <- full_join(FA_Pathways, Human)
FA_Pathways <- FA_Pathways %>% select(-Enzyme.name) %>% distinct(., .keep_all = T)

p <- ggVennDiagram(FA_Pathways, label_alpha = 0.3, show_intersect = F, edge_size = 0, label = "count")
p + scale_fill_paletteer_c(palette = 'viridis::plasma') +
  ggtitle("Enzymes in Fatty Acid Pathways") +
  theme(plot.title = element_text(hjust = 0.5))

ggVennDiagram(FA_Pathways, label_alpha = 0.3, show_intersect = T, edge_size = 0, label = "count")


################################################################################
# general pathways
Toxo    <- Apicomplexa_subset %>% 
  filter(!is.na(T..gondii)) %>% 
  mutate(T.gondii = Enzyme.name) %>% select(Enzyme.name, T.gondii) %>%  distinct()
Plasmo  <- Apicomplexa_subset %>% 
  filter(!is.na(P..falciparum)) %>% 
  mutate(P.falciparum = Enzyme.name) %>% select(Enzyme.name, P.falciparum) %>% distinct()
Crypto  <- Apicomplexa_subset %>% 
  filter(!is.na(C..parvum)) %>% 
  mutate(C.parvum = Enzyme.name)     %>% select(Enzyme.name, C.parvum) %>% distinct()
Human   <- Apicomplexa_subset %>% 
  filter(!is.na(Absent.in.Human.)) %>% 
  mutate(Absent.in.Humans = Enzyme.name) %>% select(Enzyme.name, Absent.in.Humans) %>% distinct()

all_Pathways <- full_join(Toxo, Plasmo)
all_Pathways <- full_join(all_Pathways, Crypto)
all_Pathways <- full_join(all_Pathways, Human)
all_Pathways <- all_Pathways %>% select(-Enzyme.name) %>% distinct()

p <- ggVennDiagram(all_Pathways, label_alpha = 0.3, show_intersect = F, edge_size = 0, label = "count")
p + scale_fill_paletteer_c(palette = 'viridis::plasma') +
  ggtitle("Enzymes in Apicomplexan Pathways") +
  theme(plot.title = element_text(hjust = 0.5))

# plotly
ggVennDiagram(all_Pathways, label_alpha = 0.3, show_intersect = T, edge_size = 0, label = "count")

################################################################################
# subset Pathway.name per Parasite
Toxo2    <- Apicomplexa_subset %>% filter(!is.na(T..gondii))     %>% mutate(T.gondii = Pathway.name)     %>% select(Pathway.name, T.gondii) %>% distinct()
Plasmo2  <- Apicomplexa_subset %>% filter(!is.na(P..falciparum)) %>% mutate(P.falciparum = Pathway.name) %>% select(Pathway.name, P.falciparum) %>% distinct()
Crypto2  <- Apicomplexa_subset %>% filter(!is.na(C..parvum))     %>% mutate(C.parvum = Pathway.name)     %>% select(Pathway.name, C.parvum) %>% distinct()
Human2   <- Apicomplexa_subset %>% filter(!is.na(Absent.in.Human.)) %>% mutate(Absent.in.Humans = Pathway.name)%>% select(Pathway.name, Absent.in.Humans) %>% distinct()

# join
Apicomplexan_Pathways <- full_join(Toxo2, Plasmo2)
Apicomplexan_Pathways <- full_join(Apicomplexan_Pathways, Crypto2) 
Apicomplexan_Pathways <- full_join(Apicomplexan_Pathways, Human2) %>%
  select(T.gondii, P.falciparum, C.parvum, Absent.in.Humans) %>%
  distinct(., .keep_all = T)

p <- ggVennDiagram(Apicomplexan_Pathways, label_alpha = 0.3, show_intersect = F, edge_size = 0, label = "count")
p + scale_fill_paletteer_c(palette = 'viridis::plasma') +
  ggtitle("Venn Diagram of Apicomplexan Pathways") +
  theme(plot.title = element_text(hjust = 0.5))

# plotly version (with hover)
ggVennDiagram(Apicomplexan_Pathways, label_alpha = 0.3, show_intersect = T, edge_size = 0, label = "count")







