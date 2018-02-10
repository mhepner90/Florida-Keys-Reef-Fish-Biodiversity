all_matrices_data = read_csv("diversity_data.csv")

x = c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use")

matrices_marine_zones = all_matrices_data %>% 
  group_by(MPA_NAME) %>% #add subregion later 
  summarize(
    abundance_mean = mean(abundance_sum),
    abundance_n = length(abundance_sum),
    abundance_sd = sd(abundance_sum),
    abundance_se = abundance_sd / sqrt(abundance_n),
    abundance_min = min(abundance_sum),
    abundance_max = max(abundance_sum),
    biomass_mean = mean(biomass_sum),
    biomass_n = length(biomass_sum),
    biomass_sd = sd(biomass_sum),
    biomass_se = biomass_sd / sqrt(biomass_n),
    biomass_min = min(biomass_sum),
    biomass_max = max(biomass_sum),
    evenness_mean = mean(evenness),
    evenness_n = length(evenness),
    evenness_sd = sd(evenness),
    evenness_se = evenness_sd / sqrt(evenness_n),
    evenness_min = min(evenness),
    evenness_max = max(evenness),
    richness_mean = mean(richness),
    richness_n = length(richness),
    richness_sd = sd(richness),
    richness_se = richness_sd / sqrt(richness_n),
    richness_min = min(richness),
    richness_max = max(richness),
    shannon_n = length(shannon),
    shannon_sd = sd(shannon),
    shannon_se = shannon_sd / sqrt(shannon_n),
    shannon_min = min(shannon),
    shannon_max = max(shannon),
    simpson_mean = mean(simpson),
    simpson_n = length(simpson),
    simpson_sd = sd(simpson),
    simpson_se = simpson_sd / sqrt(simpson_n),
    simpson_min = min(simpson),
    simpson_max = max(simpson),
    shannon_mean = mean(shannon),
    func.div_mean = mean(func.div),
    func.div_n = length(func.div),
    func.div_sd = sd(func.div),
    func.div_se = func.div_sd / sqrt(func.div_n),
    func.div_min = min(func.div),
    func.div_max = max(func.div)) %>%
  mutate(MPA_NAME = factor(MPA_NAME, levels = x))%>%
  arrange(MPA_NAME)

#Abundance 

abundance_ntmr_plot =  
  ggplot(matrices_marine_zones,aes(x=MPA_NAME, y=abundance_mean, color=MPA_NAME, fill=MPA_NAME))+  
  geom_bar(position="dodge",stat="identity",color="black",size=.3)+ # color="black",size=.3
  geom_errorbar(aes(max=abundance_mean+abundance_se, min=abundance_mean-abundance_se), size=.6,width=0.3,position=position_dodge(.9))+
  labs(title="A) Abundance", x="", y="Mean abundance \nnumber of individuals (± SE)",color="",fill="")+
  scale_fill_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  scale_color_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    axis.title.x=element_text(size=12),
    axis.title.y=element_text(size=12),
    legend.text=element_text(size=12),
    plot.title=element_text(hjust=0, size=14))
ggsave(file="abundance_by_level_of_protection_bargraph.pdf",abundance_ntmr_plot, width=10,height=4, path="plots")

#Biomass
biomass_ntmr_plot =  
  ggplot(matrices_marine_zones,aes(x=MPA_NAME, y=biomass_mean, color=MPA_NAME, fill=MPA_NAME))+  
  geom_errorbar(aes(ymax=biomass_mean+biomass_se, ymin=biomass_mean-biomass_se), size=.6,width=0.3,position=position_dodge(.9))+
  geom_bar(position="dodge",stat="identity")+ # color="black",size=.3
  labs(title="B) Biomass", x="", y="Mean biomass \nkilograms per 40,000 km2 (± SE)",color="",fill="")+
  scale_fill_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  scale_color_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    axis.title.x=element_text(size=12),
    axis.title.y=element_text(size=12),
    legend.text=element_text(size=12),
    plot.title=element_text(hjust=0, size=14))
ggsave(file="biomass_by_level_of_protection_bargraph.pdf",biomass_ntmr_plot, width=10,height=4, path="plots")

#Evenness
evenness_ntmr_plot =  
  ggplot(matrices_marine_zones,aes(x=MPA_NAME, y=evenness_mean, color=MPA_NAME, fill=MPA_NAME))+  
  geom_errorbar(aes(ymax=evenness_mean+evenness_se, ymin=evenness_mean-evenness_se), size=.6,width=0.3,position=position_dodge(.9))+
  geom_bar(position="dodge",stat="identity")+ # color="black",size=.3
  labs(title="C) Evenness", x="", y="Mean Evenness (± SE)",color="",fill="")+
  scale_fill_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  scale_color_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    axis.title.x=element_text(size=12),
    axis.title.y=element_text(size=12),
    legend.text=element_text(size=12),
    plot.title=element_text(hjust=0, size=14))
ggsave(file="evenness_by_level_of_protection_bargraph.pdf",evenness_ntmr_plot, width=10,height=4, path="plots")

#Richness
richness_ntmr_plot =  
  ggplot(matrices_marine_zones,aes(x=MPA_NAME, y=richness_mean, color=MPA_NAME, fill=MPA_NAME))+  
  geom_errorbar(aes(ymax=richness_mean+richness_se, ymin=richness_mean-richness_se), size=.6,width=0.3,position=position_dodge(.9))+
  geom_bar(position="dodge",stat="identity")+ # color="black",size=.3
  labs(title="D) Richness ", x="", y="Mean richness \neffective number of species (± SE)",color="",fill="")+
  scale_fill_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  scale_color_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    axis.title.x=element_text(size=12),
    axis.title.y=element_text(size=12),
    legend.text=element_text(size=12),
    plot.title=element_text(hjust=0, size=14))
ggsave(file="richness_by_level_of_protection_bargraph.pdf",richness_ntmr_plot, width=10,height=4, path="plots")

#Shannon
shannon_ntmr_plot =  
  ggplot(matrices_marine_zones,aes(x=MPA_NAME, y=shannon_mean, color=MPA_NAME, fill=MPA_NAME))+  
  geom_errorbar(aes(ymax=shannon_mean+shannon_se, ymin=shannon_mean-shannon_se), size=.6,width=0.3,position=position_dodge(.9))+
  geom_bar(position="dodge",stat="identity")+ # color="black",size=.3
  labs(title="E) Shannon", x="", y="Mean Shannon  \neffective number of species (± SE)",color="",fill="")+
  scale_fill_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  scale_color_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    axis.title.x=element_text(size=12),
    axis.title.y=element_text(size=12),
    legend.text=element_text(size=12),
    plot.title=element_text(hjust=0, size=14))
ggsave(file="shannon_by_level_of_protection_bargraph.pdf",shannon_ntmr_plot, width=10,height=4, path="plots")

#Simpson
simpson_ntmr_plot =  
  ggplot(matrices_marine_zones,aes(x=MPA_NAME, y=simpson_mean, color=MPA_NAME, fill=MPA_NAME))+  
  geom_errorbar(aes(ymax=simpson_mean+simpson_se, ymin=simpson_mean-simpson_se), size=.6,width=0.3,position=position_dodge(.9))+
  geom_bar(position="dodge",stat="identity")+ # color="black",size=.3
  labs(title="F) Simpson", x="", y="Mean Simpson \neffective number of species (± SE)",color="",fill="")+
  scale_fill_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  scale_color_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    axis.title.x=element_text(size=12),
    axis.title.y=element_text(size=12),
    legend.text=element_text(size=12),
    plot.title=element_text(hjust=0, size=14))
ggsave(file="simpson_by_level_of_protection_bargraph.pdf",simpson_ntmr_plot, width=10,height=4, path="plots")

#Functional
functional_ntmr_plot =  
  ggplot(matrices_marine_zones,aes(x=MPA_NAME, y=func.div_mean, color=MPA_NAME, fill=MPA_NAME))+  
  geom_errorbar(aes(ymax=func.div_mean+func.div_se, ymin=func.div_mean-func.div_se), size=.6,width=0.3,position=position_dodge(.9))+
  geom_bar(position="dodge",stat="identity")+ # color="black",size=.3
  labs(title="G) Functional", x="", y="Mean functional \neffective number of species (± SE)",color="",fill="")+
  scale_fill_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  scale_color_manual(
    labels= c("Not Protected","Ecological Reserve","Sanctuary Preservation Area","Special Use"),
    values=c(
      "Not Protected"="red",
      "Ecological Reserve"="forestgreen",
      "Sanctuary Preservation Area"="orange",
      "Special Use"="blue"))+
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    axis.title.x=element_text(size=12),
    axis.title.y=element_text(size=12),
    legend.text=element_text(size=12),
    plot.title=element_text(hjust=0, size=14))
ggsave(file="functional_by_level_of_protection_bargraph.pdf",functional_ntmr_plot, width=10,height=4, path="plots")

group_ntmr <- grid.arrange(abundance_ntmr_plot,biomass_ntmr_plot,evenness_ntmr_plot, richness_ntmr_plot,shannon_ntmr_plot,simpson_ntmr_plot,functional_ntmr_plot, ncol=2)

ggsave(file="all_by_ntmr_bargraph.pdf", group_ntmr, width=7, height=10, path="plots")
