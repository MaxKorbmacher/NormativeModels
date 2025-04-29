# Application of Brain Reference to cross sectional and longitudinal MS data
# Max Korbmacher, April 2025
#
# --------------------------------- #
# ------------Structure------------ #
# --------------------------------- #
# 0. Data wrangling---------------- #
# 1. Case-control checks----------- #
# 1.1 number of deviations--------- #
# 1.2 Z-score comparison----------- #
# 1.3 Diagnostic prediction-------- #
# 2. Longitudinal assessment------- #
# 2.1 Development of deviations---- #
# --------------------------------- #
# --------------------------------- #
#
# define the savepath
savepath = "/Users/max/Documents/Local/MS/NormativeModels/results/"
#
# 0. Data wrangling----------------
# load data
cross = read.csv("/Users/max/Documents/Local/MS/NormativeModels/code/Zscores.csv")
long = read.csv("/Users/max/Documents/Local/MS/NormativeModels/code/Zscores_long.csv")
# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,psych,effsize,ggseg,patchwork,rstatix,ggpubr)
# 1. Case-control checks-----------
# 1.1 number of deviations---------
cross$nb_deviations = cross %>%
  select(ends_with("z_score")) %>%
  mutate_all(~ ifelse(abs(.) > 2, 1, 0)) %>%
  transmute(z_score_sum = rowSums(.)) %>%
  pull(z_score_sum)
cross %>% group_by(diagnosis) %>% summarize(M = mean(nb_deviations), SD = sd(nb_deviations))
psych::cohen.d(cross$nb_deviations,factor(cross$diagnosis))[1]
rstatix::t_test(cross, nb_deviations~diagnosis)

# 1.2 Z-score comparison-----------
# Select z_score columns
z_scores = cross %>% select(ends_with("z_score"))

# Add diagnosis vector as a column if not already part of the data frame
z_scores = z_scores %>% mutate(diagnosis = cross$diagnosis)

# Calculate the group-wise mean for each column
group_means = z_scores %>%
  group_by(diagnosis) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(-diagnosis, names_to = "variable", values_to = "mean_value") %>%
  pivot_wider(names_from = diagnosis, values_from = mean_value)

# Calculate difference (MS - HC or vice versa)
diffs = group_means %>%
  mutate(difference = MS - HC)

# Calculate mean and SD of absolute values
diffs %>%
  summarise(mean_abs = mean(abs(difference)),sd_abs = sd(abs(difference)))
# express this as an effect size (difference of means) and add a p-value
effsize::cohen.d(diffs%>% gather(variable, value) %>%filter(variable!="difference")%>%pull(value),diffs%>%gather(variable, value) %>%filter(variable!="difference")%>%pull(variable))
t_test(diffs%>%gather(variable, value) %>%filter(variable!="difference"),value~variable)

# plot group-wise Z-vals and their differences
z_long <- diffs %>%
  rename(region = variable) %>%
  pivot_longer(cols = c(HC, MS, difference), names_to = "group", values_to = "value") %>%
  mutate(
    group = factor(group, levels = c("HC", "MS", "difference")),
    region = str_remove(region, "_z_score"), # Remove suffix
  )

# Split into cortical vs subcortical based on naming patterns
z_cortical <- z_long %>% filter(str_detect(region, "^lh_|^rh_")) 
#%>%mutate(region = str_remove(region, "^lh_|^rh_"))
z_subcortical <- z_long %>% filter(!str_detect(region, "^lh_|^rh_"))



# order the data frame
z_cortical = z_cortical[order(z_cortical$region),]
z_subcortical = z_subcortical[order(z_subcortical$region),] 

# label data correctly
z_cortical = (z_cortical %>% group_by(group)) %>% mutate(label = c(replicate(2,(brain_regions(dk)[brain_labels(dk) %in% gsub("_volume","",z_cortical$region)][1:34]))))
z_subcortical = (z_subcortical %>% group_by(group)) %>% mutate(label = (c(brain_labels(aseg)[grepl(c("halamus|allidum|mygdala|campus|utamen|audate"),brain_labels(aseg))],"Left-Cerebellum-Cortex","Right-Cerebellum-Cortex"))[order(c(brain_labels(aseg)[grepl(c("halamus|allidum|mygdala|campus|utamen|audate"),brain_labels(aseg))],"Left-Cerebellum-Cortex","Right-Cerebellum-Cortex"))])
# add hemi
z_cortical$hemi = ifelse(grepl("lh_",z_cortical$region)==T,"left","right")
z_subcortical$hemi = ifelse(grepl("Left.",z_subcortical$region)==T,"left","right")

names(z_cortical) = c("label","group","Z","region","hemi")
names(z_subcortical) = c("region","group","Z","label","hemi")
#
#
#Cortical plots
C1 = ggplot(dk %>% as_tibble() %>% left_join(z_cortical %>% select(region,hemi,Z) %>% filter(group=="HC") %>% as_tibble())) + geom_brain(atlas = dk,aes(fill = Z),color="black")+
  scale_fill_gradient2(low = "blue",mid = "white",high="red",limits = c(-1.25,1.75)) +
  theme_void() + theme(legend.position="none")
C2 = ggplot(dk %>% as_tibble() %>% left_join(z_cortical %>% select(region,hemi,Z) %>% filter(group=="MS") %>% as_tibble())) + geom_brain(atlas = dk,aes(fill = Z),color="black")+
  scale_fill_gradient2(low = "blue",mid = "white",high="red",limits = c(-1.25,1.75)) +
  theme_void() + theme(legend.position="none")
C3 = ggplot(dk %>% as_tibble() %>% left_join(z_cortical %>% select(region,hemi,Z) %>% filter(group=="difference") %>% as_tibble())) + geom_brain(atlas = dk,aes(fill = Z),color="black")+
  scale_fill_gradient2(low = "blue",mid = "white",high="red",limits = c(-1.25,1.75)) +
  theme_void() + theme(legend.position="none")

# Subcortical plots
## This plots exclusively the coronal stats (all others are troublesome)
coronal_brain_aseg = as_tibble(aseg) %>%
  filter(side == "coronal", !grepl("\\d", label))
z_subcortical = merge(z_subcortical,coronal_brain_aseg,by="label")
S1 = ggplot(z_subcortical%>%filter(group=="HC")) + geom_brain(atlas = aseg, side = "coronal",aes(fill = Z),color="black")+
  scale_fill_gradient2(low = "blue",mid = "white",high="red",limits = c(-1.25,1.75)) +
  #labs(title="Regional volume loss") + 
  theme_void()
S2 = ggplot(z_subcortical%>%filter(group=="MS")) + geom_brain(atlas = aseg, side = "coronal",aes(fill = Z),color="black")+
  scale_fill_gradient2(low = "blue",mid = "white",high="red",limits = c(-1.25,1.75)) +
  #labs(title="Regional volume loss") + 
  theme_void()
S3 = ggplot(z_subcortical%>%filter(group=="difference")) + geom_brain(atlas = aseg, side = "coronal",aes(fill = Z),color="black")+
  scale_fill_gradient2(low = "blue",mid = "white",high="red",limits = c(-1.25,1.75)) + 
  #labs(title="Regional volume loss") + 
  theme_void()

# merge the plots
p1 = ggarrange(C1,S1,nrow=1,widths=c(2,.5))
p2 = ggarrange(C2,S2,nrow=1,widths=c(2,.5))
p3 = ggarrange(C3,S3,nrow=1,widths=c(2,.5))
Zplot = ggarrange(p1,p2,p3,ncol=1,labels = c("HC","MS","Diff",
                  hjust = c(0,0,0),common.legend = T, legend = "right"))
# save a single figure
ggsave(paste(savepath,"Zplot.pdf",sep=""),plot=Zplot, width = 10, height = 6)

# 1.3 Diagnostic prediction-------- #
