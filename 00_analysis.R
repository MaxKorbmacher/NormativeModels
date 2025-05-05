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
# wash your hands before eating
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.
# define the savepath
savepath = "/Users/max/Documents/Local/MS/NormativeModels/results/"
#
# 0. Data wrangling----------------
# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,psych,effsize,ggseg,patchwork,rstatix,ggpubr,
               caret,lme4,lmerTest,haven,reshape2)
# load data
cross = read.csv("/Users/max/Documents/Local/MS/NormativeModels/code/Zscores.csv")
long = read.csv("/Users/max/Documents/Local/MS/NormativeModels/code/Zscores_long.csv")
demo10 = read.csv('/Users/max/Documents/Local/MS/data/OFAMS88_OFAMS10_lifestylepaper_ updated_beskyttet.csv',sep = ";") # 10 years follow up demo & clin scores
PASAT_OSL = read.csv("/Users/max/Documents/Local/Data/Oslo/Database_cognitive_phenotypes_MS_Oslo.csv")# zscored PASAT
fati = read_sas("/Users/max/Documents/Local/MS/demographics/Statistikk-filer/fatigue.sas7bdat") # fatigue scores
fati_OSL = read.csv("/Users/max/Documents/Local/Data/Oslo/fatigue_Oslo.csv")
# descriptives
cross %>% nrow
length(unique(long %>% na.omit %>% pull(eid)))
length(unique(long %>% filter(data == "MS") %>% na.omit %>% pull(eid)))
long %>% filter(data == "MS") %>% na.omit %>% nrow
length(unique(long %>% filter(data == "OFAMS") %>% na.omit %>% pull(eid)))
long %>% filter(data == "OFAMS") %>% na.omit %>% nrow

# 1. Case-control checks-----------
# 1.1 number of deviations---------
cross$nb_deviations = cross %>%
  select(ends_with("z_score")) %>%
  mutate_all(~ ifelse(abs(.) >= 1.96, 1, 0)) %>%
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

diffs[order(diffs$MS),]

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

# 1.3 Diagnostic prediction--------

# Here, we compare whether the Z scores or regular volumetrics perform better.

# Code to train the SVM 
set.seed(1234) 
# set the 3 fold crossvalidation with AU  
# to pick for us what we call the best model 
control <- trainControl(method="cv",number=3, classProbs = TRUE) 
metric <- "Accuracy"
# model <- train(diagnosis ~., data = cross%>%select(ends_with("z_score"),diagnosis), 
#                method = "svmRadial", 
#                tuneLength = 50,preProc = c("center","scale"),  
#                metric=metric, trControl=control)
# model
# plot(model)
# predict <- predict(model, newdata = cross) 
# confusionMatrix(predict, factor(cross$diagnosis))
# #
# #
# model1 <- train(diagnosis ~., data = cross%>%select(ends_with("volume"),diagnosis), 
#                method = "svmRadial", 
#                tuneLength = 50,preProc = c("center","scale"),  
#                metric=metric, trControl=control)
# model1
# plot(model1)
# predict <- predict(model1, newdata = cross) 
# confusionMatrix(predict, factor(cross$diagnosis))
# #
# #
# # Try again without CV to check whether Z scores are still performing better
# model3 <- train(diagnosis ~., data = cross%>%select(ends_with("z_score"),diagnosis), 
#                method = "svmRadial", 
#                tuneLength = 50,preProc = c("center","scale"),  
#                metric=metric)
# model3
# plot(model3)
# predict <- predict(model3, newdata = cross) 
# confusionMatrix(predict, factor(cross$diagnosis))
# 
# 
# model4 <- train(diagnosis ~., data = cross%>%select(ends_with("volume"),diagnosis), 
#                 method = "svmRadial", 
#                 tuneLength = 50,preProc = c("center","scale"),  
#                 metric=metric)
# model4
# plot(model4)
# predict <- predict(model4, newdata = cross) 
# confusionMatrix(predict, factor(cross$diagnosis))
#
# The comparison is somewhat inconclusive.
# Z-values seem to be better, but since the sample is so small and the group 
# differences so big, it is difficult to say which set of variable is really better.
#
# Hence, we comment this out for now.
#
#
#
#
# 2. Longitudinal assessment------- 
# 2.1 Development of deviations----
# 2.1.1 number of deviations and edss----
long$nb_deviations = long %>%
  select(ends_with("z_score")) %>%
  mutate_all(~ ifelse(abs(.) > 2, 1, 0)) %>%
  transmute(z_score_sum = rowSums(.)) %>%
  pull(z_score_sum)
m = lmer(edss~nb_deviations+age+sex+(1|eid),long)
m = lm(edss~nb_deviations+age+sex,long%>%filter(session == 1))
summary(m)
print("The number of deviations does not tell us something about the disability measured by EDSS.")

# 2.1.2 number of deviations and PASAT----
#
# OSL
PASAT_OSL$eid = gsub("MS","MS_",PASAT_OSL$subject_id)
PASAT_OSL$session = PASAT_OSL$tpoint
PASAT_OSL$PASAT = PASAT_OSL$MACFIMS_PASAT3_zscore
PASAT_OSL = PASAT_OSL %>% select(eid,session,PASAT)
# OFAMS
pasat1 = demo10%>%dplyr::select(Patnr,BL_PASATcorrect,PASAT_24M, PASAT_OFAMS10)
pasat1 = melt(pasat1, id.vars = c("Patnr"))
names(pasat1) = c("eid","session","PASAT")
pasat1$session = ifelse(pasat1$session == "BL_PASATcorrect",1,0)+ifelse(pasat1$session == "PASAT_24M",25,0)+ifelse(pasat1$session == "PASAT_OFAMS10",145,0)

long1 = merge(rbind(PASAT_OSL,pasat1),long,by=c("eid","session"))
nrow(long1)
long1%>%filter(session == 1)%>%nrow
m = lmer(PASAT~nb_deviations+age+sex+TotalGrayVol+(1|eid),long1)
m = lm(PASAT~nb_deviations+age+sex+TotalGrayVol,long1%>%filter(session == 1))
summary(m)
#
# 2.1.3 number of deviations and fatigue----
#
# prep eid and session
fati_OSL$session = substr(fati_OSL$eid,9,11)
fati_OSL$eid = substr(fati_OSL$eid,1,7)
fati_OSL = fati_OSL %>% select(eid,session,fatigue)
fati_OSL$session = as.numeric(fati_OSL$session)
#fati.cop = fati_OSL

# fix session factor levels
fati$session = factor(fati$VISIT)
fati$session = ifelse(fati$session == "Baseline",1,fati$session)
fati$session = ifelse(fati$session == 4,7,fati$session)
fati$session = ifelse(fati$session == 2,13,fati$session)
fati$session = ifelse(fati$session == 3,25,fati$session)
fati$eid = as.numeric(fati$patno)
fati$fatigue = fati %>% select(A,     B,     C,     D,     E,     F,     G,     H,     I) %>% rowMeans()
fati = fati %>% select(eid,session,fatigue)
fati = rbind(fati,fati_OSL)

long2 = merge(fati,long,by=c("eid","session"))
nrow(long2)
long2%>%filter(session == 1) %>% nrow

m = lmer(fatigue~nb_deviations+age+sex+(1|eid),long2)
m = lm(fatigue~nb_deviations+age+sex,long2%>%filter(session == 1))
summary(m)

# 2.1.4 number of deviations and age(ing)----
m = lm(nb_deviations~age+sex+TotalGrayVol,long%>%filter(session == 1))
m = lmer(nb_deviations~age+sex+TotalGrayVol+(1|eid),long)
summary(m)
effectsize::standardize_parameters(m)
#
#
# 2.2 Regional associations ----
# ---- Function to extract standardized coefficients manually
extract_std_coeffs <- function(data, predictor = "age") {
  regions <- data %>% select(ends_with("z_score")) %>% names()
  
  # Standardize predictor
  data <- data %>%
    mutate(std_predictor = scale(.data[[predictor]])[,1])
  
  std_coeffs <- sapply(regions, function(region) {
    # Standardize response
    data <- data %>%
      mutate(std_response = scale(.data[[region]])[,1])
    
    mod <- lmer(std_response ~ std_predictor + (1|eid), data = data)
    fixef(mod)["std_predictor"]
  })
  
  tibble(
    label = str_remove(regions, "_z_score"),
    std_coef = std_coeffs
  )
}

# ---- Function to plot
run_and_plot <- function(long, predictor) {
  coefs <- extract_std_coeffs(long, predictor)
  
  # Cortical
  test_cort <- z_cortical %>%
    filter(group == "MS") %>%
    left_join(coefs, by = "label")
  
  p1 <- ggplot(dk %>% as_tibble() %>%
                 left_join(test_cort %>% select(region, hemi, std_coef), by = c("region", "hemi"))) +
    geom_brain(atlas = dk, aes(fill = std_coef), color = "black") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits = c(-0.35, 0.35)) +
    theme_void() +
    theme(legend.position = "none")
  
  # Subcortical
  test_sub <- z_subcortical %>%
    filter(group == "MS") %>%
    left_join(coefs, by = c("region.x" = "label"))
  
  p2 <- ggplot(test_sub) +
    geom_brain(atlas = aseg, side = "coronal", aes(fill = std_coef), color = "black") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits = c(-0.35, 0.35)) +
    theme_void() +
    labs(fill = "Std.Coeff.")
  
  ggarrange(p1, p2,widths=c(2,.5))
}

# ---- Application of functions
age_plot     <- run_and_plot(long, "age")
edss_plot    <- run_and_plot(long, "edss")
pasat_plot   <- run_and_plot(long1, "PASAT")
fatigue_plot <- run_and_plot(long2, "fatigue")

large_plot = ggarrange(age_plot, edss_plot,
          pasat_plot, fatigue_plot, 
          ncol=1,labels=c("Age","EDSS","PASAT","Fatigue"),
          hjust = c(0,0,0,0))
ggsave(paste(savepath,"Long_Effects.pdf",sep=""),plot=large_plot, width = 10, height = 8)

# Function to also extract standardized coefficients for one predictor

# Helper: Standardize a variable
z <- function(x) {
  if (is.numeric(x)) {
    return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  } else {
    return(x)
  }
}

# Function: Extract standardized coefficients by predictor
run_and_extract <- function(df, predictor) {
  regions <- df %>% select(ends_with("z_score")) %>% names()
  
  # Manually standardize all numeric variables
  df_std <- df %>%
    mutate(across(where(is.numeric), scale))  # z-score standardization
  
  extract_coef <- function(region) {
    tryCatch({
      formula_str <- paste0(region, " ~ ", predictor, " + (1|eid)")
      model <- lmer(as.formula(formula_str), data = df_std)
      coefs <- fixef(model)[predictor]
      tibble(region = gsub("_z_score", "", region),
             predictor = predictor,
             std_coef = as.numeric(coefs))
    }, error = function(e) {
      message("Skipping ", region, " due to error: ", e$message)
      return(NULL)
    })
  }
  
  results <- map_dfr(regions, extract_coef)
  return(results)
}
age_coefs     <- run_and_extract(long,  "age")
edss_coefs    <- run_and_extract(long,  "edss")
pasat_coefs   <- run_and_extract(long1, "PASAT")
fatigue_coefs <- run_and_extract(long2, "fatigue")

all_coefs <- bind_rows(age_coefs, edss_coefs, pasat_coefs, fatigue_coefs)

subcortical_wide <- all_coefs %>%
  pivot_wider(names_from = predictor, values_from = std_coef)
subcortical_wide[2:5] = round(subcortical_wide[2:5],2)
write.csv(x = subcortical_wide,paste(savepath,"long_associations.csv",sep=""),row.names = FALSE)

run_and_extract_pvals <- function(df, predictor) {
  regions <- df %>% select(ends_with("z_score")) %>% names()
  
  # Manually standardize all numeric variables
  df_std <- df %>%
    mutate(across(where(is.numeric), scale))
  
  extract_pval <- function(region) {
    tryCatch({
      formula_str <- paste0(region, " ~ ", predictor, " + (1|eid)")
      model <- lmer(as.formula(formula_str), data = df_std)
      pval <- summary(model)$coefficients[predictor, "Pr(>|t|)"]
      tibble(region = gsub("_z_score", "", region),
             predictor = predictor,
             p_value = as.numeric(pval))
    }, error = function(e) {
      message("Skipping ", region, " due to error: ", e$message)
      return(NULL)
    })
  }
  
  results <- map_dfr(regions, extract_pval)
  return(results)
}
# aplpy functions and put it all together
age_pvals     <- run_and_extract_pvals(long,  "age")
edss_pvals    <- run_and_extract_pvals(long,  "edss")
pasat_pvals   <- run_and_extract_pvals(long1, "PASAT")
fatigue_pvals <- run_and_extract_pvals(long2, "fatigue")
all_pvals <- bind_rows(age_pvals, edss_pvals, pasat_pvals, fatigue_pvals)
#all_pvals$p_value = all_pvals$p_value * nrow(all_pvals) # can be used for Bongferroni correction
all_pvals_wide <- all_pvals %>%
  pivot_wider(names_from = predictor, values_from = p_value)
all_pvals_wide[2:5] = round(all_pvals_wide[2:5],3)
write.csv(x = all_pvals_wide,paste(savepath,"long_associations_pvalues.csv",sep=""),row.names = FALSE)


# Test area -------
#
# Hypothesis driven tests (thalamus and frontal lobe)
m = lmer(edss~Left.Thalamus_z_score+(1|eid),long)
m = lmer(edss~Right.Thalamus_z_score+(1|eid),long)

m = lmer(edss~Left.Thalamus+age+sex+TotalGrayVol+(1|eid),long)
m = lmer(edss~Right.Thalamus+age+sex+TotalGrayVol+(1|eid),long)

m = lm(edss~Left.Thalamus_z_score,long%>%filter(session==1))
m = lm(edss~Right.Thalamus_z_score,long%>%filter(session==1))

m = lm(edss~Left.Thalamus+age+sex+TotalGrayVol,long%>%filter(session==1))
m = lm(edss~Right.Thalamus+age+sex+TotalGrayVol,long%>%filter(session==1))


m = lmer(PASAT~Left.Thalamus_z_score+age+sex+TotalGrayVol+(1|eid),long1)
m = lmer(PASAT~Right.Thalamus_z_score+age+sex+TotalGrayVol+(1|eid),long1)

m = lmer(fatigue~Left.Thalamus_z_score+age+sex+(1|eid),long2)
m = lmer(fatigue~Right.Thalamus_z_score+age+sex+(1|eid),long2)


m = lmer(edss~lh_superiorfrontal_volume_z_score+age+sex+(1|eid),long)
m = lmer(edss~rh_superiorfrontal_volume_z_score+age+sex+(1|eid),long)

m = lmer(edss~lh_superiorfrontal_volume+age+sex+TotalGrayVol+(1|eid),long)
m = lmer(edss~rh_superiorfrontal_volume+age+sex+TotalGrayVol+(1|eid),long)


summary(m)
