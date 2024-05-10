# Normative models of global diffusion / white matter microstructure features
# 9 May 2024, Max Korbmacher (max.korbmacher@gmail.com)
# 
# The script establishes a normative model of a variable of choice.
# This variable needs to be defined in the beginning of the script.
#
#
# load data
df = read.csv("/Users/max/Documents/Projects/LongBAG/data/unscaled/dMRI_train.csv")
testset = read.csv("/Users/max/Documents/Projects/LongBAG/data/unscaled/dMRI_test1.csv")
#
# Note: "df" will be the training data frame from here on. It needs to contain a variable you specify below,
#       as well as "age". So do the pre-processing (before) now or never.
#
# remove EXTREME outliers from training data
df = (remove_sd_outlier(df, n_sigmas = 5))
#
#
# define variable of interest in training set
y = df$mk_Mean
# label for plots
varname = "Mean Kurtosis"
# and test set
y1 = testset$mk_Mean
#
#
#
# load packages
library(cNORM)
library(ggplot2)
library(ggpubr)
library(dataPreparation)
#
# estimate model
model = cnorm(raw = y, age = df$age,width = 1)
# inspect model fits
printSubset(model)
# graphical representation of change in R adj deoendent on nb of predictors
# (Radjusted2 as a function of the number of predictors is default)
# Mallow’s \(C_p\) (type = 1) and \(BIC\) (type = 2) as a function of \(R_{adjusted}^{2}\) or RMSE (type = 3)
plot(model, "subset", type = 0) 
# cross-validation
cnorm.cv(model$data, max=10, repetitions = 2)
# Plots the fitted and the manifest percentiles
plot(model, "percentiles")
#
# if in doubt, display several models
plot(model, "series", end = 10)
#
# In the next figure, the fitted and the manifest data are compared separately for each (age) group:
plot(model, "raw")
# we can do the same plotting logic but for norm values
plot(model, "norm")
# check for bijectivity violations
# we can check e.g. for specified age range: minAge=18, maxAge=100
# in other words, this displayes where the model is expected to fail, when extending the age range
# (negative values will indicate that the model fails)
# my rule of thumb: min/max ± 10 years
plot(model, "derivative")
# plot(model, "derivative", minAge = 35, maxAge = 95, minNorm=min(y), maxNorm=max(y))
# plot(model, "derivative", minAge=35, maxAge=95, minNorm=20, maxNorm=80)
# 
# plot(model, "derivative", minNorm=20, maxNorm=80, minAge=(min(df$age)), maxAge=(max(df$age)))
# 
# plot(model, "derivative", minNorm=min(y), maxNorm=max(y))
#
# show normative curves
plot(model, "curves", normList = c(20, 30, 40, 50, 60, 70, 80), minAge = min(df$age), 
     maxAge = max(df$age), step = 0.1, minRaw = min(y), maxRaw = max(y))
# we can now predict values
## for example at the 20% and 80% percentiles (and in the middle)
predictRaw(c(-40,0,40), c(35, 45,55,65,75,85,95), model)
## or directly onto the data, assuming that we include adults ± 10 years of the training
predictNorm(y1, testset$age, model, minNorm = 35, maxNorm=95, force = T)
## or assuming that the sample has the same age distribution as the training data.
predictNorm(y1, testset$age, model, force = T)
#
#
# Estimate predictions (potentially interesting for plotting)
testset$predictions = predictNorm(y1, testset$age, model)
testset$normalized_predictions = (predictNorm(y1, testset$age, model)-mean(df$age))/sqrt(sd(df$age)^2)
testset$normalized_scores = ((y1)-mean(y))/sqrt(sd(y)^2)
df$normalized_scores = scale(y)
#df$normalized_predictions = (predictNorm(y, df$age, model)-mean(df$age))/sqrt(sd(df$age)^2)
#training_predictions = predictNorm(y, df$age, model)
#
##
# DEVIATIONS ARE BASED ON TRUE-PREDICTED / SQRT(SD(TRAIN)^2)
# see Fig2c in Marquant et al.: https://doi.org/10.1038/s41380-019-0441-1
##
# Estimate deviations for training set
training_deviation = (df$age - (predictNorm(y, df$age, model)))/abs(sd(df$age))
# Estimate deviations for test set
testset$deviation = ((testset$age - predictNorm(y1, testset$age, model)))/sqrt(sd(df$age)^2+sd(testset$age)^2)

# Make plots showing the test data in the normative chart
## we make a function to estimate and normalise the curves/percentiles of interest (e.g., 20%, 50%, and 80% percentiles)
get_lines <- function(QUANTILE) {
  # estimate curves based on cNORM
  check = plot(model, "curves", normList = QUANTILE, minAge = min(df$age), maxAge = max(df$age), step = .1) #(max(df$age)-min(df$age))/100)
  # then get at the normalized score
  yy = (check$panel.args[[1]]$y-mean(y))/sd(y)
  return (yy)
}
check = plot(model, "curves", normList = .8, minAge = min(df$age), maxAge = max(df$age), step = 0.1)

# here we can estimate the desired quantile
low = get_lines(20)
middle = get_lines(50)
high = get_lines(80)
plot_dat = data.frame(check$panel.args[[1]]$x,low,middle,high)
names(plot_dat) = c("age","low","middle","high")
# Now, we can plot the estimated scores from the test set
p2 = ggplot(testset, aes(age, normalized_scores)) +
  geom_point(shape = 16, size = 2,color = "blue", show.legend = F, alpha = .1) +
  geom_line(plot_dat, mapping = aes(age, low)) +
  geom_line(plot_dat, mapping = aes(age, middle)) +
  geom_line(plot_dat, mapping = aes(age, high)) +
  ylab(paste("Normalized",varname)) + xlab("Age")+
  theme_bw() + ylim(-7.5,7.5) #+ scale_color_gradient(low = "blue", high = "red")
# as a control, we can also check how the training data align
p1 = ggplot(df, aes(age, normalized_scores)) +
  geom_point(shape = 16, size = 2, color = "blue", show.legend = F, alpha = .05) +
  geom_line(plot_dat, mapping = aes(age, low)) +
  geom_line(plot_dat, mapping = aes(age, middle)) +
  geom_line(plot_dat, mapping = aes(age, high)) +
  ylab(paste("Normalized",varname)) + xlab("Age")+
  theme_bw() + ylim(-7.5,7.5)#+ scale_color_gradient(low = "blue", high = "red")
ggarrange(p1,p2,labels = c("a", "b")) # a) Training data, b) Test data

# the only thing left is to make the model ready for export, so that it can be shared without information leakage (following the ethics guidelines from tsd)