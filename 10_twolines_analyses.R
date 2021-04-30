#Two-line test app 0.52
# Last update 2018 11 23
#	Written by Uri Simonsohn (urisohn@gmail.com)
#	This is the exact code behind the two-line online app (http://webstimate.org/twolines/)
#	If you see any errors or have questions people contact me directly.
#
####################################################################################
#

#WARNING: THIS WILL INSTALL PACKAGES IF YOU DON'T HAVE THEM
list.of.packages <- c("mgcv", "stringr","sandwich","lmtest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


rm(list=ls())  # clean it all
library(mgcv)         #This library has the additive model with smoothing function
library(stringr)      #To process strings in the function
library(sandwich)     #For robust standard errors
library(lmtest)       #To run linear tests





data_included_documented = read.csv(file = "data_included_documented.csv")[,-1]

#3) Run analyses
#3.1 Testing if x1 has u-shaped effect on y
twolines_polsim = twolines(pref_politicalsim ~ political_orientation,
                           data=data_included_documented)

model_polsim_lin = lm(pref_politicalsim ~ political_orientation,
                  data = data_included_documented)
summary(model_polsim_lin)

model_polsim_lin_quad = lm(pref_politicalsim ~ I(political_orientation^2),
                      data = data_included_documented)
summary(model_polsim_lin_quad)

# 4) z-standardize outcome
library(tidyverse)
data_included_documented = data_included_documented %>%
  group_by(country) %>%
  mutate(pref_politicalsim_z = scale(pref_politicalsim, center = T, scale = T)) %>%
  ungroup()

data_included_documented = data_included_documented %>%
  mutate(political_orientation_z = scale(political_orientation, center = T, scale = T))

data_included_documented$political_orientation_z = c(data_included_documented$political_orientation_z)

twolines_polsim = twolines(pref_politicalsim_z ~ political_orientation_z,
                           data=data_included_documented)
