library(splines)
library(brms)
library(ggpubr)
library(here)
library(data.table)
library(sjPlot)
#

models = list(readRDS(here("fits/model_m.rds")),readRDS(here("fits/model_f.rds")))
models_names = c("model_m","model_f")


df = fread(here("data/pina_dataset.csv"))
covars = fread(here("data/covariates.csv"))

df <- df %>% 
  mutate(ess = round(ess),
         trials = ess,
         failure_meet_recs_count = round(ess * fail_meet_recs),
         meet_recs = ess - failure_meet_recs_count,
         perurb = perurb/100,
         malebmi_sc = scale(malebmi),
         femalebmi_sc = scale(femalebmi),
         year = (midyear - 2011)/10,
         regionname_china = ifelse(whoname == "China", "China", regionname),
         qcat = factor(questionnaire_cat,levels=c("GPAQ","IPAQ","other","Eurobarometer 2013-2022")),
         national = as.numeric(coverage == 'national'),
         other = as.numeric(coverage == 'other'),
  ) 

#-----------------------------------------------
# plot coefficients and random effects
#-----------------------------------------------

agecut = c(18,  23.5, 34.5, 44.5, 54.5, 64.5, 74.5, 84)
yearcut = c(seq(2000,2017,5),2017,2020,2025)
covars = fread(here("data/covariates.csv"))

covars2 = NULL
for(i in 1:length(agecut)) {
  covars$midage = agecut[i]
  covars2 = rbind(covars2,covars)
}
covars = covars2
rm(covars2)

covars = covars %>% mutate(
  year = (midyear - 2011)/10,
  coverage = "national", 
  sex = 2,
  perurb = perurb / 100,
  malebmi_sc = scale(malebmi),
  femalebmi_sc = scale(femalebmi),
  national = as.numeric(coverage == 'national'),
  other = as.numeric(coverage == 'other'),
  regionname_china = ifelse(whoname == "China", "China", regionname),
  
)
n = nrow(covars)
covars = rbind(covars,covars)
covars$sex[1:n] = 1

covars$qcat = rep("GPAQ", nrow(covars))
covars$qcat = factor(covars$qcat, levels = c("GPAQ","IPAQ","Eurobarometer 2013-2022", "other"))

df$ageC = cut(df$midage,agecut,include.lowest = T,right=F)
df$yearC = cut(df$midyear,yearcut,include.lowest = T)
covars$ageC = cut(covars$midage,agecut,include.lowest = T,right=F)
covars$yearC = cut(covars$midyear,yearcut,include.lowest = T)

n = length(unique(df$whoname))

covars$meet_recs = 100
covars$failure_meet_recs_count = 100

i = 1  
mod = models[[i]]
s = models_names[i][[1]]
last_char <- substr(s, nchar(s), nchar(s)) 

print('---- Predicting (male) ... ----')

preds_m = predict(mod, newdata=covars[sex == 1],se.fit=TRUE,type="response",allow_new_levels=TRUE,
                  re_formula = ~
                    (-1 + ns(midage, knots = c(30,60))|regionname_china) +
                    (year|iso3) +
                    (year|regionname) +
                    qcat + malebmi_sc) / 200
saveRDS(preds_m, file = paste0("tmp_files/preds_", models_names[i], ".rds"))
draws_m = posterior_linpred(mod, newdata=covars[sex == 1],se.fit=TRUE,type="response",allow_new_levels=TRUE,transform=TRUE,
                            re_formula = ~
                              (-1 + ns(midage, knots = c(30,60))|regionname_china) +
                              (year|iso3) +
                              (year|regionname) +
                              qcat + malebmi_sc)
saveRDS(draws_m, file = paste0("tmp_files/draws_", models_names[i], ".rds"))

covars$yhat = covars$lwr = covars$upr = 0
covars[sex == 1]$yhat = preds_m[,"Estimate"]
covars[sex == 1]$lwr = preds_m[,"Estimate"] - preds_m[,"Est.Error"] * 1.96
covars[sex == 1]$upr = preds_m[,"Estimate"] + preds_m[,"Est.Error"] * 1.96
covars[sex == 1]$lwr = unlist(lapply(covars[sex == 1]$lwr, function(x){max(0, x)}))
covars[sex == 1]$upr = unlist(lapply(covars[sex == 1]$upr, function(x){min(1, x)}))



i = 2
mod = models[[i]]

s = models_names[i][[1]]
last_char <- substr(s, nchar(s), nchar(s)) 

print('---- Predicting (female) ... ----')

preds_f = predict(mod, newdata=covars[sex == 2],se.fit=TRUE,type="response",allow_new_levels=TRUE,
                  re_formula = ~
                    (-1 + ns(midage, knots = c(30,60))|regionname_china) +
                    (year|iso3) +
                    (year|regionname) +
                    qcat + malebmi_sc) / 200
saveRDS(preds_f, file = paste0("tmp_files/preds_", models_names[i], ".rds"))

draws_f = posterior_linpred(mod, newdata=covars[sex == 2],se.fit=TRUE,type="response",allow_new_levels=TRUE,transform=TRUE,
                            re_formula = ~
                              (-1 + ns(midage, knots = c(30,60))|regionname_china) +
                              (year|iso3) +
                              (year|regionname) +
                              qcat + malebmi_sc)
saveRDS(draws_f, file = paste0("tmp_files/draws_", models_names[i], ".rds"))

covars$yhat = covars$lwr = covars$upr = 0
covars[sex == 2]$yhat = preds_m[,"Estimate"]
covars[sex == 2]$lwr = preds_m[,"Estimate"] - preds_m[,"Est.Error"] * 1.96
covars[sex == 2]$upr = preds_m[,"Estimate"] + preds_m[,"Est.Error"] * 1.96
covars[sex == 2]$lwr = unlist(lapply(covars[sex == 2]$lwr, function(x){max(0, x)}))
covars[sex == 2]$upr = unlist(lapply(covars[sex == 2]$upr, function(x){min(1, x)}))

#-----------------------------------------------------------------------------------------
# save CSV with predictions
#-----------------------------------------------------------------------------------------

# use unadjusted data
df = fread(here("data/6Feb2024/5a pina dataset no ipaq adjustment.csv"))
head(df)

df <- df %>% 
  mutate(ess = round(ess),
         trials = ess,
         failure_meet_recs_count = round(ess * fail_meet_recs),
         meet_recs = ess - failure_meet_recs_count,
         perurb = perurb/100,
         malebmi_sc = scale(malebmi),
         femalebmi_sc = scale(femalebmi),
         year = (midyear - 2011)/10,
         qcat = factor(questionnaire_cat,levels=c("GPAQ","IPAQ","other","Eurobarometer 2013-2022")),
         national = as.numeric(coverage == 'national'),
         other = as.numeric(coverage == 'other'),
         regionname_china = ifelse(whoname == "China", "China", regionname),
  ) 


agecut = c(18,  23.5, 34.5, 44.5, 54.5, 64.5, 74.5, 84)
yearcut = c(seq(2000,2017,5),2017,2020,2025)
covars = fread(here("data/covariates.csv"))
covars2 = NULL
for(i in 1:length(agecut)) {
  covars$midage = agecut[i]
  covars2 = rbind(covars2,covars)
}
covars = covars2
rm(covars2)

covars = covars %>% mutate(
  year = (midyear - 2011)/10,
  coverage = "national", 
  sex = 2,
  perurb = perurb / 100,
  malebmi_sc = scale(malebmi),
  femalebmi_sc = scale(femalebmi),
  national = as.numeric(coverage == 'national'),
  other = as.numeric(coverage == 'other'),
  regionname_china = ifelse(whoname == "China", "China", regionname),
  
)
n = nrow(covars)
covars = rbind(covars,covars)
covars$sex[1:n] = 1
names(covars)
table(covars$sex)

covars$survey = NA # from the documentation: to compute population-level predictions for a given grouping variable (i.e., setting all random effects for that grouping variable to zero), set the grouping variable values to NA.

covars$qcat = rep("GPAQ", nrow(covars))
covars$qcat = factor(covars$qcat, levels = c("GPAQ","IPAQ","Eurobarometer 2013-2022", "other"))

covars$coverage = "national"

df$ageC = cut(df$midage,agecut,include.lowest = T,right=F)
df$yearC = cut(df$midyear,yearcut,include.lowest = T)
covars$ageC = cut(covars$midage,agecut,include.lowest = T,right=F)
covars$yearC = cut(covars$midyear,yearcut,include.lowest = T)

n = length(unique(df$whoname))

covars$yhat = covars$lwr = covars$upr = 0

i = 1
preds_m = readRDS(file = paste0("tmp_files/preds_", models_names[i], ".rds")) 

covars[sex == 1]$yhat = preds_m[,"Estimate"]
covars[sex == 1]$lwr = preds_m[,"Estimate"] - preds_m[,"Est.Error"] * 1.96
covars[sex == 1]$upr = preds_m[,"Estimate"] + preds_m[,"Est.Error"] * 1.96
covars[sex == 1]$lwr = unlist(lapply(covars[sex == 1]$lwr, function(x){max(0, x)}))
covars[sex == 1]$upr = unlist(lapply(covars[sex == 1]$upr, function(x){min(1, x)}))
draws_m = readRDS(file = paste0("tmp_files/draws_", models_names[i], ".rds"))

i = 2
preds_f = readRDS(file = paste0("tmp_files/preds_", models_names[i], ".rds")) 
covars[sex == 2]$yhat = preds_f[,"Estimate"]
covars[sex == 2]$lwr = preds_f[,"Estimate"] - preds_f[,"Est.Error"] * 1.96
covars[sex == 2]$upr = preds_f[,"Estimate"] + preds_f[,"Est.Error"] * 1.96
covars[sex == 2]$lwr = unlist(lapply(covars[sex == 2]$lwr, function(x){max(0, x)}))
covars[sex == 2]$upr = unlist(lapply(covars[sex == 2]$upr, function(x){min(1, x)}))

draws_f = readRDS(file = paste0("tmp_files/draws_", models_names[i], ".rds"))

s = models_names[i][[1]]
(mod_name <- substr(s, 1, 3))

write.csv(covars, file = paste0("predictions/preds_", mod_name ,".csv"))

cc = cbind(covars,rbind(t(draws_m),t(draws_f)))

# this is very slow, try write_csv?
fwrite(cc, file = paste0("predictions/draws_", mod_name ,".csv"))
