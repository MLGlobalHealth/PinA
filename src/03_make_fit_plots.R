library(splines)
library(brms)
library(ggpubr)
library(here)
library(ggforce)
library(data.table)
library(sjPlot)


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

covars$national = NA
covars$other = NA

df$ageC = cut(df$midage,agecut,include.lowest = T,right=F)
df$yearC = cut(df$midyear,yearcut,include.lowest = T)
covars$ageC = cut(covars$midage,agecut,include.lowest = T,right=F)
covars$yearC = cut(covars$midyear,yearcut,include.lowest = T)

n = length(unique(df$whoname))


for (i in 1:length(models)){
  
  print(paste0("-------------- Plotting for model ", models_names[[i]], " ------------------"))
  mod <- models[[i]]
  
  print('---- Fixed effects ----')
  # fixed effects
  pdf(here(paste0("figures/", Sys.Date(), "_", models_names[i], "_fixed_effects.pdf")),width=12,height=8)
  p <- plot_model(mod, vline.color = "red", title="Fixed effects", show.values=T)
  print(p)
  dev.off()
  
  # This one is illegible
  print('---- Random effects ----')
  # (age1 + age2 + year|regionname)
  pdf(here(paste0("figures/", Sys.Date(), "_", models_names[i], "_rf_age_year_region.pdf")),width=100,height=8)
  p <- plot_model(mod, type="re", ri.nr=1, title="Random effect (ns(age) + year|regionname)", show.values=T)
  print(p)
  dev.off()
 
  # #-----------------------------------------------
  # # make predictions plot year trends by country
  # #-----------------------------------------------
  # 
  s = models_names[i][[1]]
  last_char <- substr(s, nchar(s), nchar(s)) 
  
  if (last_char == "m"){
    print('---- Predicting ... ----')
    
    #The re.form = ~0 argument in the predict function specifies that the predictions should include the random effects. 
    # re.form = NA
    
    preds_m = readRDS(file = paste0("tmp_files/preds_", models_names[i], ".rds"))
    
    covars$yhat = covars$lwr = covars$upr = 0
    covars[sex == 1]$yhat = preds_m[,"Estimate"]
    covars[sex == 1]$lwr = preds_m[,"Q2.5"]
    covars[sex == 1]$upr = preds_m[,"Q97.5"]
    covars[sex == 1]$lwr = unlist(lapply(covars[sex == 1]$lwr, function(x){max(0, x)}))
    covars[sex == 1]$upr = unlist(lapply(covars[sex == 1]$upr, function(x){min(1, x)}))
    
    
  } else {
    print('---- Predicting ... ----')
    
    preds_f = readRDS(file = paste0("tmp_files/preds_", models_names[i], ".rds"))
    
    covars$yhat = covars$lwr = covars$upr = 0
    covars[sex == 2]$yhat = preds_f[,"Estimate"]
    covars[sex == 2]$lwr = preds_f[,"Q2.5"]
    covars[sex == 2]$upr = preds_f[,"Q97.5"]
    covars[sex == 2]$lwr = unlist(lapply(covars[sex == 2]$lwr, function(x){max(0, x)}))
    covars[sex == 2]$upr = unlist(lapply(covars[sex == 2]$upr, function(x){min(1, x)}))
    
  }
  
  print('---- Year trends ----')
  
  regnames <- unique(df[,c('iso3','whoname', 'regionname')])
  
  pdf(here(paste0("figures/", Sys.Date(), "_", models_names[i],"_year-trends.pdf")),width=12,height=8)
  for(j in 1:length(unique(df$iso3))) {
    iso3_j <- unique(df$iso3)[j]
    regionname <- df$regionname[which(df$iso3 == iso3_j)][1]
    whoname_j <- regnames$whoname[which(regnames$iso3 == iso3_j)]
    s = models_names[i][[1]]
    last_char <- substr(s, nchar(s), nchar(s)) 
    
    if (last_char == "m"){
      covars.ss = covars[midage <= 80 & sex == 1 & iso3 == iso3_j]   
      g = ggplot(df[sex == 1 & iso3 == iso3_j], aes(x=midyear, y=1-fail_meet_recs))
    } else {
      covars.ss = covars[midage <= 80 & sex == 2 & whoname == whoname_j]   
      g = ggplot(df[sex == 2 & iso3 == iso3_j], aes(x=midyear, y=1-fail_meet_recs))
    }
    
    g = g + geom_point(aes(size=ess,shape=coverage, colour=questionnaire_cat))
    g = g + geom_line(data=covars.ss,aes(y=yhat)) # is the <= 70 right? TODO: fix this to be something that is closer to what the data is. currently there's usually two points, 70 and 80 (?)
    g = g + geom_ribbon(data=covars.ss,aes(y=yhat,ymin=lwr,ymax=upr),alpha=.2)
    g = g + facet_wrap_paginate(~ ageC,nrow=3,ncol=3)
    g = g + scale_y_continuous("% meeting recommendations",labels = scales::percent,limits=c(0,1)) + 
      xlab("Year")
    g = g + ggtitle(paste0(whoname_j, ", ", regionname))
    print(g)
  }
  dev.off()
  
  # age trends 
  print('---- Age trends ----')
  pdf(here(paste0("figures/", Sys.Date(), "_", models_names[i],"_age-trends.pdf")),width=12,height=8)
  reg_names <- unique(df$regionname)
  for(reg in reg_names) {
    print(reg)
    iso3_reg <- as.data.frame(unique(df[which(df$regionname == reg), "iso3"]))
    whonames_reg <- c()
    for (kk in 1:dim(iso3_reg)[1]){
      whonames_reg <- c(whonames_reg, regnames$whoname[which(regnames$iso3 == iso3_reg[kk,1])])
    }
    
    for(j in 1:dim(iso3_reg)[1]){
      iso3_reg_j <- iso3_reg[j,1]
      whoname_reg <- whonames_reg[j]
      s = models_names[i][[1]]
      last_char <- substr(s, nchar(s), nchar(s)) 
      
      if (last_char == "m"){
        covars.ss = covars[midyear %in% (yearcut + 2) & sex == 1  & regionname == reg & iso3 == iso3_reg_j]
        g = ggplot(df[sex == 1 & regionname == reg & iso3 == iso3_reg_j], aes(x=midage, y=1-fail_meet_recs))
      } else {
        covars.ss = covars[midyear %in% (yearcut + 2) & sex == 2  & regionname == reg & iso3 == iso3_reg_j]
        g = ggplot(df[sex == 2 & regionname == reg & iso3 == iso3_reg_j ], aes(x=midage, y=1-fail_meet_recs))
      }
      
      g = g + geom_point(aes(size=ess,shape=coverage, colour=questionnaire_cat))
      g = g + geom_line(data=covars.ss,aes(y=yhat))
      g = g + geom_ribbon(data=covars.ss,aes(y=yhat,ymin=lwr,ymax=upr),alpha=.2)
      g = g + facet_wrap(~ yearC,nrow=2,ncol=3)
      g = g + scale_y_continuous("% meeting recommendations",labels = scales::percent,
                                 limits=c(0,1)) +   xlab("Age")
      g = g + ggtitle(paste0(reg, ", ", whoname_reg))
      print(g)
    }
  }
  dev.off()
  
}
