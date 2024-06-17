
library(data.table)
library(here)
library(ggplot2)
library(dplyr)
library(brms)
options(mc.cores = parallel::detectCores())
library(lme4)
library(glmmTMB)
library(sjPlot)

library(merTools)
library(doParallel)
registerDoParallel(cores=4)
library(ggforce)
library(ggpubr)

library(splines)
library(brms)
sessionInfo()

# use unadjusted data
df = fread(here("data/pina_dataset.csv"))
head(df)

min(df$midage)

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

table(df$coverage)
table(df$national)
table(df$other)

table(df$questionnaire_cat)
table(df$qcat)

max(abs(df$failure_meet_recs_count + df$meet_recs - df$ess))

#-----------------------------------------------
# fit models
#-----------------------------------------------

model_m = brm(meet_recs|trials(meet_recs + failure_meet_recs_count)  ~ 
                  (-1 + ns(midage, knots = c(30,60))|regionname_china) + 
                  (year|regionname) +
                  (year|iso3) +
                  (0+national | survey)+
                  (0+other | survey)+
                  qcat +
                  malebmi_sc,
                  family=binomial(link = "probit"),
                  data=df[sex == 1],
                  control=list(max_treedepth = 18),
                  chains=4,
                  iter=8000,
                  warmup=4000,
                  thin=4)
saveRDS(model_m, "fits/model_m.rds")

model_f = brm( meet_recs|trials(meet_recs + failure_meet_recs_count)  ~ 
                  (-1 + ns(midage, knots = c(30,60))|regionname_china) + 
                  (year|regionname) +
                  (year|iso3) +
                  (0+national | survey)+
                  (0+other | survey)+
                  qcat + 
                  femalebmi_sc,
                  family=binomial(link = "probit"),
                  data=df[sex == 2],
                  control=list(max_treedepth = 18),
                  chains=4,
                  iter=8000,
                  warmup=4000,
                  thin=4)
saveRDS(model_f, "fits/model_f.rds")

