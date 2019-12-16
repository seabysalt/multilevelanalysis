################################ Mulilevel Analysis ################################

################################# General Setup ###################################

library(easypackages)
libraries("Hmisc", "psych", "lme4", "texreg", "sjPlot", "sjmisc", "sjstats", 
          "haven", "ggplot2", "effects", "tidyverse", "ggExtra", "ggeffects", "reghelper")

setwd("~/Documents/Uni Konstanz/Master/Master Thesis/Daten/Multilevel/New")

#################################### Importing my Data ################################

dat <- read.csv("/Users/sarahalt/Desktop/Daten_Masterarbeit_Multilevel_GMC2_Sarah.csv")

attach(dat)

##factoring the categorial variables

dat$sex_m = factor(dat$sex_m,
                   levels = c(1,2),
                   labels = c("Female", "Male"))
dat$startup_m = factor(dat$startup_m,
                       levels = c(1,2),
                       labels = c("Startup", "No-Startup"))

################################### Data Screening ####################################

## checking for accuracy & missings ##

summary(dat)

########################################################################################################### 
###################################### Basic Models #######################################################
###########################################################################################################

## Null Model - Intercept only
h_null <- lmer(PSS_mean ~ 1 + (1 | teamcode), data = dat)

## Model 1: Model with control variables
h_1 <- lmer(PSS_mean ~ 1 + sex_m + Age_gmc + startup_m + (1 | teamcode), data = dat)

########################################################################################################### 
###################################### Individual PsyCap #######################################################
###########################################################################################################

## Model 2: Model with predictor iPsyCap - grand-mean-centered - deleted from analysis
h2_c_i <- lmer(PSS_mean ~ PCI_mean_gmc + sex_m + Age_gmc + startup_m + (1 | teamcode), data = dat)

## Model 3: Model with predictor WLoad - grand-mean-centered 
h3_c_i <- lmer(PSS_mean ~ WLoad_mean_gmc + sex_m + Age_gmc + startup_m + (1 | teamcode), data = dat)

## Model 4: Model with predictors WLoad & iPsyCap - grand-mean-centered
h4_c_i <- lmer(PSS_mean ~ WLoad_mean_gmc + PCI_mean_gmc + sex_m + Age_gmc + startup_m + (1 | teamcode), data = dat)

## Model 5: Model with interaction - random intercept, fixed slope - grand-mean-centered
h5_c_i <- lmer(PSS_mean ~ WLoad_mean_gmc * PCI_mean_gmc + sex_m + Age_gmc + startup_m + (1 | teamcode), data = dat)

## Model 6: Model with interaction - random intercept, random slope - grand-mean-centered
h6_c_i <- lmer(PSS_mean ~ WLoad_mean_gmc * PCI_mean_gmc + sex_m + Age_gmc + startup_m + (WLoad_mean_gmc | teamcode), data = dat)

## T-Values
summary(h_null)
summary(h_1)
summary(h2_c_i)
summary(h3_c_i)
summary(h4_c_i)
summary(h5_c_i)
summary(h6_c_i)

###### Comparison of models ####

## Direct Relationhip ##
screenreg(list(h_null, h_1, h2_c_i))
anova(h_null, h_1, h2_c_i)

## Moderation
screenreg(list(h_null, h_1, h3_c_i, h4_c_i, h5_c_i, h6_c_i))
anova(h_null, h_1, h3_c_i, h4_c_i, h5_c_i, h6_c_i)

##html overview for Word
htmlreg(list(h_null, h_1, h2_c_i, h3_c_i, h4_c_i, h5_c_i, h6_c_i), 
        file = "iPsyCap-Models.html",
        single.row = T, 
        caption = "The role of individual psychological capital in regard to perceived stress if controlled for gender, startup & age",
        custom.note = "
        %stars.
        Null Model = Intercept only model.
        Model 1 = Model with control variables.
        Model 2 = Model with predictor iPsyCap.
        Model 3 = Model with predictor workload.
        Model 4 = Model with both predictors.
        Model 5 = Random Intercept & Fixed Slope, with interaction.
        Model 6 = Random Intercept & Random Slope, with interaction;
        All predictors are grand-mean-centered; Controlled for Age, Startup & Gender",
        custom.model.names = c("Null Model", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"))

########################################################################################################### 
###################################### Team PsyCap #######################################################
###########################################################################################################

## Model 2: Model with predictor tPsyCap - grand-mean-centered - deleted
h2_c_t <- lmer(PSS_mean ~ PCT_mean_gmc + sex_m + Age_gmc + startup_m + (1 | teamcode), data = dat)

## Model 3: Model with predictor WLoad - grand-mean-centered
h3_c_t <- lmer(PSS_mean ~ WLoad_mean_gmc + sex_m + Age_gmc + startup_m + (1 | teamcode), data = dat)

## Model 4: Model with predictors WLoad & tPsyCap - grand-mean-centered
h4_c_t <- lmer(PSS_mean ~ WLoad_mean_gmc + PCT_mean_gmc + sex_m + Age_gmc + startup_m + (1 | teamcode), data = dat)

## Model 5: Model with interaction - random intercept, fixed slope - grand-mean-centered
h5_c_t <- lmer(PSS_mean ~ WLoad_mean_gmc * PCT_mean_gmc + sex_m + Age_gmc + startup_m + (1 | teamcode), data = dat)

## Model 6: Model with interaction - random intercept, random slope - grand-mean-centered
h6_c_t <- lmer(PSS_mean ~ WLoad_mean_gmc * PCT_mean_gmc + sex_m + Age_gmc + startup_m + (WLoad_mean_gmc | teamcode), data = dat)

## T-Values
summary(h_null)
summary(h_1)
summary(h2_c_t)
summary(h3_c_t)
summary(h4_c_t)
summary(h5_c_t)
summary(h6_c_t)

###### Comparison of models ####

## Direct Relationhip ##
screenreg(list(h_null, h_1, h2_c_t))
anova(h_null, h_1, h2_c_t)

## Moderation
screenreg(list(h_null, h_1, h3_c_t, h4_c_t, h5_c_t, h6_c_t))
anova(h_null, h_1, h3_c_t, h4_c_t, h5_c_t, h6_c_t)

#probing the interaction
if (require(lme4, quietly=TRUE)) {
  model <- lmer(Sepal.Width ~ Sepal.Length * Petal.Length + (1|Species), data=iris)
  interaction <- lmer(PSS_mean ~ WLoad_mean_gmc * PCT_mean_gmc + sex_m + Age_gmc + startup_m + (1 | teamcode), data = dat)
  summary(interaction)
  simple_slopes(interaction)
  simple_slopes(interaction,
                levels=list(WLoad_mean_gmc=c(4, 5, 6, 'sstest'),
                            PCT_mean_gmc=c(2, 3, 'sstest')))  # test at specific levels
}

##html overview for Word
htmlreg(list(h_null, h_1, h2_c_t, h3_c_t, h4_c_t, h5_c_t, h6_c_t), 
        file = "tPsyCap-Models.html",
        single.row = T, 
        caption = "The role of team psychological capital in regard to perceived stress if controlled for gender, startup & age",
        custom.note = "
        %stars.
        Null Model = Intercept only model.
        Model 1 = Model with control variables.
        Model 2 = Model with predictor tPsyCap.
        Model 3 = Model with predictor workload.
        Model 4 = Model with both predictors.
        Model 5 = Random Intercept & Fixed Slope, with interaction.
        Model 6 = Random Intercept & Random Slope, with interaction;
        All predictors are grand-mean-centered; Controlled for Age, Startup & Gender",
        custom.model.names = c("Null Model", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"))

###### Plotting ####

Interaktion

plot(PSS_mean,WLoad_mean_gmc)
plot(PSS_mean,PCT_mean_gmc)
plot(WLoad_mean_gmc,PCT_mean_gmc)
plot(PSS_mean,PCT_mean_gmc*WLoad_mean_gmc)

x <- ggpredict(h5_c_t, c("WLoad_mean_gmc", "PCT_mean_gmc"))
x
plot(x)

h5_c_t <- lmer(PSS_mean ~ WLoad_mean_gmc * PCT_mean_gmc + sex_m + Age_gmc + startup_m + (1 | teamcode), data = dat)

eff.h5_c_t <- effect("WLoad_mean_gmc*PCT_mean_gmc", h5_c_t, KR=T)
eff.h5_c_t <- as.data.frame(eff.h5_c_t)
ggplot(eff.h5_c_t, aes(PCT_mean_gmc, linetype=factor(WLoad_mean_gmc),
                        color = factor(WLoad_mean_gmc))) +
  geom_line(aes(y = fit, group=factor(WLoad_mean_gmc)), size=1.2) +
  geom_line(aes(y = lower,
                group=factor(WLoad_mean_gmc)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(WLoad_mean_gmc)), linetype =3) +
  xlab("team PsyCap") +
  ylab("Effects on Perceived Stress") +
  scale_colour_discrete("") +
  scale_linetype_discrete("") +
  labs(color='WLoad_mean_gmc') + theme_minimal()

plot_model(h5_c_t, type = "int", terms = c(PCT_mean_gmc,WLoad_mean_gmc), ci.lvl = 0.95)


## Further Test ##

h_test <- lmer(PSS_mean ~ WLoad_mean_gmc + PCI_mean_gmc + PCT_mean_gmc + sex_m + Age_gmc + startup_m + (1 | teamcode), data = dat)

summary(h_test)
screenreg(list(h_null, h_1, h2_c_t, h_test))

#################################### Finish ################################



##detach
detach(dat)