#script for comparing congruency effect in Experiments 1 and 2
library (lme4)
library (lmerTest)
library (emmeans)
library (pbkrtest)
library (readr)

BothExptsRT <- read_csv("~/BothExptsRT.csv")

BothExptsRT$Congruency <- as.factor (BothExptsRT$Congruency)
BothExptsRT$Experiment <- as.factor (BothExptsRT$Experiment)

contrasts (BothExptsRT$Congruency) <- matrix (c(.5, -.5)) 
contrasts (BothExptsRT$Experiment) <- matrix (c(.5, -.5)) 

#datafile relabelled so 2 levels of Congruency (Congruent vs Incongruent) to allow cross experiment comparision of Congruency magnitude

#check to see whether the interaction between Congruency and Experiment is significant (it is)
model <- lmer (RT~ Congruency*Experiment + (1+Congruency|Subject) + (1+Congruency|Vignette) + (1+Congruency|Face), data=BothExptsRT, REML=TRUE)
model.null <- lmer (RT~ Congruency + (1+Congruency|Subject) + (1+Congruency|Vignette) + (1+Congruency|Face), data=BothExptsRT, REML=TRUE)
anova (model, model.null)
summary (model)
emmeans (model, pairwise~Congruency*Experiment, adjust="none" )
