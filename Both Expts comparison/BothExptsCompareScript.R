#script for comparing congruency effect in Experiments 1 and 2
library (lme4)
library (lmerTest)
library (lsmeans)
library (pbkrtest)
library (readr)

BothExptsRT <- read_csv("~/BothExptsRT.csv")

BothExptsRT$Congruency <- as.factor (BothExptsRT$Congruency)
BothExptsRT$Experiment <- as.factor (BothExptsRT$Experiment)
BothExptsRT$KDEFfaces <- as.factor (BothExptsRT$KDEFfaces)

contrasts (BothExptsRT$Congruency)<-matrix (c(.5, -.5)) 
contrasts (BothExptsRT$Experiment)<-matrix (c(.5, -.5)) 

#datafile relabelled so 2 levels of Congruency (Congruent vs Incongruent) to allow cross experiment comparision of Congruency magnitude
#in datafile Trials corresponds to Vignettes, and KDEFfaces corresponds to Faces

#check to see whether the interaction between Congruency and Experiment is significant (it is). Maximal random effects structure converges.
model <- lmer (RT~ Congruency*Experiment + (1+Congruency|Subject) + (1+Congruency|Trials) + (1+Congruency|KDEFfaces), data=BothExptsRT, REML=TRUE)
model.null <- lmer (RT~ Congruency + (1+Congruency|Subject) + (1+Congruency|Trials) + (1+Congruency|KDEFfaces), data=BothExptsRT, REML=TRUE)
anova (model, model.null)
summary (model)
lsmeans (model, pairwise~Congruency*Experiment, adjust="none" )

#graphing the means and SEs
compdata <- read_csv("~/Exptcompgraph.csv")
compdata$Experiment <- as.factor (compdata$Experiment)

p <- ggplot(compdata, aes(x=Congruency, y=Mean, group = Experiment, colour = Experiment, ymin = 1000, ymax=3000))  + geom_line() + geom_point() + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) + labs (y="RT in ms", x= "Congruency", colour="Experiment")
p <- p + scale_colour_grey(start = 0, end = .7) + theme_bw()
p