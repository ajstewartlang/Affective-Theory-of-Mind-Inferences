#script for comparing congruency effect in Experiments 1 and 2
library (lme4)
library (lmerTest)
library (lsmeans)
library (pbkrtest)
library (readr)
library (ggplot2)

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
lsmeans (model, pairwise~Congruency*Experiment, adjust="none" )

#calculate Bayes Factor
Bayes1 <- lmBF (RT ~ Congruency*Experiment, as.data.frame(BothExptsRT), whichRandom = list ("Subject", "Vignette", "Face"))
Bayes1

Bayes2 <- lmBF (RT ~ Congruency, as.data.frame(BothExptsRT), whichRandom = list ("Subject", "Vignette", "Face"))
Bayes2

#graphing the means and SEs
compdata <- read_csv("~/Exptcompgraph.csv")
compdata$Experiment <- as.factor (compdata$Experiment)

p <- ggplot(compdata, aes(x=Congruency, y=Mean, group = Experiment, colour = Experiment, ymin = 1000, ymax=3000))  + geom_line() + geom_point() + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) + labs (y="RT in ms", x= "Congruency", colour="Experiment")
p <- p + scale_colour_grey(start = 0, end = .7) + theme_bw()
p <- p + theme(text = element_text(size = 20))
p