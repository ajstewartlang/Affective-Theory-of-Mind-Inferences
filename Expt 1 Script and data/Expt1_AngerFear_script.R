library (lme4)
library (lmerTest)
library (emmeans)
library (pbkrtest)
library (readr)
library (ggplot2)

#script for AngerFear RT and accuracy data analysis with arousal

#this is the analysis of the RT data
AngerFearRT <- read_csv("~/AngerFearRT.csv")

AngerFearRT$StoryEmotion <- as.factor (AngerFearRT$StoryEmotion)
AngerFearRT$FaceExpression <- as.factor (AngerFearRT$FaceExpression)

contrasts (AngerFearRT$StoryEmotion) <- matrix (c(.5, -.5)) 
contrasts (AngerFearRT$FaceExpression) <- matrix (c(.5, -.5)) 

#with Subject, Vignette, and Face as crossed random effects with arousal
#full model does not converge so need to drop interaction term from the random effects - in addition, Face random effect has only random intercept and slope for FaceExpression
#modelRTAr1 <- lmer (RT ~ StoryEmotion*FaceExpression*Arousal + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) + (1+FaceExpression|Face), data=AngerFearRT, REML=TRUE)
summary (modelRTAr1)
modelRT <- lmer (RT ~ StoryEmotion*FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) + (1+FaceExpression|Face), data=AngerFearRT, REML=TRUE)
#anova (modelRTAr1, modelRT)

#add covariate as first factor
#modelRTAr1 <- lmer (RT ~ Arousal + StoryEmotion*FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) + (1+FaceExpression|Face), data=AngerFearRT, REML=TRUE)
#anova (modelRTAr1, modelRT)
#summary (modelRTAr1)

modelRTAr1.b <- lmer (RT ~ Arousal + StoryEmotion*FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) + (1+FaceExpression|Face) + (0+Arousal|Face), data=AngerFearRT, REML=TRUE)
anova (modelRTAr1.b, modelRT)
summary (modelRTAr1.b)

#difference between models not signif - arousal does not interact with effect so drop arousal from subsequent analysis
modelRTnull <- lmer (RT ~  (1+StoryEmotion+FaceExpression|Subject) + + (1+StoryEmotion+FaceExpression|Vignette) + (1+FaceExpression|Face), data=AngerFearRT, REML=TRUE)
anova (modelRT, modelRTnull)
summary (modelRT)
emmeans (modelRT, pairwise~StoryEmotion*FaceExpression, adjust="none")

#calculate Bayes Factor
Bayes <- lmBF (RT ~ StoryEmotion*FaceExpression, as.data.frame(AngerFearRT), whichRandom = list ("Subject", "Vignette", "Face"))
Bayes

#graphing the means and SEs
AngerFeargraphdata <- read_csv("~/AngerFeargraphdata.csv")
p <- ggplot(AngerFeargraphdata, aes(x=StoryEmotion, y=Mean, group = FaceEmotion, colour = FaceEmotion, ymin = 1000)) + geom_line() + geom_point() + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) + labs (y="RT in ms", x= "Vignette Emotion", colour="Facial Emotion")
p <- p + scale_colour_grey(start = 0, end = .7) + theme_bw()
p <- p + theme(text = element_text(size = 20))
p

#this is the analysis of the accuracy data
AngerFearAcc <- read_csv("~/AngerFearAcc.csv")

AngerFearAcc$StoryEmotion <- as.factor (AngerFearAcc$StoryEmotion)
AngerFearAcc$FaceExpression <- as.factor (AngerFearAcc$FaceExpression)

contrasts (AngerFearAcc$StoryEmotion)<-matrix (c(.5, -.5)) 
contrasts (AngerFearAcc$FaceExpression)<-matrix (c(.5, -.5)) 

#full model does not converge - most complex is with random intercepts
modelAcc <- glmer (Accuracy ~ StoryEmotion*FaceExpression + (1|Subject) + (1|Vignette) , data=AngerFearAcc, family=binomial)
emmeans (modelAcc, pairwise~StoryEmotion*FaceExpression, adjust="none", type="response")
modelAcc.null <- glmer (Accuracy ~  (1|Subject) + (1|Vignette) , data=AngerFearAcc, family=binomial)
anova (modelAcc, modelAcc.null)
summary (modelAcc)

#graphing the means and SEs
AngerFeargraphacc <- read_csv("~/AngerFeargraphacc.csv")
p <- ggplot(AngerFeargraphacc, aes(x=StoryEmotion, y=Mean, group = FaceEmotion, colour = FaceEmotion, ymin = 80)) + geom_line() + geom_point() + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) + labs (y="% Correct", x= "Vignette Emotion", colour="Facial Emotion")
p <- p + scale_colour_grey(start = 0, end = .7) + theme_bw()
p <- p + theme(text = element_text(size = 20))
p

#some plots
index <- AngerFearRT$Vignette=="17"
ggplot (AngerFearRT[index,], aes (StoryEmotion, RT, colour=FaceExpression)) + ylim(0,3000) 

ggplot (AngerFearRT, aes (StoryEmotion, RT, colour=FaceExpression)) + ylim(0,3000) + geom_point() + facet_wrap(~AngerFearRT$Vignette) + geom_jitter()


