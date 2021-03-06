library (lme4)
library (lmerTest)
library (lsmeans)
library (pbkrtest)
library (readr)
library (ggplot2)

#script for HappySad RT and accuracy data analysis

#this is the analysis of the RT data

HappySadRT <- read_csv("~/HappySadRT.csv")

HappySadRT$StoryEmotion <- as.factor (HappySadRT$StoryEmotion)
HappySadRT$FaceExpression <- as.factor (HappySadRT$FaceExpression)

contrasts (HappySadRT$StoryEmotion) <- matrix (c(.5, -.5)) 
contrasts (HappySadRT$FaceExpression) <- matrix (c(.5, -.5)) 


#this model is with crossed random effects - full model does not converge but the following one does (dropping the interaciton term from the final random effect)
modelRT <- lmer (RT ~ StoryEmotion*FaceExpression + (1+StoryEmotion*FaceExpression|Subject) + (1+StoryEmotion*FaceExpression|Vignette) + (1+StoryEmotion+FaceExpression|Face), data=HappySadRT, REML=TRUE)
modelRT.null <- lmer (RT ~ (1+StoryEmotion*FaceExpression|Subject) + (1+StoryEmotion*FaceExpression|Vignette) + (1+StoryEmotion+FaceExpression|Face), data=HappySadRT, REML=TRUE)
anova (modelRT, modelRT.null)
summary (modelRT)
lsmeans (modelRT, pairwise~StoryEmotion*FaceExpression, adjust="none")

#the below is to allow us to calcaulte Bayes Factors
modelRT <- lmer (RT ~ StoryEmotion*FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) + (1+FaceExpression||Face), data=HappySadRT, REML=TRUE)
modelRT.null <- lmer (RT ~ (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) +(1+FaceExpression||Face) , data=HappySadRT, REML=TRUE)
modelRT.nointeraction <- lmer (RT ~ StoryEmotion + FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) +(1+FaceExpression||Face) , data=HappySadRT, REML=TRUE)
anova (modelRT, modelRT.nointeraction)
summary (modelRT)
lsmeans (modelRT, pairwise~StoryEmotion*FaceExpression, adjust="none")
exp ((14798-14766)/2)

#calculate Bayes Factor
Bayes <- lmBF (RT ~ StoryEmotion*FaceExpression, as.data.frame(HappySadRT), whichRandom = list ("Subject", "Vignette", "Face"))
Bayes

#graphing the means and SEs
HappySadgraphdata <- read_csv("~/HappySadgraphdata.csv")
p <- ggplot(HappySadgraphdata, aes(x=StoryEmotion, y=Mean, group = FaceEmotion, colour = FaceEmotion, ymin = 1000, ymax=2000)) + geom_line() + geom_point() + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) + labs (y="RT in ms", x= "Vignette Emotion", colour="Facial Emotion")
p <- p + scale_colour_grey(start = 0, end = .7) + theme_bw()
p <- p + theme(text = element_text(size = 20))
p

#this is the analysis of the accuracy data
HappySadAcc <- read_csv("~/HappySadAcc.csv")

HappySadAcc$StoryEmotion <- as.factor (HappySadAcc$StoryEmotion)
HappySadAcc$FaceExpression <- as.factor (HappySadAcc$FaceExpression)

contrasts (HappySadAcc$StoryEmotion) <- matrix (c(.5, -.5)) 
contrasts (HappySadAcc$FaceExpression) <- matrix (c(.5, -.5)) 

#full model does not converge - most complex is with random intercepts and dropping random effect of Face
modelAcc <- glmer (Acc ~ StoryEmotion*FaceExpression + (1|Subject) + (1|Vignette) , data=HappySadAcc, family=binomial)
modelAcc.null <- glmer (Acc ~  (1|Subject) + (1|Vignette) , data=HappySadAcc, family=binomial)
modelAcc.nointeraction <- glmer (Acc ~  StoryEmotion + FaceExpression + (1|Subject) + (1|Vignette) , data=HappySadAcc, family=binomial)
anova (modelAcc, modelAcc.nointeraction)
summary (modelAcc)
lsmeans (modelAcc, pairwise~StoryEmotion*FaceExpression, adjust="none", type="response")
exp ((240.26-236.63)/2)

#graphing the means and SEs
HappySadgraphacc <- read_csv("~/HappySadgraphacc.csv")
p <- ggplot(HappySadgraphacc, aes(x=StoryEmotion, y=Mean, group = FaceExpression, colour = FaceExpression, ymin = 80)) + geom_line() + geom_point() + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) + labs (y="% Correct", x= "Vignette Emotion", colour="Facial Emotion")
p <- p + scale_colour_grey(start = 0, end = .7) + theme_bw()
p <- p + theme(text = element_text(size = 20))
p

#some plots
ggplot (HappySadRT, aes (StoryEmotion, RT, colour=FaceExpression)) + ylim(0,3000) + geom_point() + facet_wrap(~HappySadRT$Vignette) + geom_jitter()

