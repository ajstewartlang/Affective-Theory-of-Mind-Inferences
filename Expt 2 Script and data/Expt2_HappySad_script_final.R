library (lme4)
library (lmerTest)
library (emmeans)
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
summary (modelRT)
emmeans (modelRT, pairwise~StoryEmotion*FaceExpression, adjust="none")

#the below is to allow us to calcaulte Bayes Factors
modelRT <- lmer (RT ~ StoryEmotion*FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) + (1+FaceExpression||Face), data=HappySadRT, REML=TRUE)
modelRT.nointeraction <- lmer (RT ~ StoryEmotion + FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) +(1+FaceExpression||Face) , data=HappySadRT, REML=TRUE)
anova (modelRT, modelRT.nointeraction)
summary (modelRT)
emmeans (modelRT, pairwise~StoryEmotion*FaceExpression, adjust="none")
exp ((14798-14766)/2)

#this is the analysis of the accuracy data
HappySadAcc <- read_csv("~/HappySadAcc.csv")

HappySadAcc$StoryEmotion <- as.factor (HappySadAcc$StoryEmotion)
HappySadAcc$FaceExpression <- as.factor (HappySadAcc$FaceExpression)

contrasts (HappySadAcc$StoryEmotion) <- matrix (c(.5, -.5)) 
contrasts (HappySadAcc$FaceExpression) <- matrix (c(.5, -.5)) 

#full model does not converge - most complex is with random intercepts and dropping random effect of Face
modelAcc <- glmer (Acc ~ StoryEmotion*FaceExpression + (1|Subject) + (1|Vignette) , data=HappySadAcc, family=binomial)
modelAcc.nointeraction <- glmer (Acc ~  StoryEmotion + FaceExpression + (1|Subject) + (1|Vignette) , data=HappySadAcc, family=binomial)
anova (modelAcc, modelAcc.nointeraction)
summary (modelAcc)
emmeans (modelAcc, pairwise~StoryEmotion*FaceExpression, adjust="none", type="response")
exp ((240.26-236.63)/2)


