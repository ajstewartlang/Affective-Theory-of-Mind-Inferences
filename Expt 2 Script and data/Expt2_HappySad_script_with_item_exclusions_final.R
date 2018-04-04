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

#exclude items 18, 21, 27, 28
filter <- !((HappySadRT$Vignette==18) | (HappySadRT$Vignette==21) | (HappySadRT$Vignette==27) | (HappySadRT$Vignette==28))

#this model is with crossed random effects - full model does not converge but the following one does (dropping the interaction terms from the random effects)
modelRT <- lmer (RT ~ StoryEmotion*FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) + (1+StoryEmotion+FaceExpression|Face), data=HappySadRT[filter,], REML=TRUE)
summary (modelRT)
emmeans (modelRT, pairwise~StoryEmotion*FaceExpression, adjust="none")

#this is the analysis of the accuracy data
HappySadAcc <- read_csv("~/HappySadAcc.csv")

HappySadAcc$StoryEmotion <- as.factor (HappySadAcc$StoryEmotion)
HappySadAcc$FaceExpression <- as.factor (HappySadAcc$FaceExpression)

contrasts (HappySadAcc$StoryEmotion) <- matrix (c(.5, -.5)) 
contrasts (HappySadAcc$FaceExpression) <- matrix (c(.5, -.5)) 

#exclude items 18, 21, 27, 28
filter <- !((HappySadAcc$Vignette==18) | (HappySadAcc$Vignette==21) | (HappySadAcc$Vignette==27) | (HappySadAcc$Vignette==28))

#full model does not converge - most complex is with random intercepts and dropping random effect of Face
modelAcc <- glmer (Acc ~ StoryEmotion*FaceExpression + (1|Subject) + (1|Vignette) , data=HappySadAcc[filter,], family=binomial)
summary (modelAcc)
emmeans (modelAcc, pairwise~StoryEmotion*FaceExpression, adjust="none", type="response")
