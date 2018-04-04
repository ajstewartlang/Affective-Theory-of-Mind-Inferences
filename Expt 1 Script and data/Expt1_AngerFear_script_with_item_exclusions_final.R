library (lme4)
library (lmerTest)
library (emmeans)
library (pbkrtest)
library (readr)

#script for AngerFear RT and accuracy data analysis with arousal

#this is the analysis of the RT data
AngerFearRT <- read_csv("~/AngerFearRT.csv")

AngerFearRT$StoryEmotion <- as.factor (AngerFearRT$StoryEmotion)
AngerFearRT$FaceExpression <- as.factor (AngerFearRT$FaceExpression)

contrasts (AngerFearRT$StoryEmotion) <- matrix (c(.5, -.5)) 
contrasts (AngerFearRT$FaceExpression) <- matrix (c(.5, -.5)) 

#exclude items 5, 8, 12, 18, 6
filter <- !((AngerFearRT$Vignette==5) | (AngerFearRT$Vignette==8) | (AngerFearRT$Vignette==12) | (AngerFearRT$Vignette==18) | (AngerFearRT$Vignette==6))

#with Subject, Vignette, and Face as crossed random effects with arousal
#full model does not converge so need to drop interaction term from the random effects - in addition, Face random effect has only random intercept and slope for FaceExpression
#model with covariate as factor
modelRTAr1 <- lmer (RT ~ Arousal + StoryEmotion*FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) + (1+FaceExpression|Face) + (0+Arousal|Face), data=AngerFearRT[filter,], REML=TRUE)

#model without covaraite
modelRT <- lmer (RT ~ StoryEmotion*FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) + (1+FaceExpression|Face), data=AngerFearRT[filter,], REML=TRUE)

#compare models
anova (modelRTAr1, modelRT)

#difference between models not signif - arousal does not interact with effect so drop arousal from subsequent analysis
summary (modelRT)
emmeans (modelRT, pairwise~StoryEmotion*FaceExpression, adjust="none")

#this is the analysis of the accuracy data
AngerFearAcc <- read_csv("~/AngerFearAcc.csv")

AngerFearAcc$StoryEmotion <- as.factor (AngerFearAcc$StoryEmotion)
AngerFearAcc$FaceExpression <- as.factor (AngerFearAcc$FaceExpression)

contrasts (AngerFearAcc$StoryEmotion)<-matrix (c(.5, -.5)) 
contrasts (AngerFearAcc$FaceExpression)<-matrix (c(.5, -.5)) 

#exclude items 5, 8, 12, 18, 6
filter <- !((AngerFearAcc$Vignette==5) | (AngerFearAcc$Vignette==8) | (AngerFearAcc$Vignette==12) | (AngerFearAcc$Vignette==18) | (AngerFearAcc$Vignette==6))


#full model does not converge - most complex is with random intercepts
modelAcc <- glmer (Accuracy ~ StoryEmotion*FaceExpression + (1|Subject) + (1|Vignette) , data=AngerFearAcc[filter,], family=binomial)
emmeans (modelAcc, pairwise~StoryEmotion*FaceExpression, adjust="none", type="response")
summary (modelAcc)


