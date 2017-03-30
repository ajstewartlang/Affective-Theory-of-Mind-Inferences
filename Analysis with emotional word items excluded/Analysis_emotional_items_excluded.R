library (lme4)
library (lmerTest)
library (lsmeans)
library (pbkrtest)
library (readr)
library (ggplot2)

#analysis for each Experiments with emotional items excluded
#for the Anger Fear Experiment (Experiment 1) these were items 18,22,27, and 29
#for the Happy Sad Experiment (Experiment 2) these were items 1,8,10,15,18,19,22,28,29, and 30

#The following is the reanalysis of Experiment 1 with emotional items excluded
AngerFearRT <- read_csv("~/AngerFearRT_emotional_items_excluded.csv")

AngerFearRT$StoryEmotion <- as.factor (AngerFearRT$StoryEmotion)
AngerFearRT$FaceExpression <- as.factor (AngerFearRT$FaceExpression)

contrasts (AngerFearRT$StoryEmotion) <- matrix (c(.5, -.5)) 
contrasts (AngerFearRT$FaceExpression) <- matrix (c(.5, -.5)) 

modelRT <- lmer (RT ~ StoryEmotion*FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) + (1+FaceExpression|Face), data=AngerFearRT, REML=TRUE)
summary (modelRT)

lsmeans (modelRT, pairwise~StoryEmotion*FaceExpression, adjust="none")

#The following is the reanalysis of Experiment 2 with emotional items excluded
HappySadRT <- read_csv("~/HappySadRT_emotional_items_excluded.csv")

HappySadRT$StoryEmotion <- as.factor (HappySadRT$StoryEmotion)
HappySadRT$FaceExpression <- as.factor (HappySadRT$FaceExpression)

contrasts (HappySadRT$StoryEmotion) <- matrix (c(.5, -.5)) 
contrasts (HappySadRT$FaceExpression) <- matrix (c(.5, -.5)) 

modelRT <- lmer (RT ~ StoryEmotion*FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Vignette) + (1+StoryEmotion+FaceExpression|Face), data=HappySadRT, REML=TRUE)
summary (modelRT)

lsmeans (modelRT, pairwise~StoryEmotion*FaceExpression, adjust="none")

