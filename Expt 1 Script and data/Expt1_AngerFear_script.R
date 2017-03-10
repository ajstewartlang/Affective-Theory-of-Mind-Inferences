library (lme4)
library (lmerTest)
library (lsmeans)
library (pbkrtest)
library (readr)


#script for AngerFear RT and accuracy data analysis with arousal

#this is the analysis of the RT data
AngerFearRT <- read_csv("~/AngerFearRT.csv")

AngerFearRT$StoryEmotion <- as.factor (AngerFearRT$StoryEmotion)
AngerFearRT$FaceExpression <- as.factor (AngerFearRT$FaceExpression)
AngerFearRT$KDEFfaces <- as.factor (AngerFearRT$KDEFfaces)

contrasts (AngerFearRT$StoryEmotion)<-matrix (c(.5, -.5)) 
contrasts (AngerFearRT$FaceExpression)<-matrix (c(.5, -.5)) 

#in datafile Trials corresponds to Vignettes, and KDEFfaces corresponds to Faces

#with Participants, Faces, and Trials (i.e., vignettes) as crossed random effects with arousal
#full model does not converge so need to drop interaction term from the random effects - KDEFfaces only has slope for FaceExpression
modelRTAr1 <- lmer (RT ~ StoryEmotion*FaceExpression*Arousal + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Trials) + (1+FaceExpression|KDEFfaces), data=AngerFearRT, REML=TRUE)
summary (modelRTAr1)
modelRT <- lmer (RT ~ StoryEmotion*FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Trials) + (1+FaceExpression|KDEFfaces), data=AngerFearRT, REML=TRUE)
anova (modelRTAr1, modelRT)

#dropping arousal as non-signif - with Participants, Faces, and Trials (i.e., vignettes) as crossed random effects
modelRT <- lmer (RT ~ StoryEmotion*FaceExpression + (1+StoryEmotion+FaceExpression|Subject) + (1+StoryEmotion+FaceExpression|Trials) + (1+FaceExpression|KDEFfaces), data=AngerFearRT, REML=TRUE)
modelRTnull <- lmer (RT ~  (1+StoryEmotion+FaceExpression|Subject) + + (1+StoryEmotion+FaceExpression|Trials) + (1+FaceExpression|KDEFfaces), data=AngerFearRT, REML=TRUE)
anova (modelRT, modelRTnull)
summary (modelRT)
summary (modelRTnull)
lsmeans (modelRT, pairwise~StoryEmotion*FaceExpression, adjust="none")

#graphing the means and SEs
AngerFeargraphdata <- read_csv("~/AngerFeargraphdata.csv")
p <- ggplot(AngerFeargraphdata, aes(x=StoryEmotion, y=Mean, group = FaceEmotion, colour = FaceEmotion, ymin = 1000)) + geom_line() + geom_point() + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) + labs (y="RT in ms", x= "Story Emotion", colour="Face Emotion")
p <- p + scale_colour_grey(start = 0, end = .7) + theme_bw()
p

#this is the analysis of the accuracy data
AngerFearAcc <- read_csv("~/AngerFearAcc.csv")

AngerFearAcc$StoryEmotion <- as.factor (AngerFearAcc$StoryEmotion)
AngerFearAcc$FaceExpression <- as.factor (AngerFearAcc$FaceExpression)

contrasts (AngerFearAcc$StoryEmotion)<-matrix (c(.5, -.5)) 
contrasts (AngerFearAcc$FaceExpression)<-matrix (c(.5, -.5)) 

#full model does not converge - most complex is with random intercepts
modelAcc <- glmer (Accuracy ~ StoryEmotion*FaceExpression + (1|Subject) + (1|Trials) , data=AngerFearAcc, family=binomial)
lsmeans (modelAcc, pairwise~StoryEmotion*FaceExpression, adjust="none", type="response")
modelAcc.null <- glmer (Accuracy ~  (1|Subject) + (1|Trials) , data=AngerFearAcc, family=binomial)
anova (modelAcc, modelAcc.null)
summary (modelAcc)

#graphing the means and SEs
AngerFeargraphacc <- read_csv("~/AngerFeargraphacc.csv")
p <- ggplot(AngerFeargraphacc, aes(x=StoryEmotion, y=Mean, group = FaceEmotion, colour = FaceEmotion, ymin = 80)) + geom_line() + geom_point() + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) + labs (y="% Correct", x= "Story Emotion", colour="Face Emotion")
p <- p + scale_colour_grey(start = 0, end = .7) + theme_bw()
p