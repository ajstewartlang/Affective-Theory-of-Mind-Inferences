library (lme4)
library (lmerTest)
library (lsmeans)
library (pbkrtest)
library (readr)

#script for HappySad RT and accuracy data analysis

#this is the analysis of the RT data

HappySadRT <- read_csv("~/HappySadRT.csv")

HappySadRT$StoryEmotion <- as.factor (HappySadRT$StoryEmotion)
HappySadRT$FaceExpression <- as.factor (HappySadRT$FaceExpression)

contrasts (HappySadRT$StoryEmotion)<-matrix (c(.5, -.5)) 
contrasts (HappySadRT$FaceExpression)<-matrix (c(.5, -.5)) 

#in datafile Trials corresponds to Vignettes, and KDEFfaces corresponds to Faces

#this model is with crossed random effects - full model does not converge but the following one does (dropping the interaciton term from the final random effect)
modelRT <- lmer (RT ~ StoryEmotion*FaceExpression + (1+StoryEmotion*FaceExpression|Subject) + (1+StoryEmotion*FaceExpression|Trials) + (1+StoryEmotion+FaceExpression|KDEFfaces), data=HappySadRT, REML=TRUE)
modelRT.null <- lmer (RT ~ (1+StoryEmotion*FaceExpression|Subject) + (1+StoryEmotion*FaceExpression|Trials) + (1+StoryEmotion+FaceExpression|KDEFfaces), data=HappySadRT, REML=TRUE)
anova (modelRT, modelRT.null)
summary (modelRT)
lsmeans (modelRT, pairwise~StoryEmotion*FaceExpression, adjust="none")

#graphing the means and SEs
HappySadgraphdata <- read_csv("~/HappySadgraphdata.csv")
p <- ggplot(HappySadgraphdata, aes(x=StoryEmotion, y=Mean, group = FaceEmotion, colour = FaceEmotion, ymin = 1000, ymax=2000)) + geom_line() + geom_point() + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) + labs (y="RT in ms", x= "Story Emotion", colour="Face Emotion")
p <- p + scale_colour_grey(start = 0, end = .7) + theme_bw()
p

#this is the analysis of the accuracy data
HappySadAcc <- read_csv("~/HappySadAcc.csv")

HappySadAcc$StoryEmotion <- as.factor (HappySadAcc$StoryEmotion)
HappySadAcc$FaceExpression <- as.factor (HappySadAcc$FaceExpression)

contrasts (HappySadAcc$StoryEmotion)<-matrix (c(.5, -.5)) 
contrasts (HappySadAcc$FaceExpression)<-matrix (c(.5, -.5)) 

#full model does not converge - most complex is with random intercepts
modelAcc <- glmer (Acc ~ StoryEmotion*FaceExpression + (1|Subject) + (1|Trials) , data=HappySadAcc, family=binomial)
summary (modelAcc)
lsmeans (modelAcc, pairwise~StoryEmotion*FaceExpression, adjust="none", type="response")

#graphing the means and SEs
HappySadgraphacc <- read_csv("~/HappySadgraphacc.csv")
p <- ggplot(HappySadgraphacc, aes(x=StoryEmotion, y=Mean, group = FaceExpression, colour = FaceExpression, ymin = 80)) + geom_line() + geom_point() + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1) + labs (y="% Correct", x= "Story Emotion", colour="Face Emotion")
p <- p + scale_colour_grey(start = 0, end = .7) + theme_bw()
p
