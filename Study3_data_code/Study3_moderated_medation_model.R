
#### Shifting effect: the mediation of subjective role_Study£ºmoderated mediation model




# Load the packages before you start
# install.packages("mediation")
source("process.R")  # use to test moderated effect
library(mediation)  # build the moderated mediation model
library(dplyr) # Manipulate the data

mediate <- mediation::mediate 
options(scipen=200)
set.seed(123)

# read data
shifts_attention_exp3 <- read.csv('')   ##'Study3_processed.csv'



#Demographic information
Demographic_data <- shifts_attention_exp3 %>% 
  dplyr::select(Subnum,Age,Sex) %>% distinct(Subnum,.keep_all = T)


#sex 38 male, 57 female
dim(Demographic_data[Demographic_data$Sex==1,])[1]
dim(Demographic_data[Demographic_data$Sex==2,])[1]


# age (mean, SD) 23.12 , 3.88
mean(Demographic_data$Age)
sd(Demographic_data$Age)




######### code Acc as 1, and control as 0
######### Real headline as 0.5, Fake headline as -0.5
shifts_attention_exp3 <-shifts_attention_exp3 %>% 
  mutate(group.x =
         case_when(group == 'Acc' ~ 1,
                   group == 'control' ~ 0,),
         Headline_Veracity.W =
           case_when(Headline_Veracity == 'Real' ~ 0.5,
                     Headline_Veracity == 'Fake' ~ -0.5
                     ))


################################ prepare to run ################################
## center the rating score (Credibility.c, Difficulty.c, Familiarity.c, Importance.c, Morality.c)
shifts_attention_exp3$Credibility.c <- scale(shifts_attention_exp3$Credibility, center = TRUE, scale = FALSE)[,]
shifts_attention_exp3$Difficulty.c <- scale(shifts_attention_exp3$Difficulty, center = TRUE, scale = FALSE)[,]
shifts_attention_exp3$Familiarity.c <- scale(shifts_attention_exp3$Familiarity, center = TRUE, scale = FALSE)[,]
shifts_attention_exp3$Importance.c <- scale(shifts_attention_exp3$Importance, center = TRUE, scale = FALSE)[,]
shifts_attention_exp3$Morality.c <- scale(shifts_attention_exp3$Morality, center = TRUE, scale = FALSE)[,]




####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

################################ build the model:  Group ---- sharing reward ----- share likelihood, Moderated by Veracity ################################
##################  direct path (Group ----share likelihood): see whether the relationship between group and share likelihood was moderated by veracity ########
objective.Mod.Med.Model.0 <- lm(Sharelikelihood ~ group.x*Headline_Veracity.W  +Difficulty.c+Familiarity.c+Importance.c+Morality.c, data = shifts_attention_exp3)
summary(objective.Mod.Med.Model.0)
confint(objective.Mod.Med.Model.0)


##################  a path (Group ---- sharing reward):  see whether the relationship between group and sharing reward was moderated by veracity#########
objective.Mod.Med.Model.1 <- lm(reward ~ group.x*Headline_Veracity.W+Difficulty.c+Familiarity.c+Importance.c+Morality.c, data = shifts_attention_exp3) 
summary(objective.Mod.Med.Model.1)
confint(objective.Mod.Med.Model.1)

##################  b path (sharing reward ----- share likelihood): see whether sharing reward can predict share likelihood ############
objective.Mod.Med.Model.2 <- lm(Sharelikelihood ~ group.x*Headline_Veracity.W + reward +Difficulty.c+Familiarity.c+Importance.c+Morality.c, data = shifts_attention_exp3)
summary(objective.Mod.Med.Model.2)
confint(objective.Mod.Med.Model.2)

############ because we find veracity can not moderate direct path or a path, so it is just a mediation model#####
############ test the mediation model  ##################
Subjective.Mod.Med.Test.w <- mediate(objective.Mod.Med.Model.1, objective.Mod.Med.Model.2, 
                          boot = TRUE, boot.ci.type = "bca", sims = 5000, 
                          treat="group.x", mediator="reward")
summary(Subjective.Mod.Med.Test.w)





################################ build the model:  Group ---- sharing reward ----- share likelihood, Moderated by Credibility ################################
##################  a path (Group ---- sharing reward):  see whether the relationship between group and sharing reward was moderated by credibility #########
Mod.Med.Model.1 <- lm(reward ~ group.x*Credibility.c+Difficulty.c+Familiarity.c+Importance.c+Morality.c, data = shifts_attention_exp3) 
summary(Mod.Med.Model.1)
confint(Mod.Med.Model.1)


##################  b path (sharing reward ----- share likelihood): see whether sharing reward can predict share likelihood ############
Mod.Med.Model.2 <- lm(Sharelikelihood ~ group.x*Credibility.c + reward +Difficulty.c+Familiarity.c+Importance.c+Morality.c, data = shifts_attention_exp3)
summary(Mod.Med.Model.2)
confint(Mod.Med.Model.2)



########## calculate low credibility: mean - SD #############
low.Credibility.c <- mean(shifts_attention_exp3$Credibility.c) - sd(shifts_attention_exp3$Credibility.c)


#########  see the mediation effect for headline with low credibility score ##########
Mod.Med.low.w <- mediate(Mod.Med.Model.1, Mod.Med.Model.2,    
                         covariates = list(Credibility.c = low.Credibility.c), 
                         boot = TRUE, boot.ci.type = "bca", sims = 5000, 
                         treat = "group.x", mediator = "reward")
summary(Mod.Med.low.w)


########## calculate high credibility: mean - SD  ############
high.Credibility.c <- mean(shifts_attention_exp3$Credibility.c) + sd(shifts_attention_exp3$Credibility.c)



#########  see the mediation effect for headline with high credibility score #############
Mod.Med.High.w <- mediate(Mod.Med.Model.1, Mod.Med.Model.2,    
                          covariates = list(Credibility.c = high.Credibility.c), 
                          boot = TRUE, boot.ci.type = "bca", sims = 5000, 
                          treat = "group.x", mediator = "reward")
summary(Mod.Med.High.w)


#### examine the moderated role of Credbility in a path (Group ---- sharing reward) #######
process(data = shifts_attention_exp3, y = 'reward', x = 'group.x', w = 'Credibility.c', cov = c('Difficulty.c','Familiarity.c','Importance.c','Morality.c'),
        model = 1, moment = 1, covcoeff = 1, modelbt = 1, boot = 10000,
        seed = 123, plot = 1)


#### examine the moderated role of Credbility in direct path (Group ---- share likelihood) #####
process(data = shifts_attention_exp3, y = 'Sharelikelihood', x = 'group.x', w = 'Credibility.c', cov = c('Difficulty.c','Familiarity.c','Importance.c','Morality.c'),
        model = 1, moment = 1, covcoeff = 1, modelbt = 1, boot = 10000,
        seed = 123, plot = 1)



########## test whether mediaton effect differ between  high credibility  and low credibility  ######
Mod.Med.Test.w <- mediate(Mod.Med.Model.1, Mod.Med.Model.2, 
                          boot = TRUE, boot.ci.type = "bca", sims = 5000, 
                          treat="group.x", mediator="reward")

summary(Mod.Med.Test.w)


test.modmed(Mod.Med.Test.w, 
            covariates.1 = list(Credibility.c = low.Credibility.c),   
            covariates.2 = list(Credibility.c = high.Credibility.c), 
            sims = 5000)






