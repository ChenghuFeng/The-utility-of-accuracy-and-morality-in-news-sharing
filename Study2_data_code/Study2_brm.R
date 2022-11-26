

memory.limit(1000000)
set.seed(42)


# Load package
library(brms)
library(readxl) # Read Excel files


#### read data (Study1_processed.xlsx)#### 
data_Study2 <- read.csv('All_study2_120_processed.csv')



#Demographic information
Demographic_data <- data_Study2 %>% 
  select(SubjectNum,age,gender) %>% distinct(SubjectNum,.keep_all = T)


# sex 33 male, 78 female
dim(Demographic_data[Demographic_data$gender==1,])[1]
dim(Demographic_data[Demographic_data$gender==2,])[1]

# age (mean, SD) 23.225 , 5.8429
mean(Demographic_data$age)
sd(Demographic_data$age)



# z-score transform for rating scores #### 
data_Study2$credibility.z <- scale(data_Study2$credibility)[,1]
data_Study2$Difficulty.z  <- scale(data_Study2$Difficulty)[,1]
data_Study2$familiarity.z  <- scale(data_Study2$familiarity)[,1]
data_Study2$importance.z  <- scale(data_Study2$importance)[,1]
data_Study2$morality.z  <- scale(data_Study2$morality)[,1]


#### z-score transform for dependent variable (sharing reward) #### 
#### by transform this, prior would be easier to set in the later ####
data_Study2$reward.z  <- scale(data_Study2$reward)[,1]



#### coding each factor #### 

# groupAccpriming(Control = 0, Accpriming = 1, Moralpriming = 0),compare Accpriming to Control group
# groupMoralpriming(Control = 0, Accpriming = 0, Moralpriming = 1),compare Moralpriming to Control group
data_Study2$group <- factor(data_Study2$group,levels = c('Control','Acc','Moral'))
contrasts(data_Study2$group)

# code moral headline as 0.5, immoral headline as -0.5
data_Study2$Headline_Morality <- factor(data_Study2$Headline_Morality,levels = c('immoral','moral'))
contrasts(data_Study2$Headline_Morality) <- c(-0.5,0.5)

# code real headline as 0.5, fake headline as -0.5
data_Study2$Headline_Veracity <- factor(data_Study2$Headline_Veracity,levels = c('fake','real'))
contrasts(data_Study2$Headline_Veracity) <- c(-0.5,0.5)


#### Objective model analysis, where independent variable (group, headline veracity, and headline morality is setting before exp)
#### run a maximal model: reward ~ group * veracity * morality +(1+veracity*morality|sub) + (1+group|item) ####



S2_Bayesian_Obj <-
  brm(data = data_Study2,
      family = gaussian,
      formula = reward.z ~ 1 + group * Headline_Veracity * Headline_Morality + (1 + Headline_Veracity * Headline_Morality | SubjectNum) + (1 + group | Headline_id),
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 1),   class = b),        ## population-levle effect (aka fix effect)
                prior(exponential(1), class = sd),      ## standard error of group level effect (aka random effect) 
                prior(exponential(1), class = sigma),   
                prior(lkj(2),         class = cor)),  
      iter = 4000,
      warmup = 2000,
      chains = 4,
      cores = 4,    ###
      seed = 42,
      control = list(adapt_delta = 0.9),  
      sample_prior = TRUE,  ### necessary for getting Bayes factor
      file = "Study2_Bayesian_objective.rds")




#### load the result of Objective model ####
S2_Bayesian_Obj <- readRDS("Study2_Bayesian_objective.rds")



########## get bayes factor (for objective model), mind again set 'sample_prior = TRUE' is important########## 


hypothesis_S2_ob <- c('groupAcc=0','groupMoral=0','Headline_Veracity1=0','Headline_Morality1=0',
                      'groupAcc:Headline_Veracity1=0','groupMoral:Headline_Veracity1=0',
                      'groupAcc:Headline_Morality1=0','groupMoral:Headline_Morality1=0',
                      'Headline_Veracity1:Headline_Morality1=0',
                      'groupAcc:Headline_Veracity1:Headline_Morality1=0',
                      'groupMoral:Headline_Veracity1:Headline_Morality1=0'
                      )


q <- hypothesis(S2_Bayesian_Obj,hypothesis_S2_ob)




########## get effect size for objective model, Cohen's d = coefficient / sigma ##########

draws1 <- as.data.frame(S2_Bayesian_Obj)
Cohend_groupMoralpriming = mean(draws1$b_groupMoral/draws1$sigma)
Cohend_groupAccpriming_headlineveracity = mean(draws1$`b_groupAcc:Headline_Veracity1`/draws1$sigma)






#### Objective model analysis, where independent variable (group, headline veracity, and headline morality is setting before exp)
#### run a maximal model: reward ~ group * veracity * morality +(1+veracity*morality|sub) + (1+group|item) ####



S2_Bayesian_Sub <-
  brm(data = data_Study2,
      family = gaussian,
      formula = reward.z ~ 1 + group * (credibility.z + Difficulty.z + familiarity.z + importance.z + morality.z)  + 
        (1 + credibility.z + Difficulty.z + familiarity.z + importance.z + morality.z | SubjectNum) +
        (1 + group * (credibility.z + Difficulty.z + familiarity.z + importance.z + morality.z) | Headline_id),
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 1),   class = b),        ## population-levle effect (aka fix effect)
                prior(exponential(1), class = sd),      ## standard error of group level effect (aka random effect) 
                prior(exponential(1), class = sigma),   
                prior(lkj(2),         class = cor)),  
      iter = 4000,
      warmup = 2000,
      chains = 4,
      cores = 4,    ###
      seed = 42,
      control = list(adapt_delta = 0.9),  
      sample_prior = TRUE,  ### necessary for getting Bayes factor
      file = "Study2_Bayesian_Subjective_full.rds")



#### load the result of subjective model ####
S2_Bayesian_Sub <- readRDS("Study2_Bayesian_Subjective_full.rds")



### Bayes factor for subjective model
hypothesis_S2_Sub <- c('groupAcc=0','groupMoral=0','credibility.z=0','Difficulty.z=0','familiarity.z=0','importance.z=0','morality.z=0',
                       'groupAcc:credibility.z = 0','groupMoral:credibility.z = 0',
                       'groupAcc:Difficulty.z =0', 'groupMoral:Difficulty.z = 0',
                       'groupAcc:familiarity.z=0','groupMoral:familiarity.z=0',
                       'groupAcc:importance.z=0','groupMoral:importance.z=0',
                       'groupAcc:morality.z=0','groupMoral:morality.z=0')


hypothesis(S2_Bayesian_Sub,hypothesis_S2_Sub)



########## get effect size for objective model, Cohen's d = coefficient / sigma ##########
draws2 <- as.data.frame(S2_Bayesian_Sub)

Cohend_importance = mean(draws2$b_importance.z/draws2$sigma)
Cohend_morality = mean(draws2$b_morality.z/draws2$sigma)
Cohend_groupAcc_Credi = mean(draws2$`b_groupAcc:credibility.z`/draws2$sigma)


### interesting result 
### Acc priming group: weigh on credibility - weigh on morality
# hypothesis(S2_Bayesian_Sub,'(credibility.z+groupAcc:credibility.z*1) - (morality.z+groupAcc:morality.z*1) =0')
hypothesis(S2_Bayesian_Sub,'abs(credibility.z+groupAcc:credibility.z*1) - abs(morality.z+groupAcc:morality.z*1) =0')

### Moral priming group: weigh on credibility - weigh on morality
# hypothesis(S2_Bayesian_Sub, "(credibility.z+groupMoral:credibility.z*1) - (morality.z+groupMoral:morality.z*1) = 0") 
hypothesis(S2_Bayesian_Sub, "abs(credibility.z+groupMoral:credibility.z*1) - abs(morality.z+groupMoral:morality.z*1) = 0") 



### Control group: weigh on credibility - weigh on morality
# hypothesis(S2_Bayesian_Sub, "credibility.z - morality.z = 0")   
hypothesis(S2_Bayesian_Sub, "abs(credibility.z) - abs(morality.z) = 0")   




### Acc priming group: weigh on credibility, morality, importance, familiarity, difficulty
hypothesis(S2_Bayesian_Sub, "credibility.z+ groupAcc:credibility.z*1 = 0")   
hypothesis(S2_Bayesian_Sub, "morality.z+ groupAcc:morality.z*1 = 0") 
hypothesis(S2_Bayesian_Sub, "importance.z+ groupAcc:importance.z*1 = 0") 
hypothesis(S2_Bayesian_Sub, "familiarity.z+ groupAcc:familiarity.z*1 = 0") 
hypothesis(S2_Bayesian_Sub, "Difficulty.z+ groupAcc:Difficulty.z*1 = 0") 



### Acc priming group total weigh (weigh1 + weigh2 + weigh3 + weigh4 + weigh5) - control group total weigh(weigh1 + weigh2 + weigh3 + weigh4 + weigh5)

hypothesis(S2_Bayesian_Sub,'(credibility.z+ groupAcc:credibility.z*1 +
                             morality.z+ groupAcc:morality.z*1 +
                             importance.z+ groupAcc:importance.z*1 +
                            familiarity.z+ groupAcc:familiarity.z*1+
                             Difficulty.z+ groupAcc:Difficulty.z*1)- 
                           (credibility.z+morality.z+importance.z+familiarity.z+Difficulty.z) = 0')





### Acc priming group: weigh on credibility, morality, importance, familiarity, difficulty, get absolute value
hypothesis(S2_Bayesian_Sub, "abs(credibility.z+ groupAcc:credibility.z*1) = 0")   
hypothesis(S2_Bayesian_Sub, "abs(morality.z+ groupAcc:morality.z*1) = 0") 
hypothesis(S2_Bayesian_Sub, "abs(importance.z+ groupAcc:importance.z*1) = 0") 
hypothesis(S2_Bayesian_Sub, "abs(familiarity.z+ groupAcc:familiarity.z*1) = 0") 
hypothesis(S2_Bayesian_Sub, "abs(Difficulty.z+ groupAcc:Difficulty.z*1) = 0") 




### Acc priming group total weigh (|weigh1| + |weigh2| + |weigh3| + |weigh4| + |weigh5|) - control group total weigh(|weigh1| + |weigh2| + |weigh3| + |weigh4| + |weigh5|)
hypothesis(S2_Bayesian_Sub,'(abs(credibility.z+ groupAcc:credibility.z*1) +
                              abs(morality.z+ groupAcc:morality.z*1) +
                              abs(importance.z+ groupAcc:importance.z*1) +
                              abs(familiarity.z+ groupAcc:familiarity.z*1) +
                              abs(Difficulty.z+ groupAcc:Difficulty.z*1))
                            -(abs(credibility.z)+abs(morality.z)+abs(importance.z)+abs(familiarity.z)+abs(Difficulty.z)) =0')




### Morality priming group: weigh on credibility, morality, importance, familiarity, difficulty
hypothesis(S2_Bayesian_Sub,'(credibility.z+ groupMoral:credibility.z*1 +
                             morality.z+ groupMoral:morality.z*1 +
                             importance.z+ groupMoral:importance.z*1 +
                            familiarity.z+ groupMoral:familiarity.z*1+
                             Difficulty.z+ groupMoral:Difficulty.z*1)- 
                           (credibility.z+morality.z+importance.z+familiarity.z+Difficulty.z) = 0')



### Morality priming group total weigh (weigh1 + weigh2 + weigh3 + weigh4 + weigh5) - control group total weigh(weigh1 + weigh2 + weigh3 + weigh4 + weigh5)

hypothesis(S2_Bayesian_Sub,'(credibility.z+ groupMoral:credibility.z*1 +
                             morality.z+ groupMoral:morality.z*1 +
                             importance.z+ groupMoral:importance.z*1 +
                            familiarity.z+ groupMoral:familiarity.z*1+
                             Difficulty.z+ groupMoral:Difficulty.z*1)- 
                           (credibility.z+morality.z+importance.z+familiarity.z+Difficulty.z) = 0')



### Morality priming group: weigh on credibility, morality, importance, familiarity, difficulty, get absolute value
hypothesis(S2_Bayesian_Sub, "abs(credibility.z+ groupMoral:credibility.z*1) = 0")   
hypothesis(S2_Bayesian_Sub, "abs(morality.z+ groupMoral:morality.z*1) = 0") 
hypothesis(S2_Bayesian_Sub, "abs(importance.z+ groupMoral:importance.z*1) = 0") 
hypothesis(S2_Bayesian_Sub, "abs(familiarity.z+ groupMoral:familiarity.z*1) = 0") 
hypothesis(S2_Bayesian_Sub, "abs(Difficulty.z+ groupMoral:Difficulty.z*1) = 0") 


### Morality priming group total weigh (|weigh1| + |weigh2| + |weigh3| + |weigh4| + |weigh5|) - control group total weigh(|weigh1| + |weigh2| + |weigh3| + |weigh4| + |weigh5|)
hypothesis(S2_Bayesian_Sub,'(abs(credibility.z+ groupMoral:credibility.z*1) +
                              abs(morality.z+ groupMoral:morality.z*1) +
                              abs(importance.z+ groupMoral:importance.z*1) +
                              abs(familiarity.z+ groupMoral:familiarity.z*1) +
                              abs(Difficulty.z+ groupMoral:Difficulty.z*1))
                            -(abs(credibility.z)+abs(morality.z)+abs(importance.z)+abs(familiarity.z)+abs(Difficulty.z)) =0')

