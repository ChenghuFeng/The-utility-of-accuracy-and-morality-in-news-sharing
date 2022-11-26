################# Study_1 : the shifting attention effect on sharing willingness ################# 

# Load package
library(brms)
library(readxl) # Read Excel files



memory.limit(1000000)
set.seed(42)


#### read data (Study1_processed.xlsx)#### 
data_Study1 <- read_excel('Study1_processed.xlsx')  



#### z-score transform for rating scores #### 
data_Study1$rating_accuracy.z <- scale(data_Study1$rating_accuracy)[,1]
data_Study1$rating_difficulty.z <- scale(data_Study1$rating_difficulty)[,1]
data_Study1$rating_familiarity.z <- scale(data_Study1$rating_familiarity)[,1]
data_Study1$rating_importance.z <- scale(data_Study1$rating_importance)[,1]
data_Study1$rating_morality.z <- scale(data_Study1$rating_morality)[,1]


#### z-score transform for dependent variable (share willing) #### 
#### by transform this, prior would be easier to set in the later ####
data_Study1$Sharewilling.z <- scale(data_Study1$Sharewilling)[,1]


#### coding each factor #### 

## groupAccpriming(Control = 0, Accpriming = 1, Moralpriming = 0),compare Accpriming to Control group ##
## groupMoralpriming(Control = 0, Accpriming = 0, Moralpriming = 1),compare Moralpriming to Control group ##
data_Study1$group <- factor(data_Study1$group,levels = c('Control','Accpriming','Moralpriming'))
contrasts(data_Study1$group)


## code real headline as 0.5, fake headline as -0.5 ##
data_Study1$Istrue <- factor(data_Study1$Istrue,levels = c('Fake','Real'))
contrasts(data_Study1$Istrue) <- c(-0.5,0.5)


## code moral headline as 0.5, neutral headline as -0.5 ##
data_Study1$IsMoral <- factor(data_Study1$IsMoral,levels = c('Neutral','Moral'))
contrasts(data_Study1$IsMoral) <- c(-0.5,0.5)



#### Objective model analysis, where independent variable (group, headline veracity, and headline morality is setting before exp)
#### run a maximal model: Sharing willing ~ group * veracity * morality +(1+veracity*morality|sub) + (1+group|item) ####

S1_Bayesian_Obj <-
  brm(data = data_Study1,
      family = gaussian,
      formula = Sharewilling.z ~ 1 + group * Istrue * IsMoral + (1 + Istrue * IsMoral | Subject) + (1 + group | item_label),
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
      file = "Study1_Bayesian_objective.rds")



#### load the result of Objective model ####
S1_Bayesian_Obj <- readRDS("Study1_Bayesian_objective.rds")


########## get bayes factor (for objective model), mind again set 'sample_prior = TRUE' is important########## 


hypothesis(S1_Bayesian_Obj, "groupAccpriming = 0")    # BF10 = 1/4.95 = 0.20
hypothesis(S1_Bayesian_Obj, "groupMoralpriming = 0")  # BF10 = 1/0.26 = 3.85
hypothesis(S1_Bayesian_Obj, "Istrue1 = 0")            # BF10 = 1/1.92 = 0.52 
hypothesis(S1_Bayesian_Obj, "IsMoral1 = 0")           # BF10 = 1/4.83 = 0.21
hypothesis(S1_Bayesian_Obj, "groupAccpriming:Istrue1 = 0") # BF10 = 1/0.04 = 25
hypothesis(S1_Bayesian_Obj, "groupMoralpriming:Istrue1 = 0") # BF10 = 1/2.82 = 0.35
hypothesis(S1_Bayesian_Obj, "groupAccpriming:IsMoral1 = 0") # BF10 = 1/6.74 = 0.148
hypothesis(S1_Bayesian_Obj, "groupMoralpriming:IsMoral1 = 0") # BF10 = 1/11.04 = 0.09
hypothesis(S1_Bayesian_Obj, "Istrue1:IsMoral1 = 0")   # BF10 = 1/2.26 = 0.44
hypothesis(S1_Bayesian_Obj, "groupAccpriming:Istrue1:IsMoral1 = 0")  # BF10 = 1/1.97 = 0.51
hypothesis(S1_Bayesian_Obj, "groupMoralpriming:Istrue1:IsMoral1 = 0") # BF10 = 1/4.4 = 0.22



########## get effect size for objective model, Cohen's d = coefficient / sigma ##########
draws <- as.data.frame(S1_Bayesian_Obj)

Cohend_groupAccpriming = mean(draws$b_groupAccpriming/draws$sigma)
Cohend_groupMoralpriming = mean(draws$b_groupMoralpriming/draws$sigma)
Cohend_Istrue1 = mean(draws$b_Istrue1/draws$sigma)
Cohend_IsMoral1 = mean(draws$b_IsMoral1/draws$sigma)
Cohend_groupAcc_true = mean(draws$`b_groupAccpriming:Istrue1`/draws$sigma)
Cohend_groupMor_true = mean(draws$`b_groupMoralpriming:Istrue1`/draws$sigma)
Cohend_groupAcc_Moral = mean(draws$`b_groupAccpriming:IsMoral1`/draws$sigma)
Cohend_groupMor_Morl = mean(draws$`b_groupMoralpriming:IsMoral1`/draws$sigma)
Cohend_true_Mor = mean(draws$`b_Istrue1:IsMoral1`/draws$sigma)
Cohend_groupAcc_true_Moral  = mean(draws$`b_groupAccpriming:Istrue1:IsMoral1`/draws$sigma)





# #### Subjective model analysis, where some independent variable were rating by participants which would be different across subjs ####
### FULL MODEL
fit3 <-
  brm(data = data_Study1,
      family = gaussian,
      formula = Sharewilling.z ~ 1 + group * (rating_accuracy.z + rating_morality.z + rating_difficulty.z + rating_familiarity.z + rating_importance.z) 
      + (1 + rating_accuracy.z + rating_morality.z + rating_difficulty.z + rating_familiarity.z + rating_importance.z| Subject) + 
        (1 + group * (rating_accuracy.z + rating_morality.z + rating_difficulty.z + rating_familiarity.z + rating_importance.z) | item_label),
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
      control = list(adapt_delta = 0.999,   ### increasing adapt_delta can decrease divergent
                     max_treedepth = 20),   ### increasing max_treedepth can decrease divergent
      sample_prior = TRUE,  ### necessary for getting Bayes factor
      file = "Study1_Bayesian_Subjective_FULL.rds")



fit3 <-  readRDS("Study1_Bayesian_Subjective_FULL.rds")




########## get bayes factor (for subjective model) ########## 
## it is a tip to store your hypothesis into a vector ##
hypothesis_n <- c('groupAccpriming = 0', 'groupMoralpriming=0',
                  'rating_accuracy.z=0','rating_morality.z=0','rating_difficulty.z=0','rating_familiarity.z=0','rating_importance.z=0',
                   'groupAccpriming:rating_accuracy.z=0','groupMoralpriming:rating_accuracy.z=0',
                  'groupAccpriming:rating_morality.z=0','groupMoralpriming:rating_morality.z=0',
                  'groupAccpriming:rating_difficulty.z=0','groupMoralpriming:rating_difficulty.z=0',
                  'groupAccpriming:rating_familiarity.z=0','groupMoralpriming:rating_familiarity.z=0',
                  'groupAccpriming:rating_importance.z=0','groupMoralpriming:rating_importance.z=0'
)
q <- hypothesis(fit3,hypothesis_n)






########## get effect size for subjective model, Cohen's d = coefficient / sigma ##########

draws2 <- as.data.frame(fit3)

Cohend_groupMoralpriming = mean(draws2$b_groupMoralpriming/draws2$sigma)
Cohend_rating_morality.z = mean(draws2$b_rating_morality.z/draws2$sigma)
Cohend_rating_importance.z = mean(draws2$b_rating_importance.z/draws2$sigma)
Cohend_Accgroup_credibility  = mean(draws2$`b_groupAccpriming:rating_accuracy.z`/draws2$sigma)
Cohend_Accgroup_Importance = mean(draws2$`b_groupAccpriming:rating_importance.z`/draws2$sigma)
Cohend_Moralgroup_morality = mean(draws2$`b_groupMoralpriming:rating_morality.z`/draws2$sigma)




### interesting result 

### Acc priming group: weigh on credibility - weigh on morality
hypothesis(fit3, 'rating_accuracy.z+groupAccpriming:rating_accuracy.z*1 = 0')
hypothesis(fit3, 'rating_morality.z+groupAccpriming:rating_morality.z*1 = 0')

hypothesis(fit3, "rating_accuracy.z+groupAccpriming:rating_accuracy.z*1  - (rating_morality.z+groupAccpriming:rating_morality.z*1)= 0")   
hypothesis(fit3, "abs(rating_accuracy.z+groupAccpriming:rating_accuracy.z*1)  - abs(rating_morality.z+groupAccpriming:rating_morality.z*1)= 0")   



### Moral priming group: weigh on credibility - weigh on morality
hypothesis(fit3,'rating_accuracy.z+groupMoralpriming:rating_accuracy.z*1 = 0')
hypothesis(fit3,'rating_morality.z+groupMoralpriming:rating_morality.z*1 = 0')

hypothesis(fit3, "rating_accuracy.z+groupMoralpriming:rating_accuracy.z*1  - (rating_morality.z+groupMoralpriming:rating_morality.z*1)  = 0") 
hypothesis(fit3, "abs(rating_accuracy.z+groupMoralpriming:rating_accuracy.z*1)  - abs(rating_morality.z+groupMoralpriming:rating_morality.z*1)= 0")   


### Control group: weigh on credibility - weigh on morality
hypothesis(fit3,'rating_accuracy.z = 0')
hypothesis(fit3,'rating_morality.z = 0')

hypothesis(fit3, "rating_accuracy.z - rating_morality.z  = 0")   
hypothesis(fit3, "abs(rating_accuracy.z)  - abs(rating_morality.z)= 0")   



### Acc priming group: weigh on credibility, morality, importance, familiarity, difficulty
hypothesis(fit3, "rating_accuracy.z+ groupAccpriming:rating_accuracy.z*1 = 0")   
hypothesis(fit3, "rating_morality.z+ groupAccpriming:rating_morality.z*1 = 0") 
hypothesis(fit3, "rating_importance.z+groupAccpriming:rating_importance.z*1 = 0")   
hypothesis(fit3, "rating_familiarity.z+groupAccpriming:rating_familiarity.z*1 = 0")   
hypothesis(fit3, "rating_difficulty.z+groupAccpriming:rating_difficulty.z*1 = 0")   



### Acc priming group total weigh (weigh1 + weigh2 + weigh3 + weigh4 + weigh5) - control group total weigh(weigh1 + weigh2 + weigh3 + weigh4 + weigh5)
hypothesis(fit3,'(rating_accuracy.z+ groupAccpriming:rating_accuracy.z*1+
                 rating_morality.z+ groupAccpriming:rating_morality.z*1+
                 rating_importance.z+groupAccpriming:rating_importance.z*1+
                 rating_familiarity.z+groupAccpriming:rating_familiarity.z*1+
                 rating_difficulty.z+groupAccpriming:rating_difficulty.z*1) -
                 (rating_accuracy.z+rating_morality.z+rating_importance.z+rating_familiarity.z+rating_difficulty.z)
                 =0
           ')


### Acc priming group: weigh on credibility, morality, importance, familiarity, difficulty, get a absolute value
hypothesis(fit3, "abs(rating_accuracy.z+ groupAccpriming:rating_accuracy.z*1) = 0")   
hypothesis(fit3, "abs(rating_morality.z+ groupAccpriming:rating_morality.z*1) = 0") 
hypothesis(fit3, "abs(rating_importance.z+groupAccpriming:rating_importance.z*1) = 0")   
hypothesis(fit3, "abs(rating_familiarity.z+groupAccpriming:rating_familiarity.z*1) = 0")   
hypothesis(fit3, "abs(rating_difficulty.z+groupAccpriming:rating_difficulty.z*1) = 0")  



### Acc priming group total weigh (|weigh1| + |weigh2| + |weigh3| + |weigh4| + |weigh5|) - control group total weigh(|weigh1| + |weigh2| + |weigh3| + |weigh4| + |weigh5|)
hypothesis(fit3,'(abs(rating_accuracy.z+ groupAccpriming:rating_accuracy.z*1)+
                 abs(rating_morality.z+ groupAccpriming:rating_morality.z*1)+
                 abs(rating_importance.z+groupAccpriming:rating_importance.z*1)+
                 abs(rating_familiarity.z+groupAccpriming:rating_familiarity.z*1)+
                 abs(rating_difficulty.z+groupAccpriming:rating_difficulty.z*1)) -
                 (abs(rating_accuracy.z)+abs(rating_morality.z)+abs(rating_importance.z)+
                 abs(rating_familiarity.z)+abs(rating_difficulty.z)) = 0
           ')


### Morality priming group: weigh on credibility, morality, importance, familiarity, difficulty
hypothesis(fit3, "rating_accuracy.z+ groupMoralpriming:rating_accuracy.z*1 = 0")   
hypothesis(fit3, "rating_morality.z+ groupMoralpriming:rating_morality.z*1 = 0") 
hypothesis(fit3, "rating_importance.z+groupMoralpriming:rating_importance.z*1 = 0")   
hypothesis(fit3, "rating_familiarity.z+groupMoralpriming:rating_familiarity.z*1 = 0")   
hypothesis(fit3, "rating_difficulty.z+groupMoralpriming:rating_difficulty.z*1 = 0")   



### Morality priming group total weigh (weigh1 + weigh2 + weigh3 + weigh4 + weigh5) - control group total weigh(weigh1 + weigh2 + weigh3 + weigh4 + weigh5)
hypothesis(fit3,'(rating_accuracy.z+ groupMoralpriming:rating_accuracy.z*1+
                 rating_morality.z+ groupMoralpriming:rating_morality.z*1+
                 rating_importance.z+groupMoralpriming:rating_importance.z*1+
                 rating_familiarity.z+groupMoralpriming:rating_familiarity.z*1+
                 rating_difficulty.z+groupMoralpriming:rating_difficulty.z*1) -
                 (rating_accuracy.z+rating_morality.z+rating_importance.z+rating_familiarity.z+rating_difficulty.z)
                 =0
           ')




### Morality priming group: weigh on credibility, morality, importance, familiarity, difficulty, get a absolute value
hypothesis(fit3, "abs(rating_accuracy.z+ groupMoralpriming:rating_accuracy.z*1) = 0")   
hypothesis(fit3, "abs(rating_morality.z+ groupMoralpriming:rating_morality.z*1) = 0") 
hypothesis(fit3, "abs(rating_importance.z+groupMoralpriming:rating_importance.z*1) = 0")   
hypothesis(fit3, "abs(rating_familiarity.z+groupMoralpriming:rating_familiarity.z*1) = 0")   
hypothesis(fit3, "abs(rating_difficulty.z+groupMoralpriming:rating_difficulty.z*1) = 0")   




# ### Morality priming group total weigh (|weigh1| + |weigh2| + |weigh3| + |weigh4| + |weigh5|) - control group total weigh(|weigh1| + |weigh2| + |weigh3| + |weigh4| + |weigh5|)
#   hypothesis(fit3,'(abs(rating_accuracy.z+ groupMoralpriming:rating_accuracy.z*1)+
#                     abs(rating_morality.z+ groupMoralpriming:rating_morality.z*1)+
#                     abs(rating_importance.z+groupMoralpriming:rating_importance.z*1)+
#                     abs(rating_familiarity.z+groupMoralpriming:rating_familiarity.z*1)+
#                     abs(rating_difficulty.z+groupMoralpriming:rating_difficulty.z*1)
#                     ) -
#                     (abs(rating_accuracy.z)+abs(rating_morality.z)+abs(rating_importance.z)+
#                    abs(rating_familiarity.z)+abs(rating_difficulty.z)) = 0
#   
#              ')


