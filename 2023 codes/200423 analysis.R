
#install.packages("readstata13")
library(readstata13)
library(data.table)
library(zoo)
library(dplyr)
library(lubridate)
library(dplyr)
library(gmodels)
library(ggplot2)  
library(xtable)
library(statar)
library(lme4)
library(lfe)
library(stargazer)



wd_data_cleaned <-"C:/Users/edika/Desktop/GITHUB/Dominance training/Coalescing-Training/2023 cleaned"



setwd(wd_data_cleaned)


dat <- read.dta13("Data_experiment_cleaned_IP_masked.dta", nonint.factors = TRUE)

names(dat)
dat<-data.table(dat)

# Each obs in the data corresponds to a participant x trial
# order_of_trial indicates the order of the trial. The 2nd trial is part of the training

# Conditions
# ------------
# We have three conditions:
#   (A) Coalesced - Identical	1 trial G- vs G+ → Training G- vs G+ → 1 trial G- vs G+
#   (B) Coalesced - Different	1 trial F- vs F+ → Training F- vs F+ → 1 trial G- vs G+
#   (C) Transparent	1 trial GS- vs GS+

unique(dat[,.(condition, order_of_trial)])



# Violation of SD (at the trial level)
# -----------------------------------

# There are two gambles on each trial that participants need to choose. These are stored in the variables "coalesced_gamble" and "coalesced_gamble1".
# In the "response" column, selecting Gamble 0 corresponds to choosing "coalesced_gamble", and selecting Gamble 1 corresponds to choosing "coalesced_gamble1".

dat[response=="gamble0" & 
      (coalesced_gamble=="G_minus.jpeg" | 
         coalesced_gamble=="F_minus.jpeg" | 
         coalesced_gamble=="FS_minus.jpeg" | 
         coalesced_gamble=="GS_minus.jpeg"), violation_SD:=1 ]

dat[response=="gamble1" & 
      (coalesced_gamble1=="G_minus.jpeg" | 
         coalesced_gamble1=="F_minus.jpeg" | 
         coalesced_gamble1=="FS_minus.jpeg" |
         coalesced_gamble1=="GS_minus.jpeg"), violation_SD:=1 ]

dat[ is.na(violation_SD), violation_SD:=0]


dat[violation_SD==0,.N, by=.(coalesced_gamble, coalesced_gamble1 ,response)] 
dat[violation_SD==1,.N, by=.(coalesced_gamble, coalesced_gamble1 ,response)] 






# Creating records of violations of SD (at the participant level)
# violation of SD in trial 1
# (indicates whether the participant violated the SD in Trial 1)

dat[order_of_trial==1 & violation_SD==1 , violation_SD_trial1:=1]
dat[ , violation_SD_trial1:=sum(violation_SD_trial1, na.rm=T), by=.(sub_no)]

# violation of SD in during training
# (indicates whether the participant violated the SD in Trial 2)

dat[ order_of_trial==2 & violation_SD==1 , violation_SD_training:=1]
dat[ , violation_SD_training:=sum(violation_SD_training, na.rm=T), by=.(sub_no)]
dat[condition=="C" , violation_SD_training:=NA]

#***************************
# Tabulations across trials requested by JB
# violation of SD in trial 3
# (indicates whether the participant violated the SD in Trial 2)

dat[ order_of_trial==3 & violation_SD==1 , violation_SD_trial3:=1]
dat[ , violation_SD_trial3:=sum(violation_SD_trial3, na.rm=T), by=.(sub_no)]
dat[condition=="C" , violation_SD_trial3:=NA]

# Tabulating violations across trials
dat[order_of_trial==1,.N, by=.(violation_SD_trial1, violation_SD_training, violation_SD_trial3, condition)][order(condition)]

#***************************


dat[,.(order_of_trial, sub_no,violation_SD, violation_SD_trial1, violation_SD_training, violation_SD_trial3)][1:20]


# We focus on studying violations of SD during trials 1 and 3 (trial 2 is part of the training)

dat13<-dat[order_of_trial!=2]

dat13[ order_of_trial==1, .(.N, mean(violation_SD)), by=.(condition)]

# 436+435+438 = 1309 obs before training in conditions A, B and C



# Distinguishing gambles before/after training per condition
dat13[, any_training:=ifelse(any_training=="Choice w/o training", 0, 1)]

dat13[any_training==0 & gamble_played_condition=="(C) GS+ vs GS-" , new_condition_detailed:="C, GS+ vs GS-, no training"]
dat13[any_training==0 & gamble_played_condition=="(B) F+ vs F-", new_condition_detailed:="B, F+ vs F-, before training"]
dat13[any_training==1 & gamble_played_condition=="(B) G+ vs G-", new_condition_detailed:="B, G+ vs G-, after training"]
dat13[any_training==0 & gamble_played_condition=="(A) G+ vs G-", new_condition_detailed:="A, G+ vs G-, before training"]
dat13[any_training==1 & gamble_played_condition=="(A) G+ vs G-", new_condition_detailed:="A, G+ vs G-, after training"]



dat13[,.N, by=.(new_condition_detailed, gamble_played_condition)]


# Proportion of satisfaction of SD over gambles by training

dat13[, satisfaction_SD:=1-violation_SD]
dat13[any_training==0, .(mean(satisfaction_SD)) , by=.(new_condition_detailed)]
dat13[any_training==1, .(mean(satisfaction_SD)) , by=.(new_condition_detailed)]



#_______________________________________________________________________
#
# PLOTS before deleting outliers
#_______________________________________________________________________


# Proportion of violations of SD 
# ---------------------------------------------------------------------


levels_cond=c( "B, G+ vs G-, after training" ,
   "B, F+ vs F-, before training",
   "A, G+ vs G-, after training",
   "A, G+ vs G-, before training",
   "C, GS+ vs GS-, no training" )

data.plot <- 
  data.table(dat13[,.(violation_SD, new_condition_detailed )] %>%
  group_by( new_condition_detailed) %>% 
  dplyr::summarise(avg_PctPasses = ci.binom(violation_SD)[1], 
                   lci_PctPasses = ci.binom(violation_SD)[2], 
                   uci_PctPasses = ci.binom(violation_SD)[3]) )

data.plot[, new_condition_detailed2:=factor(new_condition_detailed, 
                                      levels=levels_cond)]

ggplot(data.plot    , aes_string(x = "new_condition_detailed2",
                               y = "avg_PctPasses",
                               ymin = "lci_PctPasses", ymax = "uci_PctPasses" #, fill="#56B4E9" #col"
                  )) + 
  geom_point(colour = "black", show.legend = FALSE, size=2.5) + 
  geom_linerange(colour = "black") +
  coord_flip() +
  theme_bw() +
  xlab("Condition & Gamble") + 
  ylab("Proportion of Violations of SD") +
  theme(text = element_text(, 
                            size=19, family="serif"   ),
        legend.position = "bottom",
        legend.title.align = .5 ,
        legend.text=element_text(size=19),
        panel.grid.minor = element_blank()  
  )


# Proportion of violations of SD for those who violates SD during training (before deleting outliers)
# ---------------------------------------------------------------------



data.plot <- 
  data.table(dat13[violation_SD_training==1,.(violation_SD, new_condition_detailed )] %>%
               group_by( new_condition_detailed) %>% 
               dplyr::summarise(avg_PctPasses = ci.binom(violation_SD)[1], 
                                lci_PctPasses = ci.binom(violation_SD)[2], 
                                uci_PctPasses = ci.binom(violation_SD)[3]) )
data.plot[, new_condition_detailed2:=factor(new_condition_detailed, 
                                          levels=levels_cond)]

ggplot(data.plot 
       , aes_string(x = "new_condition_detailed2",
                    y = "avg_PctPasses",
                    ymin = "lci_PctPasses", ymax = "uci_PctPasses" #, fill="#56B4E9" #col"
       )) + 
  geom_point(colour = "black", show.legend = FALSE, size=2.5) + 
  geom_linerange(colour = "black") +
  coord_flip() +
  theme_bw() +
  xlab("Condition & Gamble") + 
  ylab("Proportion of Violations of SD") +
  theme(text = element_text(, 
                            size=19, family="serif"   ),
        legend.position = "bottom",
        legend.title.align = .5 ,
        legend.text=element_text(size=19),
        panel.grid.minor = element_blank()  
  )



# Proportion of violations of SD for those who satisfied SD during training (before deleting outliers)
# ---------------------------------------------------------------------



data.plot <- 
  data.table(dat13[violation_SD_training==0,.(violation_SD, new_condition_detailed )] %>%
               group_by( new_condition_detailed) %>% 
               dplyr::summarise(avg_PctPasses = ci.binom(violation_SD)[1], 
                                lci_PctPasses = ci.binom(violation_SD)[2], 
                                uci_PctPasses = ci.binom(violation_SD)[3]) )
data.plot[, new_condition_detailed2:=factor(new_condition_detailed, 
                                          levels=levels_cond)]

ggplot(data.plot 
       , aes_string(x = "new_condition_detailed2",
                    y = "avg_PctPasses",
                    ymin = "lci_PctPasses", ymax = "uci_PctPasses" #, fill="#56B4E9" #col"
       )) + 
  geom_point(colour = "black", show.legend = FALSE, size=2.5) + 
  geom_linerange(colour = "black") +
  coord_flip() +
  theme_bw() +
  xlab("Condition & Gamble") + #ylim(0,.3)
  ylab("Proportion of Violations of SD") +
  theme(text = element_text(, 
                            size=19, family="serif"   ),
        legend.position = "bottom",
        legend.title.align = .5 ,
        legend.text=element_text(size=19),
        panel.grid.minor = element_blank()  
  )


# Toggle counts

sum_up(dat13[toggle_count>0,.(toggle_count)], d=T)

dat13[,.(training_version, training_received, order_of_trial, sub_no)][1:20]


# Deleting participants with duplicated IPs and outliers in RT
# -------------------------------------------------------------

# We created "duplicate_id" and "omit_ip", both equal to 1 for duplicated IPs as well as for outliers in reaction times

dat13[order(ip, sub_no, order_of_trial)][duplicate_id==1,.(duplicate_id,omit_ip, ip, sub_no, order_of_trial, condition)]

# We will exclude obs below/above the 5 and 95 percentiles on each condition
RT_stat<-data.table(dat13 %>% group_by(condition) %>% sum_up(RT, d=T))

dat13[condition=="A" & (RT<RT_stat[condition=="A"]$p5 | RT>RT_stat[condition=="A"]$p95) , delete_outlierRT:=1]
dat13[condition=="B" & (RT<RT_stat[condition=="B"]$p5 | RT>RT_stat[condition=="B"]$p95) , delete_outlierRT:=1]
dat13[condition=="C" & (RT<RT_stat[condition=="C"]$p5 | RT>RT_stat[condition=="C"]$p95) , delete_outlierRT:=1]

# But we exclude all obs from the participants with at least one outlier in RT
dat13[, delete_outlierRT:=sum(delete_outlierRT, na.rm =T), by=.(sub_no)]
dat13[,.N, by=.(delete_outlierRT, omit_ip)]


dat13<- dat13[delete_outlierRT==0 & omit_ip==0 ]

# counting subjects

dat13[order_of_trial==1,.N, by=.(condition, gamble_played)]

# 343 + 344 + 385= 1072 subjects before training in conditions A, B, and C

# Proportion of satisfaction of SD over gambles by training

dat13[any_training==0, .(mean(satisfaction_SD)) , by=.(new_condition_detailed)]
dat13[any_training==1, .(mean(satisfaction_SD)) , by=.(new_condition_detailed)]



#_______________________________________________________________________
#
# PLOTS after deleting outliers
#_______________________________________________________________________


# Proportion of violations of SD 
# ---------------------------------------------------------------------



data.plot <- 
  data.table(dat13[,.(violation_SD, new_condition_detailed )] %>%
               group_by( new_condition_detailed) %>% 
               dplyr::summarise(avg_PctPasses = ci.binom(violation_SD)[1], 
                                lci_PctPasses = ci.binom(violation_SD)[2], 
                                uci_PctPasses = ci.binom(violation_SD)[3]) )

data.plot[, new_condition_detailed2:=factor(new_condition_detailed, 
                                          levels=levels_cond)]

ggplot(data.plot    , aes_string(x = "new_condition_detailed2",
                                 y = "avg_PctPasses",
                                 ymin = "lci_PctPasses", ymax = "uci_PctPasses" #, fill="#56B4E9" #col"
)) + 
  geom_point(colour = "black", show.legend = FALSE, size=2.5) + 
  geom_linerange(colour = "black") +
  coord_flip() +
  theme_bw() +
  xlab("Condition & Gamble") + 
  ylab("Proportion of Violations of SD") +
  theme(text = element_text(, 
                            size=19, family="serif"   ),
        legend.position = "bottom",
        legend.title.align = .5 ,
        legend.text=element_text(size=19),
        panel.grid.minor = element_blank()  
  )




#_________________________________________________________
# 
# Chi2 test 
# violations of SD before training - comparisons against the control condition (i.e., transparent gambles GS)
#_________________________________________________________
t=table(dat13[any_training==0 & condition!="B", .(violation_SD, condition)])
chisq.test(t, correct = F)

dat13[any_training==0 & condition!="B", .(mean(violation_SD)), by=.(condition)]
# Pearson chi2(1) = 186.7571   Pr = 0.000


# Chi-squared test for independence of rows and columns
# Testing if proportions from two conditions are the same
#		The proportion of participants in A who violated was 0.68 whereas the proportion from the control group C was only 0.18. 
#		The difference in proportions is significant, χ²(1, N = 728) = 186.7571, p < 0.001.



t=table(dat13[any_training==0 & condition!="A", .(violation_SD, condition)])
chisq.test(t, correct = F)

dat13[any_training==0 & condition!="A", .(mean(violation_SD)), by=.(condition)]

#  Pearson chi2(1) = 206.1521   Pr = 0.000

#		The proportion of participants in B who violated was 0.71 whereas the proportion from the control group C was only 0.18. 
#		The difference in proportions is significant, χ²(1, N = 729) = 206.1521 , p < 0.001.


# Toggle counts

sum_up(dat13[toggle_count>0,.(toggle_count)], d=T) # max 40. But 19 is 99%

hist(dat13[!missing(training_version) & toggle_count>0 & toggle_count<=19]$toggle_count)



#_________________________________________________________
# 
# Regressions
# Computing the effect of the training on conditions A and B
#_________________________________________________________

# Fit a random-effects logistic regression model and a linear model with cluster SE
dat13[, new_condition_detailed:=factor(new_condition_detailed)]

model <- glmer(violation_SD ~ relevel(new_condition_detailed, ref = "A, G+ vs G-, before training") + (1 | sub_no),
               data = dat13[ condition!="C"], family = binomial())

summary(model)

model_lm <- felm(violation_SD ~ relevel(new_condition_detailed, ref = "A, G+ vs G-, before training") |0 | 0 | sub_no,
               data = dat13[ condition!="C"])

summary(model_lm)


# Computing the aggregate effect across gambles

model2 <- glmer(violation_SD ~ any_training + (1 | sub_no),
               data = dat13[ condition!="C"], family = binomial())

summary(model2)

model_lm2 <- felm(violation_SD ~ any_training |0 | 0 | sub_no,
                 data = dat13[ condition!="C"])

summary(model_lm2)

# Computing the effect of the number of clicks during training

dat13[,.(toggle_count_upto10)]

model3 <- glmer(violation_SD ~ factor(toggle_count_upto10) + (1 | sub_no),
                data = dat13[ condition!="C"], family = binomial())

summary(model3)

model_lm3 <- felm(violation_SD ~ factor(toggle_count_upto10) |0 | 0 | sub_no,
                  data = dat13[ condition!="C"])

summary(model_lm3)



stargazer(model_lm, model_lm2, model_lm3, type="text")


#_________________________________________________________
#
# Histogram of reaction times 
# (1) Across conditions
# (2) By violation or satisfaction of stochastic dominance 
#_________________________________________________________

hist(dat13$RT)

# (1) RT decreased following training
ggplot(dat13, aes(x = RT)) +
  geom_histogram(color = "black", fill = "white" ) +  theme_minimal() +
  facet_wrap(~ gamble_played_condition +any_training, ncol = 1,
             labeller = labeller(any_training = 
                                   c("0"="Training = 0", "1"="Training = 1")))

# (2) Violation of SD are related to shorter RT
ggplot(dat13, aes(x = RT)) +
  geom_histogram(color = "black", fill = "white" ) +  theme_minimal() +
  facet_grid(gamble_played_condition +any_training~violation_SD, #ncol = 2,
             labeller = labeller(any_training = 
                                   c("0"="Training = 0", "1"="Training = 1"),
                                 violation_SD = 
                                   c("0"="Violation SD = 0", "1"="Violation SD= 1")))










