setwd('C:/Users/chloe/Desktop/Masters/stats/Threshold')
library(sciplot) #for bargraph.CI
library(multcomp) 
library(ade4)
library(vegan)
library(ggplot2)
library('lme4')
library(optimx) #the optimiser used
library(AICcmodavg) #used for aic comparison

# to check for missing values    which(is.na(dataset)==T)

thresholdcomp <- read.csv("thresholdcomp.csv", header = T, sep = ",")
str(thresholdcomp)


#graphs
#stimmult <-thresholdcomp$response*100
#stimorder <-factor(thresholdcomp$stimulus, levels = unique(thresholdcomp$stimulus)) #to make bars in the order found in csv file

#bargraph.CI(stimorder, stimmult, group = treatment, legend=T, data=thresholdcomp, ylim=c(0,100), xlab = "Stimulus", ylab = "Shadow Responses Elicited (%)")
#ggplot(thresholdcomp, aes(x=stimorder, y=response, fill = treatment)) + geom_boxplot() + labs(x="Stimulus", y = "Shadow Responses Elicited")
#ggplot(thresholdcomp, aes(x=stimorder, y=response, fill = treatment)) + geom_violin() + labs(x="Stimulus", y = "Shadow Responses Elicited") #John recommended a violin plot.





######## LME4 ANALYSIS ############
#props <- with(thresholdcomp, aggregate(response, by  = list(length = length, stimulus = stimulus), mean))
#stripchart(x~stimulus*length, data = props, vertical = T, cex = 0.5, ylab = 'Proportion Responding')


#if it says singular thing then it means there is not enough infomration because you have split the data up too much with effects
# if model not working, try changing  optimisation 

compmodel.5<- glmer(response ~ stimulus + treatment + (1|individual) + (1|length) + (1|stim_order), data = thresholdcomp, family = binomial("logit"),      
                    control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                          optCtrl = list(method = "nlminb", starttests = FALSE)))  #  + stimulus:treatment + (1|stim_order) + (1|length) + (1|individual)   # + (1|length)
compmodel.null <-glmer(response ~ treatment  + (1|individual) + (1|length) + (1|stim_order), data = thresholdcomp, family = binomial(), 
                       control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                          optCtrl = list(method = "nlminb", starttests = FALSE)))
compmodel.0 <-glmer(response ~ 1  + (1|individual) + (1|length) + (1|stim_order), data = thresholdcomp, family = binomial(), 
                    control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                           optCtrl = list(method = "nlminb", starttests = FALSE)))
compmodel.int <-glmer(response ~ stimulus*treatment  + (1|individual) + (1|length) + (1|stim_order), data = thresholdcomp, family = binomial(), 
                      control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                             optCtrl = list(method = "nlminb", starttests = FALSE)))
compmodel.intslope <-glmer(response ~ stimulus*treatment  + (1+stimulus|individual) + (1+stimulus|length) + (1|stim_order), data = thresholdcomp, family = binomial(), 
                      control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                             optCtrl = list(method = "nlminb", starttests = FALSE))) 
#putting (1+stimulus|random) effect tells the model that there may be differing intercepts based on the random effect, i.e. the baseline responses of the individual and those of differing sizes may be different.
#I didn't add to stim_order because it threw an error.
anova(compmodel.5, compmodel.null,compmodel.0,compmodel.int, compmodel.intslope) 
models <- list(compmodel.5, compmodel.null,compmodel.0,compmodel.int, compmodel.intslope)
model.names <- c('compmodel.5', 'compmodel.null','compmodel.0','compmodel.int', 'compmodel.intslope')
aictab(cand.set = models, modnames = model.names)
#based on this, the best model is the one with an interaction between stimulus and treatment that allow for different baseline responses in length and individual. It carries 100% of total explanation.


summary(compmodel.intslope)


#If this all looks ok, then please can I have some help interpreting the output of this. 
#What I think I can say: there's an effect of treatment because there the best model included an interaction between the stimuli
#the individual has a strong (I think) effect and length does but less so. I'm not sure how to interpret the intercept and stimulus part.
#I don't know what's going on with the correlation of fixed effects or the fixed effects.



#summary(glht(compmodel.5, mcp(stimulus_treat="Tukey")))
#compmodel.4<- brm(response ~ stimulus + (1|length) + (1|individual), data = thresholdcomp, family = binomial())  #  + (1|stim_order)  # + (1|length)
        