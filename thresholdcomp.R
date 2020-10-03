setwd('C:/Users/chloe/Desktop/Masters/stats/Threshold')
library(sciplot) #for bargraph.CI
library(multcomp) 
library(ade4)
library(vegan)
library(ggplot2)
library('lme4')
library(optimx) #the optimiser used
library(AICcmodavg) #used for aic comparison
library(ggeffects)
library(tidyverse)

# to check for missing values    which(is.na(dataset)==T)

thresholdcomp <- read.csv("thresholdcomp.csv", header = T, sep = ",")
str(thresholdcomp)

#mean.count<-tapply(thresholdcomp$response, list(thresholdcomp$stimulus), groups = thresholdcomp$treatment, mean)
#mean.count
#plot(mean.count.whole,type="l",col="white")
#points(mean.count.whole,col="red")
#points(mean.count.valve,col="green")
#points(mean.count.girdle,col="blue")
#comparison on responses on same graph. lines() adds another object to the existing graph



#graphs
#stimmult <-thresholdcomp$response*100
#stimorder <-factor(thresholdcomp$stimulus, levels = unique(thresholdcomp$stimulus)) #to make bars in the order found in csv file

#bargraph.CI(stimorder, stimmult, group = treatment, legend=T, data=thresholdcomp, ylim=c(0,100), xlab = "Stimulus", ylab = "Shadow Responses Elicited (%)")
#ggplot(thresholdcomp, aes(x=stimorder, y=response, fill = treatment)) + geom_boxplot() + labs(x="Stimulus", y = "Shadow Responses Elicited")
#ggplot(thresholdcomp, aes(x=stimorder, y=response, fill = treatment)) + geom_violin() + labs(x="Stimulus", y = "Shadow Responses Elicited") #John recommended a violin plot.

#thresholdcomp %>% 
#  ggplot(aes(x = factor(stimulus),fill = response)) + 
#  geom_bar(position = "fill")



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

predz <- ggpredict(compmodel.intslope, 
                   terms = c('stimulus [n=500]','treatment') 
) 

predz %>% plot()

predz_plot <- plot(predz) + labs(x = "Stimulus", y = "Predicted Response") + scale_x_continuous(limits = c(-1,0.1))



fixed <- fixef(compmodel.intslope) 
fixed

X  <- seq(from=-1,to=0.2,by=.02)
y_control <- pnorm( fixed[2]*X + fixed[1])
plot(X,y_control,main='Whole (control) stimulus')
lines(X,y_control, col=white)


plot(X,y_control,main='Response to stimuli',col="white",xlim=c(-1,0.2))
lines(X,y_control,col="red")
points(mean.count.whole, col="red")

y_valve <- pnorm( (fixed[2] + fixed[6])*X + (fixed[1] + fixed[4]) )
lines(X,y_valve,col="green")

y_girdle <- pnorm( (fixed[2] + fixed[5])*X + (fixed[1] + fixed[3]) )
lines(X,y_girdle,col="blue")





#abline 

#If this all looks ok, then please can I have some help interpreting the output of this. 
#What I think I can say: there's an effect of treatment because there the best model included an interaction between the stimuli
#the individual has a strong (I think) effect and length does but less so. I'm not sure how to interpret the intercept and stimulus part.
#I don't know what's going on with the correlation of fixed effects or the fixed effects.



#summary(glht(compmodel.5, mcp(stimulus_treat="Tukey")))
#compmodel.4<- brm(response ~ stimulus + (1|length) + (1|individual), data = thresholdcomp, family = binomial())  #  + (1|stim_order)  # + (1|length)



############ building the graph with predz and means of each

controlpredz <- subset(predz, group == "Control")
girdlepredz <- subset(predz, group == "Girdle")
valvepredz <- subset(predz, group == "Valve")

xgirdle <- girdlepredz$x
ygirdle <- girdlepredz$predicted
girdlemin <- girdlepredz$conf.low
girdlemax <- girdlepredz$conf.high
xmgirdle <- c(-0.97,-0.96,-0.9,-0.73,-0.52,-0.26,-0.03,0) # stimulus treatments. Girdle has an extra one -0.73 compared to the others.
ymgirdle <- c(0.95986622, 0.81012658, 0.77021277, 0.66530612, 0.52343750, 0.17241379, 0.02348993, 0.01672241) #mean response rates

xvalve <-valvepredz$x
yvalve <- valvepredz$predicted
valvemin <- valvepredz$conf.low
valvemax <- valvepredz$conf.high
xmvalve <- c(-0.97,-0.96, -0.9,-0.52, -0.26, -0.03, 0)
ymvalve <- c(0.97765363,0.91304348,0.88554217,0.86206897, 0.75428571, 0.10169492, 0.05555556)

xwhole <- controlpredz$x
ywhole <- controlpredz$predicted
wholemin <- controlpredz$conf.low
wholemax <- controlpredz$conf.high
xmwhole <-c(-0.97, -0.96, -0.9, -0.52, -0.26, -0.03, 0) 
ymwhole <- c(0.96111111, 0.98333333, 0.97777778, 0.95555556, 0.96111111, 0.36111111, 0.03333333)


ggplot() + geom_line(data = girdlepredz, aes(xgirdle,ygirdle), colour = "black", size = 1) + 
  geom_ribbon(aes(x = xgirdle, y=ygirdle,ymin=girdlemin, ymax=girdlemax), linetype=2, alpha=0.1, fill = "black") +
  geom_point(aes(x = xmgirdle, y = ymgirdle), colour = "black", size = 3) +
  geom_vline(aes(xintercept = -0.574), colour = "black") +
  geom_line(data = valvepredz, aes(xvalve,yvalve), colour = "orange", size = 1) +
  geom_point(aes(x = xmvalve, y = ymvalve), colour = "orange", size = 3) +
  geom_ribbon(aes(x = xvalve, y=yvalve,ymin=valvemin, ymax=valvemax), linetype=2, alpha=0.1, fill = "orange") +
  geom_vline(aes(xintercept = -0.206), colour = "orange") +
  geom_line(data = controlpredz, aes(xwhole,ywhole), colour = "blue", size = 1) +
  geom_point(aes(x = xmwhole, y = ymwhole), colour = "blue", size = 3) +
  geom_ribbon(aes(x = xwhole, y=ywhole,ymin=wholemin, ymax=wholemax), linetype=2, alpha=0.1, fill = "blue") +
  geom_vline(aes(xintercept = -0.134), colour = "blue") +
  theme_classic() + labs(x = "Weber's Contrast") + 
  scale_y_continuous("Predicted Shadow Responses", sec.axis = sec_axis(~ . * 100, name = "Shadow Responses Elicited (%)")
  )



