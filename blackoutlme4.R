setwd('C:/Users/chloe/Desktop/Masters/stats/Blackout')
library(ggplot2)
library(optimx)
library(lme4)
library(multcomp)
library(ade4)
library(vegan)

blackout <- read.csv("blackout.csv", header = T, sep = ",")
str(blackout)


stimorder <-factor(blackout$stimulus, levels = unique(blackout$stimulus)) #to make bars in the order found in csv file
stimmult <-blackout$response*100

#used to make barplot
mean.count <- tapply(stimmult, list(stimorder), mean)
mean.count
sd.count <- tapply(stimmult, list(stimorder), sd)
sd.count
n.count <- tapply(stimmult, list(stimorder), length)
n.count
se.count <- sd.count/sqrt(n.count)
se.count
barplot(mean.count, ylim = c(0,100), xlab = xlab = "Stimulus", ylab = "Response (%)")
mids <- barplot(mean.count, ylim = c(0,100), xlab = "Stimulus", ylab = "Number of Responses (%)")
arrows (mids, mean.count-se.count, mids, mean.count+se.count,code = 3, angle = 90, length = 0.1)


#boxplot
subset <-subset(blackout, select=c("stimulus","response"))
summary(subset)
plot(subset)

ggplot(blackout, aes(x=stimorder, y=response)) + geom_boxplot() + labs(x="Stimulus", y = "Shadow Responses Elicited")
ggplot(blackout, aes(x=stimorder, y=response)) + geom_violin() + labs(x="Stimulus", y = "Shadow Responses Elicited") #John recommended a violin plot.


###### LME4 Analysis##########

model.5<- glmer(response ~ stimulus + (1|individual) + (1|length) + (1|stim_order), data = blackout, family = binomial(),      
                control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                       optCtrl = list(method = "nlminb", starttests = FALSE)))
model.null<- glmer(response ~ 1 + (1|individual) + (1|length) + (1|stim_order), data = blackout, family = binomial(),      
                   control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                          optCtrl = list(method = "nlminb", starttests = FALSE)))
anova(model.5, model.null) 
model.5
summary(glht(model.5, mcp(stimulus="Tukey"))) 


#analysis
# props <- with(blackout, aggregate(response, by  = list(individual = individual, stimulus = stimulus), mean))
# stripchart(x~stimulus*individual, data = props, vertical = T, cex = 0.5, ylab = 'Proportion Responding')
# 
# #to see if there's a size effect
# model.1 <- glmer(response ~ stimulus + (1|length), data = blackout, family = binomial())    # + (1|length)
# model.0 <- glmer(response ~ 1 + (1|length), data = blackout, family = binomial())
# summary(model.1) #exp(the log odds of responding to square instead of circle were 2.0840... found under fixed effects estimate column)
# anova(model.1, model.0)      ##the probability of this happening without really improving the model fit is p = 0.00003024 (taken from James' version and is Pr value), so we can use the model with an effect of stimulus to explain the data
# summary(glht(model.1, mcp(stimulus="Tukey"))) 
# 
# 
# #to see if there's an individual effect
# model.2 <- glmer(response ~ stimulus + (1|individual), data = blackout, family = binomial())    # + (1|length)
# model.00 <- glmer(response ~ 1 + (1|individual), data = blackout, family = binomial())
# anova(model.2, model.00) 
# 
# 
# #to see if there's a habituation effect
# model.3<- glmer(response ~ stimulus + (1|stim_order), data = blackout, family = binomial())    # + (1|length)
# model.000 <- glmer(response ~ 1 + (1|stim_order), data = blackout, family = binomial())
# anova(model.3, model.000) 
# model.000


