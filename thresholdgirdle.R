setwd('C:/Users/chloe/Desktop/Masters/stats/Threshold')
library(sciplot)
library(multcomp)
library(ade4)
library(vegan)
library(ggplot2)
library('lme4')



thresholdgirdle <- read.csv("thresholdgirdle.csv", header = T, sep = ",")
str(thresholdgirdle)

mean.count <- tapply(thresholdgirdle$response, list(thresholdgirdle$stimulus), mean)
mean.count

girdlemodel.4<- glmer(response ~ stimulus + (1|stimulus_order) + (1|length) + (1|individual), data = thresholdgirdle, family = binomial(), control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                                                                                                  optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
girdlemodel.0000<- glmer(response ~ 1 + (1|stimulus_order) + (1|length) + (1|individual), data = thresholdgirdle, family = binomial(), control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                                                                                              optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(girdlemodel.4)
anova(girdlemodel.0000,girdlemodel.4)
#summary(glht(girdlemodel.4, mcp(stimulus="Tukey")))
predict(girdlemodel.4, interval = 'confidence', newdata = with(thresholdgirdle, unique(stimulus)))

plot(mean.count)

fitted <-ggpredict(girdlemodel.4, "stimulus") #I found this and presumed it was analagous to the brms marginal_effects thigny you used.
ddd <- as.vector(fitted$predicted) 
eee <- as.vector(fitted$x)


#### get smoothed curve
lo <- loess(ddd~eee, span=0.5)
plot(eee,ddd)
lines(predict(lo), col='red', lwd=2)
cc <- as.numeric(lo$x)
dd <- as.numeric(lo$fitted)
lo.df <- cbind.data.frame(cc,dd)
#data <- lo.df[order(lo.df$x), ]
derivative = c(diff(lo.df$dd) / diff(lo.df$cc), 0)
find.df <- cbind.data.frame(cc,derivative)
ggplot(find.df, aes(cc, derivative)) + geom_line()
max_deriv <- which(derivative==max(derivative))
inflection_pt_rads <- cc[max_deriv]
inflection_pt_degs <- inflection_pt_rads*180/pi
inflection_pt_degs
