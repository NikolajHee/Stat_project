# loading
# directory
setwd("/Users/nikolaj/Desktop/main/stat/p r o j e c t")

# some info about data
horse.data <- read.table('horse_data23.txt')
head(horse.data) 
colnames(horse.data)
dim(horse.data)


################################## effect on either score
################################## 
################################## 
################################## 


# we need horse as a factor (it's catagorical)
horse.data$horse <- as.factor(horse.data$horse)
horse.data$lameLeg <- as.factor(horse.data$lameLeg)



########### first we do a lot of testing to test for the differnt assumptions

# levene's test
library(car)
leveneTest(data$A~data$horse)
leveneTest(data$W~data$horse)
leveneTest(data$S~data$horse)





# boxplot of data:
library(ggplot2)

ggplot(horse.data, aes(x=horse, y=A)) + 
    geom_boxplot(size=1) + theme(text = element_text(size = 25)) 
ggsave("boxplot1.png")


ggplot(horse.data, aes(x=horse, y=S)) + 
    geom_boxplot(size=1) + theme(text = element_text(size = 25)) 
ggsave("boxplot2.png")

ggplot(horse.data, aes(x=horse, y=W)) + 
    geom_boxplot(size=1) + theme(text = element_text(size = 25)) 
ggsave("boxplot3.png")



mynamestheme <- theme(
  plot.title = element_text(family = "Helvetica", face = "bold", size = (24)),
  legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Helvetica"),
  legend.text = element_text(face = "italic", family = "Helvetica"),
  axis.title = element_text(family = "Helvetica", size = (24)),
  axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (24)),
)

fit_A <- lm(A ~ horse, data = horse.data)
fit_S <- lm(S ~ horse, data = horse.data)
fit_W <- lm(W ~ horse, data = horse.data)

df_qq <- data.frame(fit_A = resid(fit_A), fit_S = resid(fit_S), fit_W = resid(fit_W))

ggplot(df_qq, aes(sample = fit_A)) + stat_qq() + stat_qq_line() +
    labs(title="QQ-plot (A)", x = 'Theoretical Quantiles', y = 'Sample Quantiles')+ mynamestheme
ggsave("QQ1.png")

ggplot(df_qq, aes(sample = fit_S)) + stat_qq() + stat_qq_line() +
    labs(title="QQ-plot (S)", x = 'Theoretical Quantiles', y = 'Sample Quantiles')+ mynamestheme
ggsave("QQ2.png")

ggplot(df_qq, aes(sample = fit_W)) + stat_qq() + stat_qq_line() +
    labs(title="QQ-plot (W)", x = 'Theoretical Quantiles', y = 'Sample Quantiles')+ mynamestheme
ggsave("QQ3.png")

   
# normality of residuals

qqnorm(resid(fit_A) )
qqline(resid(fit_A))

qqnorm(resid(fit_S))
qqline(resid(fit_S))

qqnorm(resid(fit_W))
qqline(resid(fit_W))


bartlett.test(horse.data$A ~ horse.data$horse)
bartlett.test(horse.data$S ~ horse.data$horse)
bartlett.test(horse.data$W ~ horse.data$horse)


fligner.test(horse.data$A ~ horse.data$horse)
fligner.test(horse.data$S ~ horse.data$horse)
fligner.test(horse.data$W ~ horse.data$horse)


#anova(lm(horse ~ A, data = horse.data)) 
anova(lm(A ~ horse, data = horse.data)) # not significant
anova(lm(S ~ horse, data = horse.data)) # not significant
anova(lm(W ~ horse, data = horse.data)) # not significant


kruskal.test(A ~ horse, data = horse.data) # not significant
kruskal.test(S ~ horse, data = horse.data) # not significant
kruskal.test(W ~ horse, data = horse.data) # not significant

# we do kruskal wallis tet with W, as it does not have
# equal variance. 




#######################################
# testing area
#######################################


# changing data

score <- rep(c('A', 'S', 'W'), each= 85)
horse <- rep(horse.data$horse, 3)
horse.newData <- data.frame(score_val = c(horse.data$A,horse.data$S,horse.data$W),
            score = score, horse = horse)

boxplot(horse.newData$score_val ~ horse.newData$score:horse.newData$horse)


fit <- lm(score_val ~ score * horse, data=horse.newData)
anova(fit)

fit <- lm(score_val ~ score + score:horse, data=horse.newData)
anova(fit)

##################################################
# test 2

anova(lm(A ~ lameLeg * horse, data = horse.data))

anova(lm(A ~ lameLeg + lameLeg : horse, data = horse.data))



anova(lm(S ~ lameLeg * horse, data = horse.data))


anova(lm(W ~ lameLeg * horse, data = horse.data))

anova(lm(W ~ lameLeg + horse, data = horse.data))

anova(lm(W ~ lameLeg , data = horse.data))
