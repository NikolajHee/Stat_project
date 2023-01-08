############################################## loading
# directory
setwd("/Users/nikolaj/Desktop/main/stat/p r o j e c t")

# some info about data
horse.data <- read.table('horse_data23.txt')
head(horse.data) 
colnames(horse.data)
dim(horse.data)


######## effect on either score
# we need horse as a factor (it's catagorical)
horse.data$horse <- as.factor(horse.data$horse)

# boxplot of data:
boxplot(horse.data$A ~ horse.data$horse)
boxplot(horse.data$S ~ horse.data$horse)
boxplot(horse.data$W ~ horse.data$horse)




boxplot(horse.data$W ~ horse.data$horse)

qqnorm(horse.data$A)
qqline(horse.data$A)

qqnorm(horse.data$S)
qqline(horse.data$S)

qqnorm(horse.data$W)
qqline(horse.data$W)

# the different group looks fairly normal distributed based
# on the plot
# they overlap 

# there is a bit difference in variiance,
# maybe some transformation is in order
boxplot((horse.data$S) ~ horse.data$horse)



#anova(lm(horse ~ A, data = horse.data)) 
anova(lm(A ~ horse, data = horse.data)) # not significant
anova(lm(S ~ horse, data = horse.data)) # not significant
anova(lm(W ~ horse, data = horse.data)) # not significant

summary(lm(A ~ horse, data = horse.data))

#######################################
# test
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
