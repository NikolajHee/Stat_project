############################################## loading
# directory
setwd("/Users/nikolaj/Desktop/main/stat/p r o j e c t")

# some info about data
horse.data <- read.table('horse_data23.txt')
head(horse.data) 
colnames(horse.data)
dim(horse.data)

# standardization

horse.data$A <- (horse.data$A - mean(horse.data$A))/sd(horse.data$A)
horse.data$W <- (horse.data$W - mean(horse.data$W))/sd(horse.data$W)
horse.data$S <- (horse.data$S - mean(horse.data$S))/sd(horse.data$S)

######## effect on either score
# we need horse as a factor (it's catagorical)
horse.data$horse <- as.factor(horse.data$horse)
horse.data$lameLeg <- as.factor(horse.data$lameLeg)

# boxplot of data:

boxplot(horse.data$A ~ horse.data$horse)
boxplot(horse.data$S ~ horse.data$horse)
S_symmetry_score


boxplot(horse.data$W ~ horse.data$horse)

for (i in ['B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B9']) {
print(horse.data$A[horse.data$horse == i])
}


qqnorm(horse.data$A)
qqline(horse.data$A)

qqnorm(horse.data$S)
qqline(horse.data$S)

qqnorm(horse.data$W)
qqline(horse.data$W)


bartlett.test(horse.data$A ~ horse.data$horse)
bartlett.test(horse.data$S ~ horse.data$horse)
bartlett.test(horse.data$W ~ horse.data$horse)



fligner.test(horse.data$A ~ horse.data$horse)
fligner.test(horse.data$S ~ horse.data$horse)
fligner.test(horse.data$W ~ horse.data$horse)

with(car)


#

# there is a bit difference in variiance,
# maybe some transformation is in order
boxplot((horse.data$S) ~ horse.data$horse)



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

##################################################
# test 2

anova(lm(A ~ lameLeg * horse, data = horse.data))

anova(lm(A ~ lameLeg + lameLeg : horse, data = horse.data))



anova(lm(S ~ lameLeg * horse, data = horse.data))


anova(lm(W ~ lameLeg * horse, data = horse.data))

anova(lm(W ~ lameLeg + horse, data = horse.data))

anova(lm(W ~ lameLeg , data = horse.data))
