
setwd("/Users/nikolaj/Desktop/GIT/Stat_project/")
X = read.table('data/A_and_W.txt', col.names = c("Baseline", "Logistic", "RandomForest"))
X

par(mfcol = c(1,3))
hist(X$Baseline)
hist(X$Logistic)
hist(X$RandomForest)

qqnorm(X$Baseline)
qqline(X$Baseline)

qqnorm(X$Logistic)
qqline(X$Logistic)

qqnorm(X$RandomForest)
qqline(X$RandomForest)

# they are not that normal distributed


###################### PERMUTATION TEST
# permutation test betweenn logistic and baseline

tobs <- mean(X$Logistic) - mean(X$Baseline)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(X$Logistic, X$Baseline)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val # significant

# between randomforest and baseline

tobs <- mean(X$RandomForest) - mean(X$Baseline)

n = 10000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(X$RandomForest, X$Baseline)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val #significant

# between randomforest and logistic

tobs <- mean(X$RandomForest) - mean(X$Logistic)

n = 10000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(X$RandomForest, X$Logistic)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*(t0)
p_val # not signnificant


###################################### t.test

wilcox.test(X$Baseline, X$Logistic)
wilcox.test(X$Baseline, X$RandomForest)
wilcox.test(X$RandomForest, X$Logistic)



