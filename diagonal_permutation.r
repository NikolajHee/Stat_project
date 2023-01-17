setwd("/Users/nikolaj/Desktop/GIT/Stat_project/data")
X_class = read.csv2('accuracy_of_models_class_3.csv', sep = ',')

X_class$Base1 <- as.numeric(X_class$Base1)
X_class$Base2 <- as.numeric(X_class$Base2)
X_class$Base3 <- as.numeric(X_class$Base3)

X_class$Log1 <- as.numeric(X_class$Log1)
X_class$Log2 <- as.numeric(X_class$Log2)
X_class$Log3 <- as.numeric(X_class$Log3)

X_class$Rand1 <- as.numeric(X_class$Rand1)
X_class$Rand2 <- as.numeric(X_class$Rand2)
X_class$Rand3 <- as.numeric(X_class$Rand3)

X = read.csv2("accuracy_of_models.csv", sep = ",")

X$Base1 <- as.numeric(X$Base1)
X$Base2 <- as.numeric(X$Base2)
X$Base3 <- as.numeric(X$Base3)

X$Log1 <- as.numeric(X$Log1)
X$Log2 <- as.numeric(X$Log2)
X$Log3 <- as.numeric(X$Log3)

X$Rand1 <- as.numeric(X$Rand1)
X$Rand2 <- as.numeric(X$Rand2)
X$Rand3 <- as.numeric(X$Rand3)


qqnorm(X$Log1)
qqline(X$Log1)
qqnorm(X$Log2)
qqline(X$Log2)
qqnorm(X$Log3)
qqline(X$Log3)
qqnorm(X$Rand1)
qqline(X$Rand1)

# base 1

#t.test(X_class$Log1,X$Log1)

tobs <- mean(X_class$Base1) - mean(X$Base1)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(X_class$Base1, X$Base1)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val # significant

# base 2
tobs <- mean(X_class$Base2) - mean(X$Base2)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(X_class$Base2, X$Base2)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val # significant


# base 3

tobs <- mean(X_class$Base3) - mean(X$Base3)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(X_class$Log3, X$Log3)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val # significant


####################################################

# log 1

#t.test(X_class$Log1,X$Log1)

tobs <- mean(X_class$Log1) - mean(X$Log1)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(X_class$Log1, X$Log1)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val # significant

# log 2
tobs <- mean(X_class$Log2) - mean(X$Log2)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(X_class$Log2, X$Log2)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val # significant


# log 3

tobs <- mean(X_class$Log3) - mean(X$Log3)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(X_class$Log3, X$Log3)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val # significant

##########################

# foresst 1

tobs <- mean(X_class$Rand1) - mean(X$Rand1)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(X_class$Rand1, X$Rand1)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val # significant

# foresst 2

tobs <- mean(X_class$Rand2) - mean(X$Rand2)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(X_class$Rand2, X$Rand2)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val # significant


# foresst 3

tobs <- mean(X_class$Rand3) - mean(X$Rand3)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(X_class$Rand3, X$Rand3)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val # significant

