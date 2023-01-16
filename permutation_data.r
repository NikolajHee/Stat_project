


X = read.csv2("data/accuracy_of_models.csv", sep = ",")
X$Base1 <- as.numeric(X$Base1)
X$Base2 <- as.numeric(X$Base2)
X$Base3 <- as.numeric(X$Base3)

X$Log1 <- as.numeric(X$Log1)
X$Log2 <- as.numeric(X$Log2)
X$Log3 <- as.numeric(X$Log3)

X$Rand1 <- as.numeric(X$Rand1)
X$Rand2 <- as.numeric(X$Rand2)
X$Rand3 <- as.numeric(X$Rand3)

head(X)


################################################################## PERMUTATION TEST
## ## ## ## ## ## permutation test for logistic regression
# data 1 og 2
compare_1 <- X$Log1
compare_2 <- X$Log2

tobs <- mean(compare_1) - mean(compare_2)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(compare_1,compare_2)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val # significant

# data 1 og 3
compare_1 <- X$Log1
compare_2 <- X$Log3

tobs <- mean(compare_1) - mean(compare_2)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(compare_1,compare_2)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*(t0)
p_val # significant

# data 2 og 3
compare_1 <- X$Log2
compare_2 <- X$Log3

tobs <- mean(compare_1) - mean(compare_2)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(compare_1,compare_2)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*(1 - t0)
p_val # significant

###############################################
## ## ## ## ## ## ## ## Random Forest classifier
mean(X$Rand1)
mean(X$Rand2)
mean(X$Rand3)


# data 1 og 2
compare_1 <- X$Rand1
compare_2 <- X$Rand2

tobs <- mean(compare_1) - mean(compare_2)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(compare_1,compare_2)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*t0
p_val # significant

# data 1 og 3
compare_1 <- X$Rand1
compare_2 <- X$Rand3

tobs <- mean(compare_1) - mean(compare_2)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(compare_1,compare_2)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*(t0)
p_val # significant

# data 2 og 3
compare_1 <- X$Rand2
compare_2 <- X$Rand3

tobs <- mean(compare_1) - mean(compare_2)

n = 100000

save = numeric(n)

for (i in 1:n) {
index = sample(16, replace = FALSE)
z = c(compare_1,compare_2)
z_perm = z[index]
save[i] = mean(z_perm[9:16]) - mean(z_perm[1:8])
}
hist(save)
abline(v = tobs)

t0 <- sum(tobs < save)/n
p_val <- 2*(1 - t0)
p_val # significant

