

X = read.csv2("data/accuracy_of_models.csv", sep = ",")
X
head(X)

X$Base1 <- as.numeric(X$Base1)
X$Base2 <- as.numeric(X$Base2)
X$Base3 <- as.numeric(X$Base3)

X$Log1 <- as.numeric(X$Log1)
X$Log2 <- as.numeric(X$Log2)
X$Log3 <- as.numeric(X$Log3)

X$Rand1 <- as.numeric(X$Rand1)
X$Rand2 <- as.numeric(X$Rand2)
X$Rand3 <- as.numeric(X$Rand3)



t.test(X$Base1, X$Log1)
t.test(X$Base1, X$Rand1)
t.test(X$Log1, X$Rand1)

par(mfrow = c(1,1))
plot(1:8, X$Log1, type = 'l')
lines(1:8, X$Base1)
lines(1:8, X$Rand1)

