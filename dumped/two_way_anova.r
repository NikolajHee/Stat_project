

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



means <- colMeans(X)
means


data <- rep(c("A&W", "pc3,pc4", "A&W&pc3&pc4"), each = 3)
model <- rep(c("Base", "Log", "Rand"), 3)

df <- data.frame(data = data, model = model, acc = means[2:10])
df

df$model <- as.factor(df$model)

df$data <- as.factor(df$data)

anova(lm(acc ~ model + data, data = df))
anova(lm(acc ~ model, data = df))

summary(lm(acc ~ model + data, data = df))



df2 <- rbind(means[2:4], means[5:7], means[8:10])
colnames(df2) <- c('Base', 'Log', 'Rand')
rownames(df2) <- c('data_1', 'data_2', 'data_3')
df2

chisq.test(df2)


