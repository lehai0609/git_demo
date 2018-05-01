library(MASS)
library(ggplot2)
attach(Boston)
names(Boston)

#Split train-test data by 80-20
set.seed(1)
row.number <- sample(1:nrow(Boston), 0.8 * nrow(Boston))
train <- Boston[row.number,]
test <- Boston[-row.number,]

#Explore the dependent variables
ggplot(train, aes(medv)) +
  geom_density(fill = "blue")
ggplot(train, aes(log(medv))) + 
  geom_density(fill = "blue")
ggplot(train, aes(sqrt(medv))) +
  geom_density(fill = "blue")

#Building Model1 with all the variables
model1 <- lm(log(medv) ~ ., data = train)
summary(model1)

par(mfrow = c(2,2))
plot(model1)