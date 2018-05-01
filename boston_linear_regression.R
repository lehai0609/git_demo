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
  #Interpret Model1
  #F-statistic=115.1>>1 & p-value << 0: There's a relationship between Dependent & Independent variables 
  #p-value on each var: Small p-val <=> significant
  #R-squared val closer to 1 <=> explain more

#Building Model2 remove insignificant variables
model2 <- lm(log(medv) ~ .-zn-indus-age, data = train)
summary(model2)

par(mfrow = c(2,2))
plot(model2)

#Building Model3 with non-linearity
model3 = lm(log(medv)~crim+chas+nox+rm+dis+rad+tax+ptratio+
              black+lstat+ I(crim^2)+ I(chas^2)+I(nox^2)+ I(rm^2)+ I(dis^2)+ 
              I(rad^2)+ I(tax^2)+ I(ptratio^2)+ I(black^2)+ I(lstat^2), data=train)
summary(model3)

#Update Model3 by removing non-necessary variables
model4=update(model3, ~.-nox-rad-tax-I(crim^2)-I(chas^2)-I(rad^2)-
                I(tax^2)-I(ptratio^2)-I(black^2))
summary(model4)

#Predict test using Model4
pred1 <- predict(model4, newdata = test)
plot(test$medv, exp(pred1))
