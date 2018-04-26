#Libraries/Working directories
library(ggplot2)
setwd("C:/Users/Devon/Desktop")

#Read the data from Excel into R and show a summary
table1 <- read.csv("birthweight_smoking.csv")
summary(table1)

#Isolate the binary vector and transform into categories for plotting
smokers <- table1$smoker
factor_smokers <- factor(smokers)
levels(factor_smokers) <- c("Non-smoker", "Smoker")
factor_smokers

#Scatterplot of variables age and birthweight. Categorized by smoker/non-smokers
graph1 <- ggplot(data = table1, aes(x=age, y=birthweight,color = factor_smokers))+
  geom_point(alpha=0)+
  geom_smooth(se=FALSE)
print(graph1)

#Quick statistics of the differences between age and birthweight
summary(factor_smokers)
mean(subset(table1, smoker == 1)$birthweight)
mean(subset(table1, smoker == 0)$birthweight)

#Regression Model (Note: Not all of the variables were used)
fit <- lm(birthweight ~ age + nprevist + alcohol + tripre1 + tripre2 + tripre3 +
            smoker + unmarried + educ, 
          data=table1)
fit

#Displaying more statistics based on the regression model stored into fit
names(summary(fit))
summary(fit)$r.squared
summary(fit)$coefficients
