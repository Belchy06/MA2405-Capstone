library(dplyr)
library(reshape2)
library(ggplot2)
library(faraway)
library(tidyverse)

importedData <- read.csv(file = "diabetes.csv")

sapply(importedData, function(x) sum(is.na(x)))

summary(importedData)

cleanData <- importedData %>%
  rowwise() %>%
  filter(sum(c(Glucose, BloodPressure, SkinThickness, Insulin, BMI)) != 0)

cleanData <- filter(importedData, Glucose > 0, BloodPressure > 0, SkinThickness > 0, Insulin > 0, BMI > 0)

str(cleanData)

# Pairs
pairs(cleanData)

# Correlation heat map
cc = cor(cleanData, method = "pearson")
cc_df <- as.data.frame(cc)
cc_df$Vars = row.names(cc_df)
ccm = melt(cc_df, id = "Vars")
ccm$Vars <- factor(ccm$Vars, levels = row.names(cc_df))
ggplot(ccm, aes(x = variable, y = Vars)) + 
  geom_tile(aes(fill = value), colour = "grey45") + 
  coord_equal() + 
  geom_text(aes(label = round(value,2))) +
  scale_fill_gradient(low = "navy", high = "darkorange") + 
  theme(axis.text.y = element_text(face = "bold", colour = "grey25"), 
        legend.title = element_text(size = 10, face = "bold"),legend.position = "bottom", 
        axis.text.x = element_text(angle = 90, face = "bold",colour = "grey25", vjust = 0.5, hjust = 0), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = NA), 
        axis.ticks = element_blank()) + 
  labs(x= "", y = "", fill = "Pearson's Correlation") + 
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(ccm$Vars))) 


m1 <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = cleanData, family = binomial("logit"))
summary(m1)
plot(m1)

residuals(m1,type = "deviance") ## deviance residuals
plot(residuals(m1) ~ predict(m1,type="link"),xlab=expression(hat(eta)),ylab="Deviance residuals",pch=20,col="blue")


m2 <- glm(Outcome ~ Glucose + BMI + DiabetesPedigreeFunction, data = cleanData, family = binomial("logit"))
summary(m2)
