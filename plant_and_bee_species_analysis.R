#Here are what my acronyms mean!
#UPS - Unique Plant Species count
#UPSR - Unique Plant Species by replicate
#UBS - Unique Bee Species count
#UBSR - Unique bee species count by replicate
#TFC - total flower count
#TFCR - total flower count by replicate
#BA - bee abundance count
#BAR - bee abundance count by replicate

#Final linear regression plots!

library(ggplot2)
library(ggpubr)

#plant diversity vs bee diversity
ggplot(bee_sp_r_data, aes(x = UPSR , y = UBSR , color = Type)) +
  geom_point() +
  labs(y = "# of Unique Bee Genera per Site and Replicate", x = "# of Unique Plant Species per Site and Replicate") +
  geom_smooth(method = "lm", se = TRUE,aes(fill = Type), linewidth = 1) +
  labs(title = "Linear Regression: Bee Genera Diversity ~  Plant Species Diversity + Site Type") +
  theme_minimal() +
  theme(text = element_text(size = 18)) +
  # Add the p-value text
  annotate(
    "text",
    x = -Inf, y = 8.5,
    label = "P values:\nBee Genera ~ Unique Plant Species: 5.97e-05\nBee Genera ~ Positive Control Site: 0.00224\nBee Genera ~ Treatment Site: 0.003",
    hjust = 0, vjust = 1,
    size = 5,
    color = "black")

#plant diversity vs soil composition
ggplot(plant_sp_r_data, aes(x = PC1, y = UPSR, color = Type)) +
  geom_point() +
  labs(y = "# of Unique Plant Species per Site and Replicate", x = "PC1") +
  geom_smooth(method = "lm", se = TRUE, aes(fill = Type), linewidth = 1) +
  labs(title = "Linear Regression: Plant Species Diversity ~ Soil Contamination + Site Type") +
  theme_minimal() +
  theme(text = element_text(size = 18)) +
  annotate(
    "text",
    x = -Inf, y = 9.5,
    label = "P values:\nPlant Diversity ~ Soil Contamination: 0.8166\nPlant Diversity ~ Positive Control Site: 0.000335\nPlant Diversity ~ Treatment Site: 5.64e-07",
    hjust = 0, vjust = 1,
    size = 5,
    color = "black")

#plant diversity vs bee abundance 
ggplot(bee_sp_r_data, aes(x = UPSR , y = BAR , color = Type)) +
  geom_point() +
  labs(y = "Bee Abundance per Site and Replicate", x = "# of Unique Plant Species per Site and Replicate") +
  geom_smooth(method = "lm", se = TRUE, aes(fill = Type),linewidth = 1) +
  labs(title = "Linear Regression: Bee Abundance ~ Plant Species Diversity + Site Type") +
  theme_minimal() +
  theme(text = element_text(size = 18)) +
  annotate(
    "text",
    x = -Inf, y = 30,
    label = "P values:\nBee Abundance ~ Unique Plant Species: 0.0417\nBee Abundance ~ Total Flower Count: 0.8610\nBee Abundance ~ Positive Control Site: 3.26e-05\nBee Abundance ~ Treatment Site: 0.011",
    hjust = 0, vjust = 1,
    size = 5,
    color = "black")

#linear models of different relationships 

#unique plant species
plant_sp_data <- read.csv("roadside_pollination_sites - Unique_Plant_Sp.csv", header=TRUE)
plant_sp_lm <- lm(UPS ~ PC1 + Type, data=plant_sp_data)
summary(plant_sp_lm)

#unique plant species by replicate
plant_sp_r_data <- read.csv("roadside_pollination_sites - Unique_Plant_Sp_R.csv", header=TRUE)
plant_sp_r_lm <- lm(UPSR ~ PC1 + Type, data=plant_sp_r_data)
summary(plant_sp_r_lm)

#unique bee species
bee_sp_data <- read.csv("roadside_pollination_sites - Unique_Bee_Sp.csv", header=TRUE)
bee_sp_lm <- lm(UBS ~ UPS + Type + TFC, data=bee_sp_data)
summary(bee_sp_lm)

#unique bee species by replicate
bee_sp_r_data <- read.csv("roadside_pollination_sites - Unique_Bee_Sp_R.csv", header=TRUE)
bee_sp_r_lm <- lm(UBSR ~ UPSR + Type + TFCR, data=bee_sp_r_data)
summary(bee_sp_r_lm)

#bee abundance
bee_abundance_data <- read.csv("roadside_pollination_sites - Unique_Bee_Sp.csv", header=TRUE)
bee_abundance_lm <- lm(BA ~ UPS + Type + TFC, data=bee_sp_data)
summary(bee_abundance_lm)

#bee abundance by replicate
bee_abundance_r_data <- read.csv("roadside_pollination_sites - Unique_Bee_Sp_R.csv", header=TRUE)
bee_abundance_r_lm <- lm(BAR ~ UPSR + TFCR + Type, data=bee_sp_r_data)
summary(bee_abundance_r_lm)

#misc.

cor(bee_sp_data$UBS, bee_sp_data$UPS) #0.48
cor(bee_sp_data$UBS, bee_sp_data$TFC) #0.5
cor(bee_sp_data$UPS, bee_sp_data$TFC) #0.6


#starts with UPSR as base
uniquesp_upsr <- lm(UBSR ~ UPSR, data=bee_sp_r_data)
uniquesp_tfcr <- lm(UBSR ~ UPSR + TFCR , data=bee_sp_r_data)
uniquesp_type <- lm(UBSR ~ UPSR + TFCR + Type, data=bee_sp_r_data)
anova(uniquesp_upsr, uniquesp_tfcr)
#p val 0.05188
#anova - function will output an F-statistic and p-value, indicating whether the addition of a block of predictors significantly improved the model's fit. 
anova(uniquesp_tfcr, uniquesp_type)
#p val 0.002544

#Res.Df - degrees of freedom of residuals
#RSS - residual sum of squares
#Df - number of additional parameters from model 1 to 2
# Sun of sq - difference in RSS between 2 models
#F - f statistic
#Pr(>F) - p val for F test telling us whether the additional predictor in the second model significantly improves model (should be less than 0.05)


