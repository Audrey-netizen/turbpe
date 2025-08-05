#Here are what my acronyms mean!
#UPS - Unique Plant Species count
#UPSR - Unique Plant Species by replicate
#UBS - Unique Bee Species count
#UBSR - Unique bee species count by replicate
#TFC - total flower count
#TFCR - total flower count by replicate
#BA - bee abundance count
#BAR - bee abundance count by replicate

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

