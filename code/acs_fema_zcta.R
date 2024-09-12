library(ggplot2)
library(dplyr)
library(tidyr)#pivot_longer
library(scales)


setwd("H:/Shared drives/Papers 2024/D4 Hack")
ca_shape_w_suc_claims=read.csv("./Data/FEMA/ca_shape_w_suc_claims.csv")

Check=ca_shape_w_suc_claims[ca_shape_w_suc_claims$ZCTA5CE10==95969,]
Check$i_claim_suc


voter=read.csv("./Data/Census/ces_vars.csv")
voter
voter$ZIP

voter$SusQuantile=SES$SusQuantile[match(voter$ZIP,SES$ZCTA)]
sum(is.na(voter$SusQuantile))
voter=voter[!(is.na(voter$SusQuantile)),]


ca_shape_w_suc_claimsv2 <- ca_shape_w_suc_claims %>%
  group_by(ZCTA5CE10) %>%
  mutate(n_claims = n(),
         n_claims_suc = sum(i_claim_suc),
         p_claims_suc = n_claims_suc/n_claims) %>%
  filter(row_number() == 1)

summary(ca_shape_w_suc_claims$p_claims_suc)
summary(ca_shape_w_suc_claims$n_claims)
View(ca_shape_w_suc_claimsv2)
FEMA=ca_shape_w_suc_claimsv2

sum(FEMA$n_claims==1)/1763

sum(FEMA$n_claims<1)
FEMAsuc=FEMA[FEMA$p_claims_suc>0,]


names(FEMA)
FEMAunsuc=FEMA[FEMA$p_claims_suc==0,]
FEMAunsuc$n_claims
FEMA$n_claims_suc[FEMA$n_claims==15979]


1-(536/1763)
log10(100000)

hist(FEMA$p_claims_suc)
quantile(FEMAsuc$p_claims_suc, probs = seq(0, 1, by = 0.33), na.rm = TRUE)

quantile(FEMAsuc$p_claims_suc[FEMAsuc$n_claims>1], probs = seq(0, 1, by = 0.33), na.rm = TRUE)

FEMA <- FEMA %>%
  mutate(suc_quantile = cut(p_claims_suc,
                                     breaks = c(0,quantile(FEMAsuc$p_claims_suc, probs = seq(0, 1, by = 0.33), na.rm = TRUE)),
                                     include.lowest = TRUE,  # Ensures the lowest value is included
                                     labels = c("L1", "L2", "L3", "L4")))  # Labeling the quantiles
FEMA$suc_quantile
FEMA$ZCTA5CE10

SES = read.csv("./Data/ACS/SES variables ACS.csv")
any(is.na(FEMA$suc_quantile))
SES$ZCTA
SES$SusQuantile=FEMA$suc_quantile[match(SES$ZCTA,FEMA$ZCTA5CE10)]
sum(is.na(SES$SusQuantile))
SES=SES[!is.na(SES$SusQuantile),]
names(SES)
SES[,c(3:10)]=lapply(SES[,c(3:10)],as.numeric)


SES$MedIncome=SES$MedIncome+1
SES$Poverty=SES$Poverty+0.001
SES$Unemploy=SES$Unemploy+0.001
SES$NCitizen=SES$NCitizen+0.001
SES$LinIsolation=SES$LinIsolation+0.001
SES$EduLessHigh=SES$EduLessHigh+0.001
SES$EduLessCollege=SES$EduLessCollege+0.001

#Income
#MedIncome, Poverty, Unemploy

#Outsider
#NCitizen, LinIsolation

#Education
#EduLessHigh, EduLessCollege

#Health care
#ElderNoInsMedi

long_data <- SES %>%
  pivot_longer(cols = c(MedIncome, Poverty, Unemploy),  # Replace with your SES variable names
               names_to = "SES_variable", 
               values_to = "SES_value")


# Create a named vector with custom labels for each SES variable
ses_labels <- c(
  MedIncome = "Household Income",
  Poverty = "% Poverty",
  Unemploy = "% Unemployment"
)

options(scipen = 999)

# Create the boxplot with custom facet labels
P1=ggplot(long_data, aes(x = SusQuantile, y = SES_value)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outlier dots
  facet_grid(SES_variable ~ .,     # Facet by SES variable on the vertical axis (rows)
             scales = "free_y",    # Allow each panel to have its own y-axis scale
             labeller = labeller(SES_variable = ses_labels)) +  # Apply custom labels
  labs(title = "",
       x = "Level of sucessful claim rate",
       y = "Log scale") +
  scale_y_log10() +  # Log transformation applied to the y-axis
  theme_minimal() +
  theme(legend.position = "none",strip.text = element_text(size = 12))  # Remove legend if not needed
    # Increase the facet label size to 14
P1





long_data <- SES %>%
  pivot_longer(cols = c(NCitizen, LinIsolation),  # Replace with your SES variable names
               names_to = "SES_variable", 
               values_to = "SES_value")


# Create a named vector with custom labels for each SES variable
ses_labels <- c(
  NCitizen = "% Non-citizen",
  LinIsolation = "% not speaking English well"
)

# Create the boxplot with custom facet labels
P2=ggplot(long_data, aes(x = SusQuantile, y = SES_value)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outlier dots
  facet_grid(SES_variable ~ .,     # Facet by SES variable on the vertical axis (rows)
             scales = "free_y",    # Allow each panel to have its own y-axis scale
             labeller = labeller(SES_variable = ses_labels)) +  # Apply custom labels
  labs(title = "",
       x = "Level of sucessful claim rate",
       y = "Log scale") +
  scale_y_log10() +  # Log transformation applied to the y-axis
  theme_minimal() +
  theme(legend.position = "none",strip.text = element_text(size = 12))  # Remove legend if not needed
P2


P2=ggplot(long_data, aes(x = SusQuantile, y = SES_value, fill = SusQuantile)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outlier dots
  facet_grid(SES_variable ~ .,     # Facet by SES variable on the vertical axis (rows)
             scales = "free_y",    # Allow each panel to have its own y-axis scale
             labeller = labeller(SES_variable = ses_labels)) +  # Apply custom labels
  labs(title = "",
       x = "Level of sucessful claim rate",
       y = "Log scale") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend if not needed
P2





long_data <- SES %>%
  pivot_longer(cols = c(EduLessHigh, EduLessCollege),  # Replace with your SES variable names
               names_to = "SES_variable", 
               values_to = "SES_value")


# Create a named vector with custom labels for each SES variable
ses_labels <- c(
  EduLessHigh = "Less than high school degree",
  EduLessCollege = "Less than college degree"
)

# Create the boxplot with custom facet labels
P3=ggplot(long_data, aes(x = SusQuantile, y = SES_value)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outlier dots
  facet_grid(SES_variable ~ .,     # Facet by SES variable on the vertical axis (rows)
             scales = "free_y",    # Allow each panel to have its own y-axis scale
             labeller = labeller(SES_variable = ses_labels)) +  # Apply custom labels
  labs(title = "",
       x = "Level of sucessful claim rate",
       y = "Log scale") +
  scale_y_log10() +  # Log transformation applied to the y-axis
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend if not needed
P3




long_data <- voter %>%
  pivot_longer(cols = c(zcta_ces_percent_4),  # Replace with your SES variable names
               names_to = "SES_variable", 
               values_to = "SES_value")
log10(long_data$SES_value)

# Create a named vector with custom labels for each SES variable
ses_labels <- c(
  zcta_ces_percent_4 = "% voters registered"
)

# Create the boxplot with custom facet labels
P4=ggplot(long_data, aes(x = SusQuantile, y = SES_value)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outlier dots
  facet_grid(SES_variable ~ .,     # Facet by SES variable on the vertical axis (rows)
             scales = "free_y",    # Allow each panel to have its own y-axis scale
             labeller = labeller(SES_variable = ses_labels)) +  # Apply custom labels
  labs(title = "",
       x = "Level of sucessful claim rate",
       y = "") +
  #scale_y_log10() +  # Log transformation applied to the y-axis
  theme_minimal() +
  theme(legend.position = "none")+  # Remove legend if not needed
theme(
  strip.text = element_text(size = 12)  # Increase the facet label size to 14
)
P4


