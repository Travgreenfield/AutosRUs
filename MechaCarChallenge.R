# Dependencies
library(jsonlite)
library(tidyverse)

# Read in dataset
mechaCar <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

# Update column names
mechaCar <- mechaCar %>% rename(vehicle_length = `vehicle length`, vehicle_weight = `vehicle weight`, spoiler_angle = `spoiler angle`, ground_clearance = `ground clearance`)

# Linear regression to predict mpg
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance, mechaCar))

# Suspension coil
coilSuspension <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# Generating summary data
summary(coilSuspension$PSI)
coil_mean <- mean(coilSuspension$PSI)
coil_variance <- var(coilSuspension$PSI)
coil_STD <- sd(coilSuspension$PSI)
coil_median <- median(coilSuspension$PSI)

# Formatting into table
summary_coil_table <- data.frame(coil_mean, coil_median, coil_variance, coil_STD)

#Suspension Coil T-Test
shapiro.test(coilSuspension$PSI)

sample_data <- coilSuspension %>% sample_n(45)

t.test(sample_data$PSI, mu=1500)