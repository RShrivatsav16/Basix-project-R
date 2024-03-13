library(tidyverse)
library(e1071)
library(extraDistr)
library(MASS)
library(MultiRNG)
library(matrixcalc)
library(Matrix)
library(dplyr)


# Setting Seed for reproducibility 

set.seed(616)

# Functions for lognormal which takes mean and sd

qlnorm2<-function(p,mean,sd)
  qlnorm(p=p,
         meanlog=log(mean*(1+sd^2/mean^2)^-.5),
         sdlog=log(1+sd^2/mean^2)^.5)

rlnorm2<-function(n,mean,sd)
  qlnorm2(runif(n),mean,sd)


# Let's make a risk profile for the 2003 policy 

# POLICY INFORMATION 

# No of trials

n <- 1000 

# Targeted rainfall for 2003

#target_rainfall <- 653  

# How much premium would be paid by the size of the farmers 

premiums <- c(900, 650, 450)


# payments we would make on the basis of the range it falls into such as 95-75%, 75-35% and below 35%

payment_large_farmer <-  c(30, 175, 650)
payment_medium_farmer <- c(25, 100, 500)
payment_small_farmer <-  c(20,  75, 310)


# Weight of the rainfall index

weights_rainfall <- c(.25, 3, .75)


# PARAMETERS FOR LOGNORMAL

phase_means <- c(115.0, 191.1, 209.7)
phase_sd <- c(56.0, 82.5, 97.7)



# SIMULATE RAINFALL FOR EACH PHASE

# Creating a data frame with each phase and then calculating the weighted index and payouts 

simulate_rainfall <- data_frame(
  phase1 = rlnorm2(n, phase_means[1], phase_sd[1]),
  phase2 = rlnorm2(n, phase_means[2], phase_sd[2]),
  phase3 = rlnorm2(n, phase_means[3], phase_sd[3])
)


# Function that takes the risk profile...

Risk_profile <- function(target_rainfall){
  
  simulate_rainfall <- simulate_rainfall%>%
    mutate(Weighted_index = phase1*weights_rainfall[1] + phase2*weights_rainfall[2] + phase3*weights_rainfall[3],
           Perc_target = Weighted_index/target_rainfall)
  
  
  # Computing payouts
  
  Compute_payout <- function(Premium, Perc_target, payments){
    
    payout <- case_when(
      Perc_target >= 0.95 ~ 0,
      Perc_target >= 0.75 ~ payments[1] * ((0.95 - Perc_target)*100),
      Perc_target >= 0.35 ~ payments[1] * ((0.95 - 0.75)*100) + payments[2] * ((0.75 - Perc_target)*100),
      TRUE ~ payments[1] * ((0.95 - 0.75)*100) + payments[2] * ((0.75 - 0.35)*100) + payments[3] * ((0.35 - Perc_target)*100)
    )
    
    payout_net <- payout - Premium 
    return(payout_net)
    
  }
    
  # Calculate the payouts for each farmer with Compute_payouts()
    
  Large_farmer <- Compute_payout(premiums[1], simulate_rainfall$Perc_target, payment_large_farmer)
  
  Medium_farmer <-Compute_payout(premiums[2], simulate_rainfall$Perc_target, payment_medium_farmer)
  
  Small_farmer <- Compute_payout(premiums[3], simulate_rainfall$Perc_target, payment_small_farmer)
  
  
  simulate_rainfall <- simulate_rainfall%>%
    mutate(Larger_farmer = Large_farmer,
           Medium_farmer = Medium_farmer,
           Small_farmer = Small_farmer)

  return(simulate_rainfall)
  
}



# SENSITIVITY ANALYSIS

range_target <- seq(600, 700, by = 5)

larger_farmer_sens <- sapply(range_target, function(x) Risk_profile(x)$Larger_farmer)%>%as.data.frame()

# Giving column names 
colnames(larger_farmer_sens) <- paste0(range_target)

Medium_farmer_sens <- sapply(range_target, function(x) Risk_profile(x)$Medium_farmer)%>%as.data.frame()

# Giving column names 
colnames(Medium_farmer_sens) <- paste0(range_target)

Small_farmer_sens <- sapply(range_target, function(x) Risk_profile(x)$Small_farmer)%>%as.data.frame()

# Giving column names 
colnames(Small_farmer_sens) <- paste0(range_target)


df_large <- data.frame(Rainfall_target = range_target,
                  Mean = colMeans(larger_farmer_sens),
                  CI_80_Lower = apply(larger_farmer_sens, 2, quantile, probs = 0.1),
                  CI_80_Upper = apply(larger_farmer_sens, 2, quantile, probs = 0.9),
                  CI_95_Lower = apply(larger_farmer_sens, 2, quantile, probs = 0.025),
                  CI_95_Upper = apply(larger_farmer_sens, 2, quantile, probs = 0.975)
)


df_medium <- data.frame(Rainfall_target = range_target,
                       Mean = colMeans(Medium_farmer_sens),
                       CI_80_Lower = apply(Medium_farmer_sens, 2, quantile, probs = 0.1),
                       CI_80_Upper = apply(Medium_farmer_sens, 2, quantile, probs = 0.9),
                       CI_95_Lower = apply(Medium_farmer_sens, 2, quantile, probs = 0.025),
                       CI_95_Upper = apply(Medium_farmer_sens, 2, quantile, probs = 0.975)
)


df_small <- data.frame(Rainfall_target = range_target,
                       Mean = colMeans(Small_farmer_sens),
                       CI_80_Lower = apply(Small_farmer_sens, 2, quantile, probs = 0.1),
                       CI_80_Upper = apply(Small_farmer_sens, 2, quantile, probs = 0.9),
                       CI_95_Lower = apply(Small_farmer_sens, 2, quantile, probs = 0.025),
                       CI_95_Upper = apply(Small_farmer_sens, 2, quantile, probs = 0.975)
)



# CREATING A RIBBON PLOT

# For Large Farm 

ggplot(df_large, aes(x = Rainfall_target))+
  geom_ribbon(aes(ymin = CI_95_Lower, ymax = CI_95_Upper), alpha = 0.3, fill = 'blue')+
  geom_ribbon(aes(ymin = CI_80_Lower, ymax = CI_80_Upper), alpha = 0.3, fill = 'blue') +
  geom_line(aes(y = Mean)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'bold'))+
  ggtitle('Large Farmer')
  


# For Medium Farm 

ggplot(df_medium, aes(x = Rainfall_target))+
  geom_ribbon(aes(ymin = CI_95_Lower, ymax = CI_95_Upper), alpha = 0.3, fill = 'blue')+
  geom_ribbon(aes(ymin = CI_80_Lower, ymax = CI_80_Upper), alpha = 0.3, fill = 'blue') +
  geom_line(aes(y = Mean)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'bold'))+
  ggtitle('Medium Farmer')


# For Small Farm

ggplot(df_small, aes(x = Rainfall_target))+
  geom_ribbon(aes(ymin = CI_95_Lower, ymax = CI_95_Upper), alpha = 0.3, fill = 'blue')+
  geom_ribbon(aes(ymin = CI_80_Lower, ymax = CI_80_Upper), alpha = 0.3, fill = 'blue') +
  geom_line(aes(y = Mean)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'bold'))+
  ggtitle('Small Farmer')


# CREATING A DENSITY PLOT

# Setting the data up for density plot

dens_large <- larger_farmer_sens%>%
  pivot_longer(cols = everything(), names_to = 'Rainfall_index', values_to = 'Net_payout')%>%
  filter(Rainfall_index %in% seq(600,700, by = 6))


dens_medium <- Medium_farmer_sens%>%
  pivot_longer(cols = everything(), names_to = 'Rainfall_index', values_to = 'Net_payout')%>%
  filter(Rainfall_index %in% seq(600,700, by = 6))

  

dens_small <- Small_farmer_sens%>%
  pivot_longer(cols = everything(), names_to = 'Rainfall_index', values_to = 'Net_payout')%>%
  filter(Rainfall_index %in% seq(600,700, by = 6))


# Plotting the density plot

ggplot(dens_large,aes(x = Net_payout, fill = Rainfall_index)) +
  geom_density(aes(group = Rainfall_index), alpha = 0.5) +
  ggtitle('Density comparison for large farmer')+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 15)) + 
  coord_cartesian(xlim = c(-900, 0))
  

ggplot(dens_medium,aes(x = Net_payout, fill = Rainfall_index)) +
  geom_density(aes(group = Rainfall_index), alpha = 0.5) +
  ggtitle('Density comparison for medium farmer')+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 15)) + 
  coord_cartesian(xlim = c(-650, 0))


ggplot(dens_small,aes(x = Net_payout, fill = Rainfall_index)) +
  geom_density(aes(group = Rainfall_index), alpha = 0.5) +
  ggtitle('Density comparison for small farmer')+
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 15)) + 
  coord_cartesian(xlim = c(-450, 0))



##################################################

## 2004 Policy ##
# Set the parameters of the policy

premium_2004 <- 125

normal_index <- c(75, 110, 75)
payments <- c(15, 10, 5)
trigger_max <- c(20, 40, 10)
max_payout <- c(3000, 2000, 1000)

# Set the parameters for rainfall simulation

trial <- 1000

phase_means <- c(115.0, 191.1, 209.7)
phase_sd <- c(56, 82.5, 97.7)

# Simulate rainfall for each phase

set.seed(616)

simulate_rainfall_2004 <- data.frame(
  phase1 = rlnorm2(trial, phase_means[1], phase_sd[1]),
  phase2 = rlnorm2(trial, phase_means[2], phase_sd[2]),
  phase3 = rlnorm2(trial, phase_means[3], phase_sd[3])
)

# Define the payout function for the 2004 policy

Compute_payout_2004 <- function(Premium, rainfall, normal_index, payments, trigger_max, max_payout) {
  
  # Calculate shortfall payouts
  
  shortfall <- pmax(normal_index - rainfall, 0)
  shortfall_payouts <- payments * shortfall
  
  # Calculate max trigger payouts
  
  max_trigger_payouts <- (rainfall <= trigger_max) * max_payout
  
  # Combine the payouts
  
  phase_payouts <- pmax(shortfall_payouts, max_trigger_payouts)
  
  # Calculate total and net payouts
  
  total_payout <- sum(phase_payouts)
  payout_net <- total_payout - Premium
  
  return(payout_net)
}

# Calculate the net payouts for each simulated rainfall scenario

net_payouts <- apply(simulate_rainfall_2004, 1, function(rainfall) {
  Compute_payout_2004(premium_2004, rainfall, normal_index, payments, trigger_max, max_payout)})%>%
  as.data.frame()


colnames(net_payouts) <- 'Net_payout'

# SENSITIVITY ANALYSIS

perform_sensitivity_analysis <- function(new_premium) {
  # Calculate the net payouts for each simulated rainfall scenario with the new premium
  net_payouts <- apply(simulate_rainfall_2004, 1, function(rainfall) {
    Compute_payout_2004(new_premium, rainfall, normal_index, payments, trigger_max, max_payout)
  }) %>% as.data.frame()
  
  colnames(net_payouts) <- 'Net_payout'
  
  # Return the net payouts data frame
  return(net_payouts)
}

# Perform sensitivity analysis for different premium values
premium_values <- c(100, 125, 150)  # Example premium values to analyze

sensitivity_results <- lapply(premium_values, perform_sensitivity_analysis)

# Print the net payouts for each premium value
for (i in seq_along(premium_values)) {
  cat("Premium:", premium_values[i], "\n")
  print(sensitivity_results[[i]])
  cat("\n")
}

combined_results <- do.call(rbind, sensitivity_results)
combined_results$Premium <- rep(premium_values, each = nrow(sensitivity_results[[1]]))

# Plot the net payouts for different premium values
ggplot(combined_results, aes(x = Premium, y = Net_payout, group = 1)) +
  geom_line() +
  geom_point() +
  labs(x = "Premium", y = "Net Payout") +
  ggtitle("Sensitivity Analysis: Net Payouts for Different Premium Values") +
  theme(plot.title = element_text(hjust = 0.5))

# Combine the net payouts from sensitivity_results into a single data frame
combined_data <- do.call(rbind, sensitivity_results)
combined_data$Premium <- rep(premium_values, each = nrow(sensitivity_results[[1]]))

# Calculate the density values
density_values <- with(combined_data, density(Net_payout))

# Create a data frame with the density values
density_df <- data.frame(x = density_values$x, y = density_values$y)

# Create the density plot with ribbon
ggplot() +
  geom_density(data = combined_data, aes(x = Net_payout, fill = factor(Premium)), alpha = 0.5) +
  geom_ribbon(data = density_df, aes(x = x, ymin = 0, ymax = y), alpha = 0.2) +
  labs(x = "Net Payout", y = "Density") +
  ggtitle("Sensitivity Analysis: Net Payouts Density and Ribbons") +
  theme(plot.title = element_text(hjust = 0.5))


### Using Ranges: 

premium_range <- seq(100, 150, by = 10)
range_sens <- lapply(premium_range, perform_sensitivity_analysis)

for (i in seq_along(premium_range)) {
  cat("Premium:", premium_range[i], "\n")
  print(range_sens[[i]])
  cat("\n")
}

combined_results <- do.call(rbind, range_sens)
combined_results$Premium <- rep(premium_range, each = nrow(range_sens[[1]]))

density_values <- with(combined_results, density(Net_payout))
density_df <- data.frame(x = density_values$x, y = density_values$y)

#Density w/ Ribbon
ggplot() +
  geom_density(data = combined_results, aes(x = Net_payout, fill = factor(Premium)), alpha = 0.5) +
  geom_ribbon(data = density_df, aes(x = x, ymin = 0, ymax = y), alpha = 0.2) +
  labs(x = "Net Payout", y = "Density") +
  ggtitle("Sensitivity Analysis: Net Payouts Density and Ribbons") +
  theme(plot.title = element_text(hjust = 0.5))

#Ribbon Plot 

combined_data <- do.call(rbind, range_sens)
combined_data$Premium <- rep(premium_range, each = nrow(range_sens[[1]]))

df_range <- combined_data %>%
  group_by(Premium) %>%
  summarize(
    Mean = mean(Net_payout),
    CI_80_Lower = quantile(Net_payout, probs = 0.1),
    CI_80_Upper = quantile(Net_payout, probs = 0.9),
    CI_95_Lower = quantile(Net_payout, probs = 0.025),
    CI_95_Upper = quantile(Net_payout, probs = 0.975)
  )


ggplot(df_range, aes(x = premium_range)) +
  geom_ribbon(aes(ymin = CI_95_Lower, ymax = CI_95_Upper), alpha = 0.3, fill = 'blue') +
  geom_ribbon(aes(ymin = CI_80_Lower, ymax = CI_80_Upper), alpha = 0.3, fill = 'blue') +
  geom_line(aes(y = Mean)) +
  labs(x = "Premium Range", y = "Net Payout") +
  ggtitle("Sensitivity Analysis: Net Payouts Ribbon Plot") +
  theme(plot.title = element_text(hjust = 0.5))
