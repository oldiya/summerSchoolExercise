################################################################################
# Title: Code to do exercise question 1
# 
################################################################################

# Load the data ----

    observations <- read.csv("data/observations.csv")
    
# Explore the data ----

    str(observations)
    
# Fitting a Poisson GLM ----
  # Model 1 

    bryoModel1 <- glm(bryophitaNumObs ~ GiniDBH,
                      family = poisson,
                      data = observations)
    
    summary(bryoModel1)
    
  # Model 2
    bryoModel2 <- glm(bryophitaNumObs ~ GiniDBH + ShannonIndexTreeSpp,
                      family = poisson,
                      data = observations)
    
    summary(bryoModel2)
    
# Deviance explained by the model1
    
    # Extract the null and residual deviance from the model
    dev.null <- bryoModel1$null.deviance
    dev.resid <- bryoModel1$deviance
    
    # Calculate the deviance explained by the model
    dev.explained <- (dev.null - dev.resid)/dev.null
    
    # Round to 3 decimal places
    dev.explained <- round(dev.explained, 3)
    
    dev.explained
    
    # Simulate residuals 
    simResids <- DHARMa::simulateResiduals(bryoModel1)
    
    # Generate plots to compare the model residuals to expectations
    plot(simResids)
    