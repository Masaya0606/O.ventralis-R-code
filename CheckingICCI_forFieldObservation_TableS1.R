# Load package
library(readxl)

# CSR
CSR_dates<-read_excel("~/path/CSR_dates.xlsx", sheet = "Sheet1")

# ERFM
ERFM_dates<-read_excel("~/path/ERFM_dates.xlsx", sheet = "Sheet1")

# Build model
CSR_dates$fID <- factor(CSR_dates$ID)
# Build model with data excluding NAs
CSR_clean <- na.omit(CSR_dates)


## Analysis using cbind
library(lme4)
# success = number of sperm received, failure = lekking count - number of sperm received
glmer_model <- glmer(cbind(success, failure) ~ 1 + (1|fID), family = binomial, data = CSR_clean)
glmer_model_2 <- glmer(cbind(success, failure) ~ 1 + (1|fID/Day), family = binomial, data = CSR_clean)

anova(glmer_model, glmer_model_2)

library(performance)
icc(glmer_model)


## ERFM

# Build model
ERFM_dates$fID <- factor(ERFM_dates$ID)
# Build model with data excluding NAs
ERFM_clean <- na.omit(ERFM_dates)

model <- lme(ERFM ~ 1, random = ~1 | fID, data = ERFM_clean, method = "REML")


# Test considering Day and ID
model_nest <- lme(ERFM ~ 1, random = ~1 | fID/Day, data = ERFM_clean, method = "REML")
model_ID_Day_independt <- lme(ERFM ~ 1, random = list(fID = ~1, Day = ~1), data = ERFM_clean, method = "REML")
anova(model_nest, model_ID_Day_independt)

# Random effect with only individual ID (fID)
mod1 <- lme(ERFM ~ 1, random = ~1 | fID, data = ERFM_clean, method = "REML")

# Model with Day nested within individual ID (fID/Day)
mod2 <- lme(ERFM ~ 1, random = ~1 | fID/Day, data = ERFM_clean, method = "REML")

# Alternatively, treat individual and day as independent (crossed effects)
mod3 <- lme(ERFM ~ 1, random = list(fID = ~1, Day = ~1), data = ERFM_clean, method = "REML")

anova(mod1, mod2)  # Comparison possible with REML (OK if only random effects differ)
anova(mod1, mod3)


# Display variance components
VarCorr(model)
VarCorr(model_nest)
VarCorr(model_ID_Day_independt)

# Extract variance as numeric values
vc <- VarCorr(model)
between <- as.numeric(vc["(Intercept)", "Variance"])
within  <- as.numeric(vc["Residual", "Variance"])

# Calculate intraclass correlation coefficient (ICC)
ICC <- between / (between + within)
ICC


# Extract variance as numeric values
vc <- VarCorr(model_nest)
between <- as.numeric(vc["(Intercept)", "Variance"])
within  <- as.numeric(vc["Residual", "Variance"])

# Calculate intraclass correlation coefficient (ICC)
ICC <- between / (between + within)
ICC

# Extract variance as numeric values
vc <- VarCorr(model_ID_Day_independt)
between <- as.numeric(vc["(Intercept)", "Variance"])
within  <- as.numeric(vc["Residual", "Variance"])

# Calculate intraclass correlation coefficient (ICC)
ICC <- between / (between + within)
ICC
