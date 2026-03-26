# load packages
library(dplyr)
library(readr)
library(ggplot2)
library(ipumsr)
library(dagitty)
library(car)
library(tseries)
library(ggfortify)
library(ggeffects)

# load data
ddi <- read_ipums_ddi("final/usa_00003.xml")
data <- read_ipums_micro(ddi)

# filter to just bachelor's degree or higher (otherwise won't have DEGFIELD)
data <- data %>%
  filter(EDUC %in% c(10, 11))

# filer to just years 2020-2023 (year 2000 has missing vals) 
data <- data[data$YEAR %in% c(2020, 2021, 2022, 2023), ]

# check vars
vars_to_check <- c("SEX", "EDUC", "DEGFIELD", "INCTOT", "POVERTY", "RACE", "HISPAN", "YEAR", "REGION", "BPL", "OCC", "INCWAGE", "COUNTYICP")
for (var in vars_to_check) {
  print(var)
  print(ipums_val_labels(ddi, var = var))
}

# missing values
data <- data %>%
  mutate(
    SEX      = ifelse(SEX == 9, NA, SEX),
    EDUC     = ifelse(EDUC == 99, NA, EDUC),
    DEGFIELD = ifelse(DEGFIELD == 0, NA, DEGFIELD),
    INCTOT   = ifelse(INCTOT %in% c(9999998, 9999999), NA, INCTOT),
    INCWAGE  = ifelse(INCWAGE %in% c(999998, 999999), NA, INCWAGE),
    POVERTY  = ifelse(POVERTY == 0, NA, POVERTY),
    HISPAN   = ifelse(HISPAN == 9, NA, HISPAN),
    REGION   = ifelse(REGION %in% c(97, 99), NA, REGION),
    METRO    = ifelse(METRO %in% c(0, 4), NA, METRO),
    OCC      = ifelse(OCC == 000, NA, OCC)
  )

# check missing vals
colSums(is.na(data[vars_to_check]))

# dont use OCC because too many missing values
# 15 percent of poverty variable missing and data evenly distributed so keep it
mean(is.na(data$POVERTY))
data %>%
  mutate(pov_missing = is.na(POVERTY)) %>%
  count(pov_missing, SEX)

data %>%
  mutate(pov_missing = is.na(POVERTY)) %>%
  count(pov_missing, DEGFIELD)


# create percent_female_variable and add back to data
gender_field_summary <- data %>%
  group_by(DEGFIELD) %>%
  summarise(
    n = n(),
    pct_female = mean(SEX == 2, na.rm = TRUE) * 100
  )

data <- data %>%
  left_join(gender_field_summary, by = "DEGFIELD")

# linear model with confounders
model_confounders <- lm(log_income ~ pct_female + POVERTY + SEX + COUNTYICP + BPL + RACE + HISPAN, data = data)
summary(model_confounders)


#  ---- test for non linearity #1 (income) ----
model_confounders_poly1 <- lm(log_income ~ pct_female + I(POVERTY^2) + I(POVERTY^3) + POVERTY +
                               SEX + COUNTYICP + BPL + RACE + HISPAN,
                             data = data)
poverty.interpret <- ggpredict(model_confounders_poly1, terms = "POVERTY")
plot(poverty.interpret) #check if plot is linear
model_nonlin1 <- lm(INCWAGE ~ pct_female + POVERTY + SEX + COUNTYICP + BPL + RACE + HISPAN + I(POVERTY^2) + I(POVERTY^3), data = data)
summary(model_nonlin1)

# check on log transformed income variable to see if that is the issue
data$log_income <- ifelse(data$INCWAGE > 0, log(data$INCWAGE), 0)
model_log <- lm(incwage_log ~ I(POVERTY^2) + I(POVERTY^3) + POVERTY + SEX + COUNTYICP + BPL + RACE + HISPAN +pct_female , data = data)
poverty_log.interpret <- ggpredict(model_log, terms = "POVERTY")
plot(poverty_log.interpret)

#  ---- test for non linearity #1 (pct_female) ----
model_confounders_poly2 <- lm(log_income ~ pct_female + I(pct_female^2) + I(pct_female^3) + POVERTY +
                               SEX + COUNTYICP + BPL + RACE + HISPAN,
                             data = data)
pct_female.intercept <- ggpredict(model_confounders_poly2, terms = "pct_female")
plot(pct_female.intercept) #check if plot is linear
model_nonlin2 <- lm(log_income ~ pct_female + POVERTY + SEX + COUNTYICP + BPL + RACE + HISPAN + I(pct_female^2) + I(pct_female^3), data = data)
summary(model_nonlin2)

#  ---- test for interaction term #1 (poverty & pct_female) ----
# function to interpret interaction coefficient
meplot <- function(model,var1,var2,int,vcov,ci=.95,
                   xlab=var2,ylab=paste("Marginal Effect of",var1),
                   main="Marginal Effect Plot",
                   me_lty=1,me_lwd=1,me_col="black",
                   ci_lty=1,ci_lwd=.5,ci_col="black",
                   yint_lty=2,yint_lwd=1,yint_col="black"){
  require(ggplot2)
  alpha <- 1-ci
  z <- qnorm(1-alpha/2)
  beta.hat <- coef(model)
  cov <- vcov
  z0 <- seq(min(model.frame(model)[,var2],na.rm=T),max(model.frame(model)[,var2],na.rm=T),length.out=1000)
  dy.dx <- beta.hat[var1] + beta.hat[int]*z0
  se.dy.dx <- sqrt(cov[var1,var1] + z0^2*cov[nrow(cov),ncol(cov)] + 2*z0*cov[var1,ncol(cov)])
  upr <- dy.dx + z*se.dy.dx
  lwr <- dy.dx - z*se.dy.dx
  ggplot(data=NULL,aes(x=z0, y=dy.dx)) +
    labs(x=xlab,y=ylab,title=main) +
    geom_line(aes(z0, dy.dx),size = me_lwd, 
              linetype = me_lty, 
              color = me_col) +
    geom_line(aes(z0, lwr), size = ci_lwd, 
              linetype = ci_lty, 
              color = ci_col) +
    geom_line(aes(z0, upr), size = ci_lwd, 
              linetype = ci_lty, 
              color = ci_col) +
    geom_hline(yintercept=0,linetype=yint_lty,
               size=yint_lwd,
               color=yint_col)
}

model_interaction1 <- lm(log_income ~ POVERTY + pct_female + POVERTY:pct_female +
                          SEX + COUNTYICP + BPL + RACE + HISPAN, data = data)
meplot(model_interaction1, var1="POVERTY", 
       var2="pct_female", int="POVERTY:pct_female", 
       vcov=vcov(model_interaction1)) #check if slope

summary(model_interaction1)

#  ---- test for interaction term #2 (sex & race) ----
model_interaction2 <- lm(INCWAGE ~ SEX + RACE + SEX:RACE +
                       pct_female + POVERTY + COUNTYICP + BPL + HISPAN,
                     data = data)
meplot(model_interaction2, var1="SEX", var2="RACE", 
       int="SEX:RACE", vcov=vcov(model_interaction2)) #check if slope

summary(model_interaction2)

#  ---- re run regression model ----
model_new <- lm(log_income ~ pct_female + POVERTY + SEX + COUNTYICP + BPL + RACE + HISPAN + POVERTY:pct_female + I(pct_female^2) + I(pct_female^3) + I(POVERTY^2) + I(POVERTY^3), data = data)
summary(model_new)

# publishable table:
publishable_table <- modelsummary(
  list("Advanced Multivariate Regression Model" = model_new),
  output = "regression_table_etc.docx",  # You can change this to "markdown" or "html" for testing
  title = "Regression of log(Income) on Gender Composition and Controls",
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
  coef_rename = c(
    "(Intercept)" = "Intercept",
    "pct_female" = "Percent Female in Field",
    "POVERTY" = "Poverty Status",
    "SEX" = "Sex",
    "COUNTYICP" = "County",
    "BPL" = "Place of Birth",
    "RACE" = "Race",
    "HISPAN" = "Hispanic Origin",
    "pct_female2" = "Percent Female²",
    "pct_female3" = "Percent Female³",
    "POVERTY2" = "Poverty²",
    "POVERTY3" = "Poverty³",
    "pct_female:POVERTY" = "Female % × Poverty Status"
  ),
  notes = "Standard errors in parentheses."
)


# summarize distributions
summary(data$INCWAGE)

ggplot(data, aes(x = INCWAGE)) +
  geom_histogram(binwidth = 5000, fill = "lightgreen", color = "black") +
  labs(
    x = "Total Income",
    y = "Count",
    title = "Distribution of Total Income"
  )

# BOX COX TEST FOR TRANSFORMATION:
# Remove zeros (Box-Cox requires positive values)
incwage_pos <- data$INCWAGE[data$INCWAGE > 0]

# Fit a linear model (response ~ 1 since we only care about distribution shape)
library(MASS)
boxcox_result <- boxcox(incwage_pos ~ 1, lambda = seq(-2, 2, 0.1))

# Find optimal lambda
lambda_opt <- boxcox_result$x[which.max(boxcox_result$y)]
lambda_opt
# lambda close in between square root and log - chose log

# collapse to field level averages for better visual:
field_summary <- data %>%
  group_by(DEGFIELD) %>%
  summarise(
    avg_income = mean(INCWAGE, na.rm = TRUE),
    pct_female = mean(SEX == 2, na.rm = TRUE) * 100,
    n = n()
  ) %>%
  filter(!is.na(avg_income), !is.na(pct_female))

ggplot(field_summary, aes(x = pct_female, y = avg_income)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    x = "Percent Female in Field",
    y = "Average Income",
    title = "Field-Level Relationship: Gender Makeup and Income"
  ) +
  theme_minimal()
