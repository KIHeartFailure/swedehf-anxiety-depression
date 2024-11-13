source(here::here("setup/setup.R"))

# load data
load(here("data/clean-data/data.RData"))

dataass <- mice::complete(imprsdata, 3)
dataass <- mice::complete(imprsdata, 6)

# check assumptions for cox models ----------------------------------------

i <- 1
mod <- coxph(formula(paste0(
  "Surv(", outvars$time[i], ",", outvars$var[i], " == 'Yes') ~ shf_anxiety + ", paste(modvars, collapse = " + ")
)), data = dataass)

# prop hazard assumption
testpat <- cox.zph(mod)
print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

x11()
plot(testpat[1], resid = T, col = "red")
plot(testpat[5], resid = T, col = "red")

# outliers
ggcoxdiagnostics(mod,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)


car::vif(mod)
rms::vif(mod)

i <- 2
mod <- coxph(formula(paste0(
  "Surv(", outvars$time[i], ",", outvars$var[i], " == 'Yes') ~ shf_anxiety + ", paste(modvars, collapse = " + ")
)), data = dataass)

# prop hazard assumption
testpat <- cox.zph(mod)
print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

x11()
plot(testpat[1], resid = T, col = "red")
plot(testpat[5], resid = T, col = "red")

# outliers
ggcoxdiagnostics(mod,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)


i <- 3
mod <- coxph(formula(paste0(
  "Surv(", outvars$time[i], ",", outvars$var[i], " == 'Yes') ~ shf_anxiety + ", paste(modvars, collapse = " + ")
)), data = dataass)

# prop hazard assumption
testpat <- cox.zph(mod)
print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

x11()
plot(testpat[1], resid = T, col = "red")
plot(testpat[5], resid = T, col = "red")

# outliers
ggcoxdiagnostics(mod,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)


i <- 4
mod <- coxph(formula(paste0(
  "Surv(", outvars$time[i], ",", outvars$var[i], " == 'Yes') ~ shf_anxiety + ", paste(modvars, collapse = " + ")
)), data = dataass)

# prop hazard assumption
testpat <- cox.zph(mod)
print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

x11()
plot(testpat[1], resid = T, col = "red")
plot(testpat[5], resid = T, col = "red")

# outliers
ggcoxdiagnostics(mod,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)


# For log reg models
ormod <- glm(formula(paste0("shf_anxiety =='Moderate' ~ ", paste(modvars, collapse = " + "))),
  family = binomial(link = "logit"), data = dataass %>% filter(shf_anxiety != "Severe")
)

# vif
print(car::vif(ormod))

# outliers
cooks <- cooks.distance(ormod)
plot(cooks)
abline(h = 4 / nrow(dataass), lty = 2, col = "red") # add cutoff line

ormod <- glm(formula(paste0("shf_anxiety =='Severe' ~ ", paste(modvars, collapse = " + "))),
  family = binomial(link = "logit"), data = dataass %>% filter(shf_anxiety != "Moderate")
)

# vif
print(car::vif(ormod))

# outliers
cooks <- cooks.distance(ormod)
plot(cooks)
abline(h = 4 / nrow(dataass), lty = 2, col = "red") # add cutoff line
