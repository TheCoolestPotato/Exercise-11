library(tidyverse)
library(usethis)
library(lmodel2)
library(skimr)
library(latticeExtra)
library(broom)
library(sjPlot)
library(mosaic)
library(manipulate)
library(patchwork)
library(infer)
library(naniar)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/Mammal_lifehistories_v2.txt"
d <- read_tsv(f)
skim(d)
##Order, family, genus, and species are categorical variables while mass, gestation time, newborn weight, weaning time, wean mass, AFR, max lifespan, litter size, litters per year, and refs are all numerical variables.
d <- replace_with_na_all(d,condition = ~.x == -999)
d$`litter size` <- NULL
d$refs <- NULL
d$log_mass <- log(d$`mass(g)`)
d$log_gestation <- log(d$`gestation(mo)`)
d$log_newborn <- log(d$`newborn(g)`)
d$log_weaning <- log(d$`weaning(mo)`)
d$log_weanmass <- log(d$`wean mass(g)`)
d$log_AFR <- log(d$`AFR(mo)`)
d$log_maxlife <- log(d$`max. life(mo)`)
d$log_littersperyr <- log(d$`litters/year`)
d$relgest <- resid(lm(`log_gestation` ~ `log_mass`, data = d, na.action=na.exclude))
d$relwean <- resid(lm(`log_weaning` ~ `log_mass`, data = d,na.action=na.exclude))
d$relAFR <- resid(lm(`log_AFR` ~ `log_mass`, data = d, na.action=na.exclude))
d$rellife <- resid(lm(`log_maxlife` ~ `log_mass`, data = d, na.action=na.exclude))
d$relnewbornmass <- resid(lm(`log_newborn` ~ `log_mass`, data = d, na.action=na.exclude))
d$relweanmass <- resid(lm(`log_weanmass` ~ `log_mass`, data = d, na.action=na.exclude))
rellife_plot <- ggplot(data = d, aes(x = order, y = rellife)) +
  geom_boxplot(na.rm = TRUE) +
  geom_jitter(na.rm = TRUE, alpha = 0.1)
plot(rellife_plot)
##Primates have the highest residual lifespan
relnewborn_plot <- ggplot(data = d, aes(x = order, y = relnewbornmass)) +
  geom_boxplot(na.rm = TRUE) +
  geom_jitter(na.rm = TRUE, alpha = 0.1)
plot(relnewborn_plot)
##Cetacea and Macroscelidae have the highest average residual body mass at birth
relweanmass_plot <- ggplot(data = d, aes(x = order, y = relweanmass)) +
  geom_boxplot(na.rm = TRUE) +
  geom_jitter(na.rm = TRUE, alpha = 0.1)
plot(relweanmass_plot)
##Perissodactyla have the highest residual body mass at weaning
d1 <- drop_na(d)
lm_life <- lm(data = d1, log_maxlife ~ log_gestation + log_newborn + log_weaning + log_weanmass + log_littersperyr + log_mass)
summary(lm_life)
lm_AFR <- lm(data = d1, log_AFR ~ log_gestation + log_newborn + log_weaning + log_weanmass + log_littersperyr + log_mass)
summary(lm_AFR)
drop1(lm_life, test = "F")
lm_life2 <- update(lm_life, .~. - `log_weanmass`)
drop1(lm_life2, test = "F")
b3 <- update(lm_life2, .~. - `log_newborn`)
drop1(b3, test = "F")
##All predictors now successful