
##############################
##  robustness.R: Code for robustness checks
##  Note: See models.R for for the models
##        in the paper.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###


######################################
#  First model, Poisson specification
######################################

model.1.count <- glm(formula = revision ~ Conditionb + base_accuracy + 
                       recognize.n + poli_interest.n, 
                     data = df[df$Conditionb != "Affirmed",],
                     poisson())

model.1.count %>% summary()

pred.1.count <- ggpredict(model.1.count, terms = "Conditionc")


######################################
#  Second model, Poisson specification
######################################

model.2.count <- glm(formula = revision ~ Condition + base_accuracy + 
                       recognize.n + poli_interest.n, 
                     data = df[df$Conditionb != "Affirmed",],
                     poisson())

model.2.count %>% summary()

pred.2.count <- ggpredict(model.2.count, terms = "Condition")


######################################
#  Third model, Poisson specification
######################################

model.3.count <- glm(formula = revision ~ Conditionb + base_accuracy +
                       recognize.n + poli_interest.n, 
                     data = df[df$Conditionb != "Normative",],
                     poisson())

model.3.count %>% summary()

pred.3.count <- ggpredict(model.3.count, terms = "Conditionb")


######################################
#  Poisson adjusted prediction plots
######################################

plot.1.count <- pred.1.count %>%
  ggplot(aes(x = x, y = predicted)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0, size = 1) +
  scale_x_discrete(labels = c("Control", "Definitive", "Normative")) +
  labs(x = "", y = "Predicted Number of Reclassifications") +
  ylim(1.4, 2.6)

plot.2.count <- pred.2.count %>%
  ggplot(aes(x = x, y = predicted)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0, size = 1) +
  scale_x_discrete(labels = c("Control", "Def,\nFalse", "Def,\nTrue",
                              "Norm,\nFalse", "Norm,\nTrue")) +
  labs(x = "", y = "") +
  ylim(1.4, 2.6)

plot.3.count <- pred.3.count %>%
  ggplot(aes(x = fct_relevel(x, "Control", "Definitive", "Affirmed"), 
             y = predicted)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0, size = 1) +
  scale_x_discrete(labels = c("Control", "Definitive", "Affirmed")) +
  labs(x = "", y = "") +
  ylim(1.4, 2.6)


######################################
#  First model, OLS specification, 6 collapsed
######################################

df$revision2 <- df$revision
df$revision2[df$revision == 6] <- 5
#table(df$revision, df$revision2)

model.1.ols.5 <- lm(formula = revision2 ~ Conditionb + base_accuracy +
                      recognize.n + poli_interest.n, 
                    data = df[df$Conditionb != "Affirmed",])

model.1.ols.5 %>% summary()


######################################
#  First model, Poisson specification, 6 collapsed
######################################

model.1.count.5 <- glm(formula = revision2 ~ Conditionb + base_accuracy +
                         recognize.n + poli_interest.n, 
                       data = df[df$Conditionb != "Affirmed",],
                       poisson())

model.1.count.5 %>% summary()


######################################
#  Second model, OLS specification, 6 collapsed
######################################

model.2.ols.5 <- lm(formula = revision2 ~ Condition + base_accuracy +
                      recognize.n + poli_interest.n, 
                    data = df[df$Conditionb != "Affirmed",])

model.2.ols.5 %>% summary()


######################################
#  Second model, Poisson specification, 6 collapsed
######################################

model.2.count.5 <- glm(formula = revision2 ~ Condition + base_accuracy +
                         recognize.n + poli_interest.n, 
                       data = df[df$Conditionb != "Affirmed",],
                       poisson())

model.2.count.5 %>% summary()


######################################
#  Third model, OLS specification, 6 collapsed
######################################

model.3.ols.5 <- lm(formula = revision2 ~ Conditionb + base_accuracy +
                      recognize.n + poli_interest.n, 
                    data = df[df$Conditionb != "Normative",])

model.3.ols.5 %>% summary()


######################################
#  Third model, Poisson specification, 6 collapsed
######################################

model.3.count.5 <- glm(formula = revision2 ~ Conditionb + base_accuracy +
                         recognize.n + poli_interest.n, 
                       data = df[df$Conditionb != "Normative",],
                       poisson())

model.3.count.5 %>% summary()


### END ###
