
##############################
##  models.R: Code for models in binding paper
##  Note: See prep.R for data and variable
##        specifications and packages.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###


######################################
#  First model, OLS specification
######################################

model.1.ols <- lm(formula = revision ~ 
                Conditionb + base_accuracy + recognize.n + poli_interest.n, 
            data = df[df$Conditionb != "Affirmed",])

model.1.ols %>% summary()
sqrt(mean(model.1.ols$residuals^2)) #RMSE
model.frame(model.1.ols) %>% nrow() #N

pred.1.ols <- ggpredict(model.1.ols, terms = "Conditionb")


######################################
#  Second model, OLS specification
######################################

model.2.ols <- lm(formula = revision ~ Condition + base_accuracy +
                recognize.n + poli_interest.n, 
                data = df[df$Conditionb != "Affirmed",])

model.2.ols %>% summary()
sqrt(mean(model.2.ols$residuals^2)) #RMSE
model.frame(model.2.ols) %>% nrow() #N

pred.2.ols <- ggpredict(model.2.ols, terms = "Condition")


######################################
#  Third model, OLS specification
######################################

model.3.ols <- lm(formula = revision ~ Conditionb + base_accuracy +
                recognize.n + poli_interest.n, 
                data = df[df$Conditionb != "Normative",])

model.3.ols %>% summary()
sqrt(mean(model.3.ols$residuals^2)) #RMSE
model.frame(model.3.ols) %>% nrow() #N


pred.3.ols <- ggpredict(model.3.ols, terms = "Conditionb")


######################################
#  OLS adjusted prediction plots
######################################

plot.1.ols <- pred.1.ols %>%
  ggplot(aes(x = x, y = predicted)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0, size = 1) +
  scale_x_discrete(labels = c("Control", "Definitive", "Normative")) +
  labs(x = "", y = "Predicted Number of Reclassifications") +
  ylim(1.4, 2.6) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 14))

plot.2.ols <- pred.2.ols %>%
  ggplot(aes(x = x, y = predicted)) +
  geom_point(size = 4, color = "#481567FF") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0, size = 1, color = "#481567FF") +
  scale_x_discrete(labels = c("Control", "Def,\nInverted", "Def,\nActual",
                              "Norm,\nInverted", "Norm,\nActual")) +
  labs(x = "", y = "Predicted Number of Reclassifications") +
  ylim(1.4, 2.6) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 14))

plot.3.ols <- pred.3.ols %>%
  ggplot(aes(x = fct_relevel(x, "Control", "Definitive", "Affirmed"), 
             y = predicted)) +
  geom_point(size = 4, color = "#55C667FF") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0, size = 1, color = "#55C667FF") +
  scale_x_discrete(labels = c("Control", "Definitive", "Affirmed")) +
  labs(x = "", y = "Predicted Number of Reclassifications") +
  ylim(1.4, 2.6) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 14))


######################################
#  Marginal plots
######################################

  # Get list of control variable names
control.vars <- c("base_accuracy","recognize.n","poli_interest.n")

  #G et means of control variable names
control.values <- describe(df[df$Conditionb != "Affirmed", 
                              control.vars])$mean
control.values2 <- describe(df[df$Conditionb != "Normative", 
                               control.vars])$mean

  # Get all non-Affirmed Conditionc contrasts with Authoritative condition
contrast.1 <- contrast(model.1.ols,
                       list(Conditionb = c("Control","Normative"),
                            base_accuracy = control.values[1],
                            recognize.n = control.values[2],
                            poli_interest.n = control.values[3]),
                       list(Conditionb = "Definitive",
                            base_accuracy = control.values[1],
                            recognize.n = control.values[2],
                            poli_interest.n = control.values[3])
)

  # Get all Condition contrasts with Authoritative, True condition
contrast.2.t <- contrast(model.2.ols,
                         list(Condition = c("Control","Norm, True",
                                            "Norm, False","Def, False"),
                              base_accuracy = control.values[1],
                              recognize.n = control.values[2],
                              poli_interest.n = control.values[3]),
                         list(Condition = c("Def, True"),
                              base_accuracy = control.values[1],
                              recognize.n = control.values[2],
                              poli_interest.n = control.values[3])
)

  # Get all Condition contrasts with Authoritative, False condition
contrast.2.f <- contrast(model.2.ols,
                         list(Condition = c("Control","Norm, True",
                                            "Norm, False","Def, True"),
                              base_accuracy = control.values[1],
                              recognize.n = control.values[2],
                              poli_interest.n = control.values[3]),
                         list(Condition = c("Def, False"),
                              base_accuracy = control.values[1],
                              recognize.n = control.values[2],
                              poli_interest.n = control.values[3])
)

  # Get all non-Normative Conditionc contrasts with Authoritative ondition
contrast.3 <- contrast(model.3.ols,
                       list(Conditionb = c("Control","Affirmed"),
                            base_accuracy = control.values2[1],
                            recognize.n = control.values2[2],
                            poli_interest.n = control.values2[3]),
                       list(Conditionb = "Definitive",
                            base_accuracy = control.values2[1],
                            recognize.n = control.values2[2],
                            poli_interest.n = control.values2[3])
)

  # Convert contrast class objects to data frames
contrast.1.out <- cbind(unlist(contrast.1[1]),
                        unlist(contrast.1[2]),
                        unlist(contrast.1[3]),
                        unlist(contrast.1[4]),
                        unlist(contrast.1[5]),
                        unlist(contrast.1[6]),
                        unlist(contrast.1[7]),
                        unlist(contrast.1[8])) %>%
  as.data.frame()

colnames(contrast.1.out) <- list.names(contrast.1[1:8])

contrast.2.t.out <- cbind(unlist(contrast.2.t[1]),
                          unlist(contrast.2.t[2]),
                          unlist(contrast.2.t[3]),
                          unlist(contrast.2.t[4]),
                          unlist(contrast.2.t[5]),
                          unlist(contrast.2.t[6]),
                          unlist(contrast.2.t[7]),
                          unlist(contrast.2.t[8])) %>%
  as.data.frame()

colnames(contrast.2.t.out) <- list.names(contrast.2.t[1:8])

contrast.2.f.out <- cbind(unlist(contrast.2.f[1]),
                          unlist(contrast.2.f[2]),
                          unlist(contrast.2.f[3]),
                          unlist(contrast.2.f[4]),
                          unlist(contrast.2.f[5]),
                          unlist(contrast.2.f[6]),
                          unlist(contrast.2.f[7]),
                          unlist(contrast.2.f[8])) %>%
  as.data.frame()

colnames(contrast.2.f.out) <- list.names(contrast.2.f[1:8])

contrast.3.out <- cbind(unlist(contrast.3[1]),
                        unlist(contrast.3[2]),
                        unlist(contrast.3[3]),
                        unlist(contrast.3[4]),
                        unlist(contrast.3[5]),
                        unlist(contrast.3[6]),
                        unlist(contrast.3[7]),
                        unlist(contrast.3[8])) %>%
  as.data.frame()

colnames(contrast.3.out) <- list.names(contrast.3[1:8])

  # Convert non-label columns to numeric
contrast.1.out[-1] <- apply(contrast.1.out[-1], 2, function(x) as.numeric(x))
contrast.2.t.out[-1] <- apply(contrast.2.t.out[-1], 2, function(x) as.numeric(x))
contrast.2.f.out[-1] <- apply(contrast.2.f.out[-1], 2, function(x) as.numeric(x))
contrast.3.out[-1] <- apply(contrast.3.out[-1], 2, function(x) as.numeric(x))

  # Get the plots
plot.1.margin <- contrast.1.out %>%
  ggplot(aes(x = Contrast, y = Conditionb)) +
  geom_point(size = 4, color = ifelse(contrast.1.out$Pvalue < .05, "black", "gray75")) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 color = ifelse(contrast.1.out$Pvalue < .05, "black", "gray75"),
                 height = 0, size = 1) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Difference in Conditional Mean Reclassification from Definitive Condition",
       y = "",
       title = "") +
  #scale_y_discrete(labels = c("Control", "Normative")) +
  scale_x_continuous(limits = c(-1,1),
                     labels = scales::number_format(accuracy = 0.01)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 14))

plot.2.t.margin <- contrast.2.t.out %>%
  ggplot(aes(x = Contrast, y = fct_relevel(Condition,
                                           "Control","Norm, False",
                                           "Norm, True","Def, False"))) +
  geom_point(size = 4, color = ifelse(contrast.2.t.out$Pvalue < .05, "#481567FF", "gray75")) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 color = ifelse(contrast.2.t.out$Pvalue < .05, "#481567FF", "gray75"),
                 height = 0, size = 1) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  scale_y_discrete(labels = c("Control", "Norm,\nActual",
                              "Norm,\nInverted","Def,\nInverted"),
                   breaks = c("Control", "Norm, True",
                              "Norm, False","Def, False")) +
  scale_x_continuous(limits = c(-1,1),
                     labels = scales::number_format(accuracy = 0.01)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 14)) +
  labs(x = "Difference in Conditional Mean Reclassification from Definitive, Actual Condition",
       y = "")

# plot.2.f.margin <- contrast.2.f.out %>%
#   ggplot(aes(x = Contrast, y = fct_relevel(Condition,
#                                            "Control","Norm, False",
#                                            "Norm, True","Def, True"))) +
#   geom_point(size = 4, color = ifelse(contrast.2.f.out$Pvalue < .05, "#ec008b", "gray75")) +
#   geom_errorbarh(aes(xmin = Lower, xmax = Upper),
#                  color = ifelse(contrast.2.f.out$Pvalue < .05, "#ec008b", "gray75"),
#                  height = 0, size = 1) +
#   geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
#   scale_y_discrete(labels = c("Control", "Norm, True",
#                               "Norm, False","Def, True"),
#                    breaks = c("Control", "Norm, True",
#                               "Norm, False","Def, True")) +
#   scale_x_continuous(limits = c(-1,1),
#                      labels = scales::number_format(accuracy = 0.01)) +
#   theme(plot.title = element_text(hjust = 0.5),
#         axis.text.y = element_text(size = 14)) +
#   labs(x = "Difference in Conditional Mean Reclassification from Definitive, False Condition",
#        y = "")

plot.3.margin <- contrast.3.out %>%
  ggplot(aes(x = Contrast, y = fct_relevel(Conditionb, "Control","Affirmed"))) +
  geom_point(size = 4, color = ifelse(contrast.3.out$Pvalue < .05, "#55C667FF", "gray75")) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 color = ifelse(contrast.3.out$Pvalue < .05, "#55C667FF", "gray75"),
                 height = 0, size = 1) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  #scale_y_discrete(labels = c("Control", "Affirmed")) +
  scale_x_continuous(limits = c(-1,1),
                     labels = scales::number_format(accuracy = 0.01)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 14)) +
  labs(x = "Difference in Conditional Mean Reclassification from Definitive Condition",
       y = "")


######################################
#  Merge the plots
######################################

png("figures/main_predict.png", width = 6, height = 4, 
    units = "in", res = 600)
plot.1.ols
dev.off()

png("figures/main_margin.png", width = 6, height = 4, 
    units = "in", res = 600)
plot.1.margin
dev.off()

png("figures/extra_margin_predict.png", width = 12, height = 10, 
    units = "in", res = 600)
ggarrange(plot.2.t.margin, plot.2.ols, #plot.2.f.margin,
          plot.3.margin, plot.3.ols,
          align = "hv", ncol = 2, nrow = 2)
dev.off()


### END ###
