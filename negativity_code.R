##############################
##  Title: negativity_code.R
##  Note: Code for crecreating the 
##        paper models
##  Author: Redacted
##############################

### BEGIN ###


### Necessary packages
# install.paclages("pacman")
pacman::p_load(tidyverse,
               factoextra,
               FactoMineR,
               ggrepel,
               ggeffects,
               Hmisc,
               lmtest,
               sandwich,
               data.table,
               rcompanion,
               MuMIn,
               modeest,
               effects,
               psych,
               lme4,
               performance,
               janitor,
               install = T)


### Data
load("data/campaign.rda") # This is the raw data download from ICPSR. You will need to download
                          # the dataset (https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/34895),
                          # as I do not have authorization to distribute it. Note that I renamed the 
                          # file to "campaign."
campaign <- da34895.0001
rm(da34895.0001)

### Recode some variables
  # Generate a new candidate name variable with a few of the names corrected
  # so that rows match when using clustered SEs later
campaign$CANDNAME <- trimws(campaign$CANDNAME)
campaign$CANDNAME <- dplyr::recode(campaign$CANDNAME, 'CL "Butch" Otter' = "Butch Otter",
                    "Robin Weirauck" = "Robin Weirauch",
                    "Patrick J. Kennedy" = "Patrick Kennedy",
                    "Richard M. Romeo" = "Richard Romeo",
                    "Gary R. Page" = "Gary Page",
                    "Bernhard Sanders" = "Bernie Sanders",
                    "Frank Lobiondo" = "Frank LoBiondo",
                    "Mike Soarel" = "Mike Sodrel",
                    "Paul Fillmor" = "Paul Gillmor",
                    "Rosa Delauro" = "Rosa DeLauro",
                    "Diana Degette" = "Diana DeGette")


  # Generate a white/non-white binary variable
campaign$NONWHITE = "NA"
campaign$NONWHITE[campaign$BLACK == "(1) Black" | campaign$LATINO == "(1) Latino" | campaign$API == "(1) Asian-Pacific"] <- 1
campaign$NONWHITE[campaign$BLACK == "(0) Not Black" & campaign$LATINO == "(0) Not Latino" & 
                    campaign$API == "(0) Not Asian-Pacific"] <- 0

  # Get district/state pop per 1000
campaign$DPOP2 <- campaign$DPOP/1000

  # Generate "high-status negative" variable
campaign$PRESNEGATIVE = 0
campaign$PRESNEGATIVE[campaign$GONEGBUSH == "(1) negativity towards Bush" | campaign$GONEGCHENEY == "(1) negativity towards Cheney" | 
                        campaign$GONEGKERRY == "(1) negativity towards Kerry" | campaign$GONEGDEM == "(1) negativity towards Democratic party" | 
                        campaign$GONEGREP == "(1) negativity towards Republican party"] <- 1

  # Remove independents
campaign$DEMOCRAT2 = "NA"
campaign$DEMOCRAT2[campaign$PARTY == "(1) Republican"] <- 0
campaign$DEMOCRAT2[campaign$PARTY == "(2) Democrat"] <- 1

  # Log fundraising variable
campaign$LOGFRAISED <- log(campaign$FRAISED + 1)


### Get analytical sample
campaign.analysis <- na.omit(campaign[,c("BIOFAMIL2","BIOTOWN","BIOOCCUP","BIOORG","BIOVOLUN","BIOVET",
                                         "FEMALE","GONEG","ISSUEOWNER","ISSSAL","DEMOCRAT2","NONWHITE",
                                         "LOGFRAISED","SENATE","COOKS","CHALL","OPEN","DPOP2","DEDPERHS",
                                         "DINCMEDF","PRESNEGATIVE","YEAR","CANDNAME","OPPGONEG","ID_",
                                         "FRAISED","DISTRICT","STATE")])


### MCA
  # Carry out the MCA
vars <- c("BIOFAMIL2","BIOTOWN","BIOOCCUP",
          "BIOORG","BIOVOLUN","BIOVET")
campaign.sub <- campaign.analysis[vars]

campaign.mca <- MCA(campaign.sub, graph = F)
campaign.mca$eig #Dim.1 = 26.62%; Dim.2 = 18.20% var explained

  # Get the biplot
mca.plot <-
  
  fviz_mca_var(campaign.mca, repel = T, col.var="black") +
  labs(title = "", x="Dimension #1 (26.83%)", y = "Dimension #2 (18.18%)") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.position = "right")

pdf("figures/dv_mca.pdf", width=8, height=6)
mca.plot
dev.off()

  # Add coordinate for Dim.1 back to the data frame
campaign.analysis$names <- rownames(campaign.analysis)
ind.coord <- as.data.frame(campaign.mca$ind$coord[,1])
colnames(ind.coord) <- "mca_sd"
ind.coord$mca_sd <- scale(ind.coord$mca_sd)
ind.coord$names <- rownames(ind.coord)
campaign.analysis <- merge(campaign.analysis, ind.coord, by = "names")


### Scale ISSUEOWNER
campaign.analysis$SDISSUEOWNER <- scale(campaign.analysis$ISSUEOWNER)


### Fix a gender coding error
campaign.analysis$FEMALE[campaign.analysis$CANDNAME == "Katherine Harris" &
                           campaign.analysis$YEAR == 2004] <- "(1) Female"


### Descriptives
campaign.analysis[c("OPPGONEG","FEMALE","GONEG","DEMOCRAT2","NONWHITE",
                    "SENATE","CHALL","OPEN","PRESNEGATIVE","YEAR",
                    "BIOFAMIL2","BIOTOWN","BIOOCCUP","BIOORG","BIOVOLUN",
                    "BIOVET")] %>%
  # na.omit() %>%
  as.matrix() %>%
  Hmisc::describe(exclude.missing = T, digits = 2) #for categorical variables

campaign.analysis[c("mca_sd","SDISSUEOWNER","ISSSAL","FRAISED","COOKS",
                    "DPOP2","DEDPERHS","DINCMEDF")] %>%
  # na.omit() %>%
  as.data.frame() %>%
  psych::describe() #for continuous variables

campaign.analysis[c("mca_sd","SDISSUEOWNER","ISSSAL","FRAISED","COOKS",
                    "DPOP2","DEDPERHS","DINCMEDF")] %>%
  # na.omit() %>%
  as.data.frame() %>%
  summary() #for continuous variables, getting IQR


### How many "clusters"?
unique(campaign.analysis$CANDNAME) %>% length() #604


### Model 1
model.1 <- glm(OPPGONEG ~ mca_sd + FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
                 DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
                 DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + relevel(factor(YEAR), ref = "2004"),
               family = binomial(link = "probit"), data = campaign.analysis)

model.1.stats <- coeftest(model.1, vcovCL, type = "HC0", cluster =~ CANDNAME)

coeftest(model.1, vcovCL, type = "HC0", cluster =~ CANDNAME) %>% round(3)


### Model 2
model.2 <- glm(OPPGONEG ~ mca_sd*FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
                 DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
                 DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + relevel(factor(YEAR), ref = "2004"), 
               family = binomial(link = "probit"), data = campaign.analysis)

model.2.stats <- coeftest(model.2, vcovCL, type = "HC0", cluster =~ CANDNAME)

coeftest(model.2, vcovCL, type = "HC0", cluster =~ CANDNAME) %>% round(3)


### Coefficient plot
coef.1 <- model.1.stats %>% as.table() %>% as.data.frame() %>% spread(., Var2, Freq)
coef.2 <- model.2.stats %>% as.table() %>% as.data.frame() %>% spread(., Var2, Freq)

coef.1 <- rename(coef.1, var = Var1)
coef.2 <- rename(coef.2, var = Var1)


  # Add 95% CIs
coef.1$ll1 <- coef.1$Estimate - (coef.1$`Std. Error` * 1.96)
coef.1$ul1 <- coef.1$Estimate + (coef.1$`Std. Error` * 1.96)
coef.2$ll2 <- coef.2$Estimate - (coef.2$`Std. Error` * 1.96)
coef.2$ul2 <- coef.2$Estimate + (coef.2$`Std. Error` * 1.96)

  # Create empty DF
coefs <- data.frame(matrix(nrow = 27, ncol = 8))
colnames(coefs) <- c("id","var","estimate1","ll1","ul1","estimate2","ll2","ul2")
coefs$id <- 1:nrow(coefs)
coefs$var <- c("Independent Variables","Personal Availability (sd)","Female","Availability*Female"," ",
               "Controls","Went Negative","Issue Ownership (sd)","Issue Salience (%)","Ownership*Salience",
               "Democrat","Non-White","Funds Raised (log)","Senate","Competitiveness","Challenger","Open Seat",
               "Compete*Challenger","Compete*Open","District/State Pop (per 1000)","District/State % >= H.S. Diploma",
               "District/State Median Fam Inc","High-Status Negativity","  ", "Year Fixed-Effects","2002","2006")

  # Populate DF
# coef.1$names <- rownames(coef.1)
# coef.2$names <- rownames(coef.2)

      # Coefficients, Model 1
coefs$estimate1[coefs$var == "Personal Availability (sd)"] <- coef.1$Estimate[coef.1$var == "mca_sd"]
coefs$estimate1[coefs$var == "Female"] <- coef.1$Estimate[coef.1$var == "FEMALE(1) Female"]
coefs$estimate1[coefs$var == "Went Negative"] <- coef.1$Estimate[coef.1$var == "GONEG(1) negative towards/criticism of the opponent"]
coefs$estimate1[coefs$var == "Issue Ownership (sd)"] <- coef.1$Estimate[coef.1$var == "SDISSUEOWNER"]
coefs$estimate1[coefs$var == "Issue Salience (%)"] <- coef.1$Estimate[coef.1$var == "ISSSAL"]
coefs$estimate1[coefs$var == "Ownership*Salience"] <- coef.1$Estimate[coef.1$var == "SDISSUEOWNER:ISSSAL"]
coefs$estimate1[coefs$var == "Democrat"] <- coef.1$Estimate[coef.1$var == "DEMOCRAT21"]
coefs$estimate1[coefs$var == "Non-White"] <- coef.1$Estimate[coef.1$var == "NONWHITE1"]
coefs$estimate1[coefs$var == "Funds Raised (log)"] <- coef.1$Estimate[coef.1$var == "LOGFRAISED"]
coefs$estimate1[coefs$var == "Senate"] <- coef.1$Estimate[coef.1$var == "SENATE(1) Senate candidate"]
coefs$estimate1[coefs$var == "Competitiveness"] <- coef.1$Estimate[coef.1$var == "COOKS"]
coefs$estimate1[coefs$var == "Challenger"] <- coef.1$Estimate[coef.1$var == "CHALL(1) challenger"]
coefs$estimate1[coefs$var == "Open Seat"] <- coef.1$Estimate[coef.1$var == "OPEN(1) open seat candidate"]
coefs$estimate1[coefs$var == "Compete*Challenger"] <- coef.1$Estimate[coef.1$var == "COOKS:CHALL(1) challenger"]
coefs$estimate1[coefs$var == "Compete*Open"] <- coef.1$Estimate[coef.1$var == "COOKS:OPEN(1) open seat candidate"]
coefs$estimate1[coefs$var == "District/State Pop (per 1000)"] <- coef.1$Estimate[coef.1$var == "DPOP2"]
coefs$estimate1[coefs$var == "District/State % >= H.S. Diploma"] <- coef.1$Estimate[coef.1$var == "DEDPERHS"]
coefs$estimate1[coefs$var == "District/State Median Fam Inc"] <- coef.1$Estimate[coef.1$var == "DINCMEDF"]
coefs$estimate1[coefs$var == "High-Status Negativity"] <- coef.1$Estimate[coef.1$var == "PRESNEGATIVE"]
coefs$estimate1[coefs$var == "2002"] <- coef.1$Estimate[coef.1$var == 'relevel(factor(YEAR), ref = "2004")2002']
coefs$estimate1[coefs$var == "2006"] <- coef.1$Estimate[coef.1$var == 'relevel(factor(YEAR), ref = "2004")2006']

      # ll1, Model 1
coefs$ll1[coefs$var == "Personal Availability (sd)"] <- coef.1$ll1[coef.1$var == "mca_sd"]
coefs$ll1[coefs$var == "Female"] <- coef.1$ll1[coef.1$var == "FEMALE(1) Female"]
coefs$ll1[coefs$var == "Went Negative"] <- coef.1$ll1[coef.1$var == "GONEG(1) negative towards/criticism of the opponent"]
coefs$ll1[coefs$var == "Issue Ownership (sd)"] <- coef.1$ll1[coef.1$var == "SDISSUEOWNER"]
coefs$ll1[coefs$var == "Issue Salience (%)"] <- coef.1$ll1[coef.1$var == "ISSSAL"]
coefs$ll1[coefs$var == "Ownership*Salience"] <- coef.1$ll1[coef.1$var == "SDISSUEOWNER:ISSSAL"]
coefs$ll1[coefs$var == "Democrat"] <- coef.1$ll1[coef.1$var == "DEMOCRAT21"]
coefs$ll1[coefs$var == "Non-White"] <- coef.1$ll1[coef.1$var == "NONWHITE1"]
coefs$ll1[coefs$var == "Funds Raised (log)"] <- coef.1$ll1[coef.1$var == "LOGFRAISED"]
coefs$ll1[coefs$var == "Senate"] <- coef.1$ll1[coef.1$var == "SENATE(1) Senate candidate"]
coefs$ll1[coefs$var == "Competitiveness"] <- coef.1$ll1[coef.1$var == "COOKS"]
coefs$ll1[coefs$var == "Challenger"] <- coef.1$ll1[coef.1$var == "CHALL(1) challenger"]
coefs$ll1[coefs$var == "Open Seat"] <- coef.1$ll1[coef.1$var == "OPEN(1) open seat candidate"]
coefs$ll1[coefs$var == "Compete*Challenger"] <- coef.1$ll1[coef.1$var == "COOKS:CHALL(1) challenger"]
coefs$ll1[coefs$var == "Compete*Open"] <- coef.1$ll1[coef.1$var == "COOKS:OPEN(1) open seat candidate"]
coefs$ll1[coefs$var == "District/State Pop (per 1000)"] <- coef.1$ll1[coef.1$var == "DPOP2"]
coefs$ll1[coefs$var == "District/State % >= H.S. Diploma"] <- coef.1$ll1[coef.1$var == "DEDPERHS"]
coefs$ll1[coefs$var == "District/State Median Fam Inc"] <- coef.1$ll1[coef.1$var == "DINCMEDF"]
coefs$ll1[coefs$var == "High-Status Negativity"] <- coef.1$ll1[coef.1$var == "PRESNEGATIVE"]
coefs$ll1[coefs$var == "2002"] <- coef.1$ll1[coef.1$var == 'relevel(factor(YEAR), ref = "2004")2002']
coefs$ll1[coefs$var == "2006"] <- coef.1$ll1[coef.1$var == 'relevel(factor(YEAR), ref = "2004")2006']

      # ul1, Model 1
coefs$ul1[coefs$var == "Personal Availability (sd)"] <- coef.1$ul1[coef.1$var == "mca_sd"]
coefs$ul1[coefs$var == "Female"] <- coef.1$ul1[coef.1$var == "FEMALE(1) Female"]
coefs$ul1[coefs$var == "Went Negative"] <- coef.1$ul1[coef.1$var == "GONEG(1) negative towards/criticism of the opponent"]
coefs$ul1[coefs$var == "Issue Ownership (sd)"] <- coef.1$ul1[coef.1$var == "SDISSUEOWNER"]
coefs$ul1[coefs$var == "Issue Salience (%)"] <- coef.1$ul1[coef.1$var == "ISSSAL"]
coefs$ul1[coefs$var == "Ownership*Salience"] <- coef.1$ul1[coef.1$var == "SDISSUEOWNER:ISSSAL"]
coefs$ul1[coefs$var == "Democrat"] <- coef.1$ul1[coef.1$var == "DEMOCRAT21"]
coefs$ul1[coefs$var == "Non-White"] <- coef.1$ul1[coef.1$var == "NONWHITE1"]
coefs$ul1[coefs$var == "Funds Raised (log)"] <- coef.1$ul1[coef.1$var == "LOGFRAISED"]
coefs$ul1[coefs$var == "Senate"] <- coef.1$ul1[coef.1$var == "SENATE(1) Senate candidate"]
coefs$ul1[coefs$var == "Competitiveness"] <- coef.1$ul1[coef.1$var == "COOKS"]
coefs$ul1[coefs$var == "Challenger"] <- coef.1$ul1[coef.1$var == "CHALL(1) challenger"]
coefs$ul1[coefs$var == "Open Seat"] <- coef.1$ul1[coef.1$var == "OPEN(1) open seat candidate"]
coefs$ul1[coefs$var == "Compete*Challenger"] <- coef.1$ul1[coef.1$var == "COOKS:CHALL(1) challenger"]
coefs$ul1[coefs$var == "Compete*Open"] <- coef.1$ul1[coef.1$var == "COOKS:OPEN(1) open seat candidate"]
coefs$ul1[coefs$var == "District/State Pop (per 1000)"] <- coef.1$ul1[coef.1$var == "DPOP2"]
coefs$ul1[coefs$var == "District/State % >= H.S. Diploma"] <- coef.1$ul1[coef.1$var == "DEDPERHS"]
coefs$ul1[coefs$var == "District/State Median Fam Inc"] <- coef.1$ul1[coef.1$var == "DINCMEDF"]
coefs$ul1[coefs$var == "High-Status Negativity"] <- coef.1$ul1[coef.1$var == "PRESNEGATIVE"]
coefs$ul1[coefs$var == "2002"] <- coef.1$ul1[coef.1$var == 'relevel(factor(YEAR), ref = "2004")2002']
coefs$ul1[coefs$var == "2006"] <- coef.1$ul1[coef.1$var == 'relevel(factor(YEAR), ref = "2004")2006']

      # Coefficients, Model 2
coefs$estimate2[coefs$var == "Personal Availability (sd)"] <- coef.2$Estimate[coef.2$var == "mca_sd"]
coefs$estimate2[coefs$var == "Female"] <- coef.2$Estimate[coef.2$var == "FEMALE(1) Female"]
coefs$estimate2[coefs$var == "Availability*Female"] <- coef.2$Estimate[coef.2$var == "mca_sd:FEMALE(1) Female"]
coefs$estimate2[coefs$var == "Went Negative"] <- coef.2$Estimate[coef.2$var == "GONEG(1) negative towards/criticism of the opponent"]
coefs$estimate2[coefs$var == "Issue Ownership (sd)"] <- coef.2$Estimate[coef.2$var == "SDISSUEOWNER"]
coefs$estimate2[coefs$var == "Issue Salience (%)"] <- coef.2$Estimate[coef.2$var == "ISSSAL"]
coefs$estimate2[coefs$var == "Ownership*Salience"] <- coef.2$Estimate[coef.2$var == "SDISSUEOWNER:ISSSAL"]
coefs$estimate2[coefs$var == "Democrat"] <- coef.2$Estimate[coef.2$var == "DEMOCRAT21"]
coefs$estimate2[coefs$var == "Non-White"] <- coef.2$Estimate[coef.2$var == "NONWHITE1"]
coefs$estimate2[coefs$var == "Funds Raised (log)"] <- coef.2$Estimate[coef.2$var == "LOGFRAISED"]
coefs$estimate2[coefs$var == "Senate"] <- coef.2$Estimate[coef.2$var == "SENATE(1) Senate candidate"]
coefs$estimate2[coefs$var == "Competitiveness"] <- coef.2$Estimate[coef.2$var == "COOKS"]
coefs$estimate2[coefs$var == "Challenger"] <- coef.2$Estimate[coef.2$var == "CHALL(1) challenger"]
coefs$estimate2[coefs$var == "Open Seat"] <- coef.2$Estimate[coef.2$var == "OPEN(1) open seat candidate"]
coefs$estimate2[coefs$var == "Compete*Challenger"] <- coef.2$Estimate[coef.2$var == "COOKS:CHALL(1) challenger"]
coefs$estimate2[coefs$var == "Compete*Open"] <- coef.2$Estimate[coef.2$var == "COOKS:OPEN(1) open seat candidate"]
coefs$estimate2[coefs$var == "District/State Pop (per 1000)"] <- coef.2$Estimate[coef.2$var == "DPOP2"]
coefs$estimate2[coefs$var == "District/State % >= H.S. Diploma"] <- coef.2$Estimate[coef.2$var == "DEDPERHS"]
coefs$estimate2[coefs$var == "District/State Median Fam Inc"] <- coef.2$Estimate[coef.2$var == "DINCMEDF"]
coefs$estimate2[coefs$var == "High-Status Negativity"] <- coef.2$Estimate[coef.2$var == "PRESNEGATIVE"]
coefs$estimate2[coefs$var == "2002"] <- coef.2$Estimate[coef.2$var == 'relevel(factor(YEAR), ref = "2004")2002']
coefs$estimate2[coefs$var == "2006"] <- coef.2$Estimate[coef.2$var == 'relevel(factor(YEAR), ref = "2004")2006']

      # ll2, Model 2
coefs$ll2[coefs$var == "Personal Availability (sd)"] <- coef.2$ll2[coef.2$var == "mca_sd"]
coefs$ll2[coefs$var == "Female"] <- coef.2$ll2[coef.2$var == "FEMALE(1) Female"]
coefs$ll2[coefs$var == "Availability*Female"] <- coef.2$ll2[coef.2$var == "mca_sd:FEMALE(1) Female"]
coefs$ll2[coefs$var == "Went Negative"] <- coef.2$ll2[coef.2$var == "GONEG(1) negative towards/criticism of the opponent"]
coefs$ll2[coefs$var == "Issue Ownership (sd)"] <- coef.2$ll2[coef.2$var == "SDISSUEOWNER"]
coefs$ll2[coefs$var == "Issue Salience (%)"] <- coef.2$ll2[coef.2$var == "ISSSAL"]
coefs$ll2[coefs$var == "Ownership*Salience"] <- coef.2$ll2[coef.2$var == "SDISSUEOWNER:ISSSAL"]
coefs$ll2[coefs$var == "Democrat"] <- coef.2$ll2[coef.2$var == "DEMOCRAT21"]
coefs$ll2[coefs$var == "Non-White"] <- coef.2$ll2[coef.2$var == "NONWHITE1"]
coefs$ll2[coefs$var == "Funds Raised (log)"] <- coef.2$ll2[coef.2$var == "LOGFRAISED"]
coefs$ll2[coefs$var == "Senate"] <- coef.2$ll2[coef.2$var == "SENATE(1) Senate candidate"]
coefs$ll2[coefs$var == "Competitiveness"] <- coef.2$ll2[coef.2$var == "COOKS"]
coefs$ll2[coefs$var == "Challenger"] <- coef.2$ll2[coef.2$var == "CHALL(1) challenger"]
coefs$ll2[coefs$var == "Open Seat"] <- coef.2$ll2[coef.2$var == "OPEN(1) open seat candidate"]
coefs$ll2[coefs$var == "Compete*Challenger"] <- coef.2$ll2[coef.2$var == "COOKS:CHALL(1) challenger"]
coefs$ll2[coefs$var == "Compete*Open"] <- coef.2$ll2[coef.2$var == "COOKS:OPEN(1) open seat candidate"]
coefs$ll2[coefs$var == "District/State Pop (per 1000)"] <- coef.2$ll2[coef.2$var == "DPOP2"]
coefs$ll2[coefs$var == "District/State % >= H.S. Diploma"] <- coef.2$ll2[coef.2$var == "DEDPERHS"]
coefs$ll2[coefs$var == "District/State Median Fam Inc"] <- coef.2$ll2[coef.2$var == "DINCMEDF"]
coefs$ll2[coefs$var == "High-Status Negativity"] <- coef.2$ll2[coef.2$var == "PRESNEGATIVE"]
coefs$ll2[coefs$var == "2002"] <- coef.2$ll2[coef.2$var == 'relevel(factor(YEAR), ref = "2004")2002']
coefs$ll2[coefs$var == "2006"] <- coef.2$ll2[coef.2$var == 'relevel(factor(YEAR), ref = "2004")2006']

      # ul2, Model 2
coefs$ul2[coefs$var == "Personal Availability (sd)"] <- coef.2$ul2[coef.2$var == "mca_sd"]
coefs$ul2[coefs$var == "Female"] <- coef.2$ul2[coef.2$var == "FEMALE(1) Female"]
coefs$ul2[coefs$var == "Availability*Female"] <- coef.2$ul2[coef.2$var == "mca_sd:FEMALE(1) Female"]
coefs$ul2[coefs$var == "Went Negative"] <- coef.2$ul2[coef.2$var == "GONEG(1) negative towards/criticism of the opponent"]
coefs$ul2[coefs$var == "Issue Ownership (sd)"] <- coef.2$ul2[coef.2$var == "SDISSUEOWNER"]
coefs$ul2[coefs$var == "Issue Salience (%)"] <- coef.2$ul2[coef.2$var == "ISSSAL"]
coefs$ul2[coefs$var == "Ownership*Salience"] <- coef.2$ul2[coef.2$var == "SDISSUEOWNER:ISSSAL"]
coefs$ul2[coefs$var == "Democrat"] <- coef.2$ul2[coef.2$var == "DEMOCRAT21"]
coefs$ul2[coefs$var == "Non-White"] <- coef.2$ul2[coef.2$var == "NONWHITE1"]
coefs$ul2[coefs$var == "Funds Raised (log)"] <- coef.2$ul2[coef.2$var == "LOGFRAISED"]
coefs$ul2[coefs$var == "Senate"] <- coef.2$ul2[coef.2$var == "SENATE(1) Senate candidate"]
coefs$ul2[coefs$var == "Competitiveness"] <- coef.2$ul2[coef.2$var == "COOKS"]
coefs$ul2[coefs$var == "Challenger"] <- coef.2$ul2[coef.2$var == "CHALL(1) challenger"]
coefs$ul2[coefs$var == "Open Seat"] <- coef.2$ul2[coef.2$var == "OPEN(1) open seat candidate"]
coefs$ul2[coefs$var == "Compete*Challenger"] <- coef.2$ul2[coef.2$var == "COOKS:CHALL(1) challenger"]
coefs$ul2[coefs$var == "Compete*Open"] <- coef.2$ul2[coef.2$var == "COOKS:OPEN(1) open seat candidate"]
coefs$ul2[coefs$var == "District/State Pop (per 1000)"] <- coef.2$ul2[coef.2$var == "DPOP2"]
coefs$ul2[coefs$var == "District/State % >= H.S. Diploma"] <- coef.2$ul2[coef.2$var == "DEDPERHS"]
coefs$ul2[coefs$var == "District/State Median Fam Inc"] <- coef.2$ul2[coef.2$var == "DINCMEDF"]
coefs$ul2[coefs$var == "High-Status Negativity"] <- coef.2$ul2[coef.2$var == "PRESNEGATIVE"]
coefs$ul2[coefs$var == "2002"] <- coef.2$ul2[coef.2$var == 'relevel(factor(YEAR), ref = "2004")2002']
coefs$ul2[coefs$var == "2006"] <- coef.2$ul2[coef.2$var == 'relevel(factor(YEAR), ref = "2004")2006']

  # Make the plot
coefs$var <- factor(coefs$var, levels = coefs$var)

coefplot1 <- ggplot(coefs, aes(x = estimate1, y = as.factor(var))) +
  geom_point(aes(color = "Model 1", fill = "Model 1"),
             position = position_nudge(y = .2)) +
  geom_errorbarh(aes(xmin = ll1, xmax = ul1, color = "Model 1"), height = 0.2,
                 position = position_nudge(y = .2)) +
  geom_point(aes(x = estimate2, y = as.factor(var), color = "Model 2", fill = "Model 2"),
             position = position_nudge(y = -.2)) +
  geom_errorbarh(aes(xmin = ll2, xmax = ul2, color = "Model 2"), height = 0.2,
                 position = position_nudge(y = -.2)) +
  geom_vline(xintercept = 0, color="black") +
  xlab("Probit Estimates of Negativity Reception") +
  ylab("") +
  theme_bw() +
  theme(axis.title=element_text(face = "bold"),
        legend.position = "bottom",
        axis.ticks.y = element_blank()) +
  scale_y_discrete(limits=rev(levels(coefs$var)),
                   labels=c("Independent Variables" = expression(bold("Independent Variables")),
                            "Personal Availability (sd)" = "Personal Availability (sd)",
                            "Female" = "Female",
                            " " = "",
                            "Controls" = expression(bold("Controls")),
                            "Went Negative" = "Went Negative",
                            "Issue Ownership (sd)" = "Issue Ownership (sd)",
                            "Issue Salience (%)" = "Issue Salience (%)",
                            "Ownership*Salience" = "Ownership*Salience",
                            "Democrat" = "Democrat",
                            "Non-White" = "Non-White",
                            "Funds Raised (log)" = "Funds Raised (log)",
                            "Senate" = "Senate",
                            "Competitiveness" = "Competitiveness",
                            "Challenger" = "Challenger",
                            "Open Seat" = "Open Seat",
                            "Compete*Challenger" = "Compete*Challenger",
                            "Compete*Open" = "Compete*Open",
                            "District/State Pop (per 1000)" = "District/State Pop (per 1000)",
                            "District/State % >= H.S. Diploma" = "District/State % >= H.S. Diploma",
                            "District/State Median Fam Inc" = "District/State Median Fam Inc",
                            "High-Status Negativity" = "High-Status Negativity",
                            "  " = "",
                            "Year Fixed-Effects" = expression(bold("Year Fixed-Effects")),
                            "2002" = "2002",
                            "2006" = "2006")) +
  scale_color_manual(values = c("Model 1" = "#E69F00", "Model 2" = "#1696d2"), name = "") +
  scale_fill_manual(values = c("Model 1" = "#E69F00", "Model 2" = "#1696d2"), name = "")

pdf("figures/coefplot.pdf", width=6, height=8)
coefplot1
dev.off()


### Fit statistics and model comparison/selection
  # AIC
model.1$aic
model.2$aic #better

  # BIC
BIC(model.1) #slightly better
BIC(model.2)

  # AIC_c
AICc(model.1)
AICc(model.2) #better

  # Deviance
model.1$deviance
model.2$deviance #better

  # Pseudo R^2
nagelkerke(model.1)$Pseudo.R.squared.for.model.vs.null
nagelkerke(model.2)$Pseudo.R.squared.for.model.vs.null #slightly better

  # Likelihood ratio chi-square test
lrtest(model.1, model.2) #model 2 is better


### Predicted probabilities for model 2
model.2.pred <- glm(OPPGONEG ~ as.numeric(mca_sd)*factor(FEMALE) + factor(GONEG) + as.numeric(SDISSUEOWNER)*as.numeric(ISSSAL) + 
                 factor(DEMOCRAT2) + factor(NONWHITE) + as.numeric(LOGFRAISED) + factor(SENATE) + as.numeric(COOKS)*factor(CHALL) + 
                   as.numeric(COOKS)*factor(OPEN) + as.numeric(DPOP2) + as.numeric(DEDPERHS) + as.numeric(DINCMEDF) + 
                   factor(PRESNEGATIVE) + factor(YEAR), 
               family = binomial(link = "probit"), data = campaign.analysis) #ggpredict() can't deal with relevel().

pred.2 <- ggpredict(model.2.pred, terms = c("mca_sd [-1,0,1]", "FEMALE"),
                    condition = c(GONEG = as.character(mfv(campaign.analysis$GONEG)),
                                  DEMOCRAT2 = mfv(campaign.analysis$DEMOCRAT2),
                                  NONWHITE = mfv(campaign.analysis$NONWHITE),
                                  SENATE = as.character(mfv(campaign.analysis$SENATE)),
                                  CHALL = as.character(mfv(campaign.analysis$CHALL)),
                                  OPEN = as.character(mfv(campaign.analysis$OPEN)),
                                  PRESNEGATIVE = as.character(mfv(campaign.analysis$PRESNEGATIVE)),
                                  YEAR = "2004"))

  # Hypothesis tests that margins_i = margins_j
margins(model.2.pred, at = list(GONEG = as.character(mfv(campaign.analysis$GONEG)),
                                DEMOCRAT2 = mfv(campaign.analysis$DEMOCRAT2),
                                NONWHITE = mfv(campaign.analysis$NONWHITE),
                                SENATE = as.character(mfv(campaign.analysis$SENATE)),
                                CHALL = as.character(mfv(campaign.analysis$CHALL)),
                                OPEN = as.character(mfv(campaign.analysis$OPEN)),
                                PRESNEGATIVE = as.character(mfv(campaign.analysis$PRESNEGATIVE)),
                                YEAR = "2004",
                                FEMALE = c("(0) Male","(1) Female"),
                                SDISSUEOWNER = 0,
                                ISSSAL = 13.23,
                                LOGFRAISED = 13.38,
                                COOKS = 0.24,
                                DPOP2 = 1985.61,
                                DEDPERHS = 82.11,
                                DINCMEDF = 51210.01), 
        variables = "mca_sd") %>% summary()


### Predicted probabilities plot
predict.plot <- ggplot(pred.2, aes(x = x, y = predicted, group = group)) +
  geom_line(aes(color = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .2) +
  ylim(0, 1) + 
  xlab("") +
  ylab("Predicted Probability of Negativity Reception") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.title.y = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     values = c("(0) Male" = "#E69F00", 
                                       "(1) Female" = "#999999"),
                     labels = c("(0) Male" = "Male",
                                "(1) Female" = "Female")) +
  scale_fill_manual(name = "", 
                     values = c("(0) Male" = "#E69F00", 
                                "(1) Female" = "#999999"),
                     labels = c("(0) Male" = "Male",
                                "(1) Female" = "Female"))


### Get separate models for challenger status
  # Incumbents
inc.model <- glm(OPPGONEG ~ as.numeric(mca_sd)*factor(FEMALE) + factor(GONEG) + as.numeric(SDISSUEOWNER)*as.numeric(ISSSAL) + 
                      factor(DEMOCRAT2) + factor(NONWHITE) + as.numeric(LOGFRAISED) + factor(SENATE) + as.numeric(COOKS) +
                      as.numeric(DPOP2) + as.numeric(DEDPERHS) + as.numeric(DINCMEDF) + 
                      factor(PRESNEGATIVE) + factor(YEAR), 
                    family = binomial(link = "probit"), data = campaign.analysis[campaign.analysis$CHALL == "(0) not a challenger" &
                                                                                 campaign.analysis$OPEN == "(0) not an open seat candidate",])

coeftest(inc.model, vcovCL, type = "HC0", cluster =~ CANDNAME) %>% round(3) #sig

inc.pred <- ggpredict(inc.model, terms = c("mca_sd [-1,0,1]", "FEMALE"),
                    condition = c(GONEG = as.character(mfv(campaign.analysis$GONEG)),
                                  DEMOCRAT2 = mfv(campaign.analysis$DEMOCRAT2),
                                  NONWHITE = mfv(campaign.analysis$NONWHITE),
                                  SENATE = as.character(mfv(campaign.analysis$SENATE)),
                                  OPEN = as.character(mfv(campaign.analysis$OPEN)),
                                  PRESNEGATIVE = as.character(mfv(campaign.analysis$PRESNEGATIVE)),
                                  SDISSUEOWNER = mean(campaign.analysis$SDISSUEOWNER),
                                  ISSSAL = mean(campaign.analysis$ISSSAL),
                                  LOGFRAISED = mean(campaign.analysis$LOGFRAISED),
                                  COOKS = mean(campaign.analysis$COOKS),
                                  DPOP2 = mean(campaign.analysis$DPOP2),
                                  DEDPERHS = mean(campaign.analysis$DEDPERHS),
                                  DINCMEDF = mean(campaign.analysis$DINCMEDF),
                                  YEAR = "2004"))

  # Challengers
cha.model <- glm(OPPGONEG ~ as.numeric(mca_sd)*factor(FEMALE) + factor(GONEG) + as.numeric(SDISSUEOWNER)*as.numeric(ISSSAL) + 
                   factor(DEMOCRAT2) + factor(NONWHITE) + as.numeric(LOGFRAISED) + factor(SENATE) + as.numeric(COOKS) +
                   as.numeric(DPOP2) + as.numeric(DEDPERHS) + as.numeric(DINCMEDF) + 
                   factor(PRESNEGATIVE) + factor(YEAR), 
                 family = binomial(link = "probit"), data = campaign.analysis[campaign.analysis$CHALL == "(1) challenger",])

coeftest(cha.model, vcovCL, type = "HC0", cluster =~ CANDNAME) %>% round(3) #not sig

cha.pred <- ggpredict(cha.model, terms = c("mca_sd [-1,0,1]", "FEMALE"),
                      condition = c(GONEG = as.character(mfv(campaign.analysis$GONEG)),
                                    DEMOCRAT2 = mfv(campaign.analysis$DEMOCRAT2),
                                    NONWHITE = mfv(campaign.analysis$NONWHITE),
                                    SENATE = as.character(mfv(campaign.analysis$SENATE)),
                                    OPEN = as.character(mfv(campaign.analysis$OPEN)),
                                    PRESNEGATIVE = as.character(mfv(campaign.analysis$PRESNEGATIVE)),
                                    SDISSUEOWNER = mean(campaign.analysis$SDISSUEOWNER),
                                    ISSSAL = mean(campaign.analysis$ISSSAL),
                                    LOGFRAISED = mean(campaign.analysis$LOGFRAISED),
                                    COOKS = mean(campaign.analysis$COOKS),
                                    DPOP2 = mean(campaign.analysis$DPOP2),
                                    DEDPERHS = mean(campaign.analysis$DEDPERHS),
                                    DINCMEDF = mean(campaign.analysis$DINCMEDF),
                                    YEAR = "2004"))

  # Get separate predicted probability plots by challenger status
inc.plot <- ggplot(inc.pred, aes(x = x, y = predicted, group = group)) +
  geom_line(aes(color = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .2) +
  ylim(0, 1) + 
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.title.y = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     values = c("(0) Male" = "#E69F00", 
                                "(1) Female" = "#999999"),
                     labels = c("(0) Male" = "Male",
                                "(1) Female" = "Female")) +
  scale_fill_manual(name = "", 
                    values = c("(0) Male" = "#E69F00", 
                               "(1) Female" = "#999999"),
                    labels = c("(0) Male" = "Male",
                               "(1) Female" = "Female"))

cha.plot <- ggplot(cha.pred, aes(x = x, y = predicted, group = group)) +
  geom_line(aes(color = group), alpha = .2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .1) +
  ylim(0, 1) + 
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.title.y = element_text(face = "bold")) +
  scale_color_manual(name = "", 
                     values = c("(0) Male" = "#E69F00", 
                                "(1) Female" = "#999999"),
                     labels = c("(0) Male" = "Male",
                                "(1) Female" = "Female")) +
  scale_fill_manual(name = "", 
                    values = c("(0) Male" = "#E69F00", 
                               "(1) Female" = "#999999"),
                    labels = c("(0) Male" = "Male",
                               "(1) Female" = "Female"))


### Put all of the predicted probability plots together
pred.figure <- ggarrange(predict.plot, 
                          annotate_figure(ggarrange(cha.plot, inc.plot, nrow = 2, 
                                                    labels = c("B", "C"), legend = "none",
                                                    align = "hv")),
                         labels = "A", common.legend = T, legend = "bottom", 
                          ncol = 2, widths = c(1,1), align = "v")

pdf("figures/predict.pdf", width = 8, height = 6)
pred.figure
dev.off()


### Robustness Checks
  # Temporal Stability
    # Models and predicted probabilities
y2002.model <- glm(OPPGONEG ~ as.numeric(mca_sd)*factor(FEMALE) + factor(GONEG) + as.numeric(SDISSUEOWNER)*as.numeric(ISSSAL) + 
                      factor(DEMOCRAT2) + as.numeric(LOGFRAISED) + factor(SENATE) + as.numeric(COOKS)*factor(CHALL) + 
                      as.numeric(COOKS)*factor(OPEN) + as.numeric(DPOP2) + as.numeric(DEDPERHS) + as.numeric(DINCMEDF), 
                    family = binomial(link = "probit"), data = campaign.analysis[campaign.analysis$YEAR == "2002",]) #Had to drop the NONWHITE and
                                                                                                #PRESNEGATIVE variables b/c those cases perfectly
                                                                                                #predict the O category on the DV.

y2002.stats <- coeftest(y2002.model, vcovCL, type = "HC0", cluster =~ CANDNAME)

y2002.pred <- ggpredict(y2002.model, terms = c("mca_sd [-1,0,1]", "FEMALE"),
                        condition = c(GONEG = as.character(mfv(campaign.analysis$GONEG)),
                                      DEMOCRAT2 = mfv(campaign.analysis$DEMOCRAT2),
                                      #NONWHITE = mfv(campaign.analysis$NONWHITE),
                                      SENATE = as.character(mfv(campaign.analysis$SENATE)),
                                      OPEN = as.character(mfv(campaign.analysis$OPEN)),
                                      #PRESNEGATIVE = as.character(mfv(campaign.analysis$PRESNEGATIVE)),
                                      SDISSUEOWNER = mean(campaign.analysis$SDISSUEOWNER),
                                      ISSSAL = mean(campaign.analysis$ISSSAL),
                                      LOGFRAISED = mean(campaign.analysis$LOGFRAISED),
                                      COOKS = mean(campaign.analysis$COOKS),
                                      DPOP2 = mean(campaign.analysis$DPOP2),
                                      DEDPERHS = mean(campaign.analysis$DEDPERHS),
                                      DINCMEDF = mean(campaign.analysis$DINCMEDF)))

y2004.model <- glm(OPPGONEG ~ as.numeric(mca_sd)*factor(FEMALE) + factor(GONEG) + as.numeric(SDISSUEOWNER)*as.numeric(ISSSAL) + 
                     factor(DEMOCRAT2) + factor(NONWHITE) + as.numeric(LOGFRAISED) + factor(SENATE) + as.numeric(COOKS)*factor(CHALL) + 
                     as.numeric(COOKS)*factor(OPEN) + as.numeric(DPOP2) + as.numeric(DEDPERHS) + as.numeric(DINCMEDF) + 
                     factor(PRESNEGATIVE), 
                   family = binomial(link = "probit"), data = campaign.analysis[campaign.analysis$YEAR == "2004",])

y2004.stats <- coeftest(y2004.model, vcovCL, type = "HC0", cluster =~ CANDNAME)

y2004.pred <- ggpredict(y2004.model, terms = c("mca_sd [-1,0,1]", "FEMALE"),
                        condition = c(GONEG = as.character(mfv(campaign.analysis$GONEG)),
                                      DEMOCRAT2 = mfv(campaign.analysis$DEMOCRAT2),
                                      NONWHITE = mfv(campaign.analysis$NONWHITE),
                                      SENATE = as.character(mfv(campaign.analysis$SENATE)),
                                      OPEN = as.character(mfv(campaign.analysis$OPEN)),
                                      PRESNEGATIVE = as.character(mfv(campaign.analysis$PRESNEGATIVE)),
                                      SDISSUEOWNER = mean(campaign.analysis$SDISSUEOWNER),
                                      ISSSAL = mean(campaign.analysis$ISSSAL),
                                      LOGFRAISED = mean(campaign.analysis$LOGFRAISED),
                                      COOKS = mean(campaign.analysis$COOKS),
                                      DPOP2 = mean(campaign.analysis$DPOP2),
                                      DEDPERHS = mean(campaign.analysis$DEDPERHS),
                                      DINCMEDF = mean(campaign.analysis$DINCMEDF)))

y2006.model <- glm(OPPGONEG ~ as.numeric(mca_sd)*factor(FEMALE) + factor(GONEG) + as.numeric(SDISSUEOWNER)*as.numeric(ISSSAL) + 
                     factor(DEMOCRAT2) + factor(NONWHITE) + as.numeric(LOGFRAISED) + factor(SENATE) + as.numeric(COOKS)*factor(CHALL) + 
                     as.numeric(COOKS)*factor(OPEN) + as.numeric(DPOP2) + as.numeric(DEDPERHS) + as.numeric(DINCMEDF) + 
                     factor(PRESNEGATIVE), 
                   family = binomial(link = "probit"), data = campaign.analysis[campaign.analysis$YEAR == "2006",])

y2006.stats <- coeftest(y2006.model, vcovCL, type = "HC0", cluster =~ CANDNAME)

y2006.pred <- ggpredict(y2006.model, terms = c("mca_sd [-1,0,1]", "FEMALE"),
                        condition = c(GONEG = as.character(mfv(campaign.analysis$GONEG)),
                                      DEMOCRAT2 = mfv(campaign.analysis$DEMOCRAT2),
                                      NONWHITE = mfv(campaign.analysis$NONWHITE),
                                      SENATE = as.character(mfv(campaign.analysis$SENATE)),
                                      OPEN = as.character(mfv(campaign.analysis$OPEN)),
                                      PRESNEGATIVE = as.character(mfv(campaign.analysis$PRESNEGATIVE)),
                                      SDISSUEOWNER = mean(campaign.analysis$SDISSUEOWNER),
                                      ISSSAL = mean(campaign.analysis$ISSSAL),
                                      LOGFRAISED = mean(campaign.analysis$LOGFRAISED),
                                      COOKS = mean(campaign.analysis$COOKS),
                                      DPOP2 = mean(campaign.analysis$DPOP2),
                                      DEDPERHS = mean(campaign.analysis$DEDPERHS),
                                      DINCMEDF = mean(campaign.analysis$DINCMEDF)))

    # Coefficient plot
coef.02 <- y2002.stats %>% as.table() %>% as.data.frame() %>% spread(., Var2, Freq)
coef.04 <- y2004.stats %>% as.table() %>% as.data.frame() %>% spread(., Var2, Freq)
coef.06 <- y2006.stats %>% as.table() %>% as.data.frame() %>% spread(., Var2, Freq)

coef.02 <- rename(coef.02, var = Var1)
coef.04 <- rename(coef.04, var = Var1)
coef.06 <- rename(coef.06, var = Var1)

      # Add 95% CIs
coef.02$ll1 <- coef.02$Estimate - (coef.02$`Std. Error` * 1.96)
coef.02$ul1 <- coef.02$Estimate + (coef.02$`Std. Error` * 1.96)
coef.04$ll2 <- coef.04$Estimate - (coef.04$`Std. Error` * 1.96)
coef.04$ul2 <- coef.04$Estimate + (coef.04$`Std. Error` * 1.96)
coef.06$ll3 <- coef.06$Estimate - (coef.06$`Std. Error` * 1.96)
coef.06$ul3 <- coef.06$Estimate + (coef.06$`Std. Error` * 1.96)

      # Create empty DF
coefs.y <- data.frame(matrix(nrow = 23, ncol = 11))
colnames(coefs.y) <- c("id","var","estimate1","ll1","ul1","estimate2","ll2","ul2","estimate3","ll3","ul3")
coefs.y$id <- 1:nrow(coefs.y)
coefs.y$var <- c("Independent Variables","Personal Availability (sd)","Female","Availability*Female"," ",
               "Controls","Went Negative","Issue Ownership (sd)","Issue Salience (%)","Ownership*Salience",
               "Democrat","Non-White","Funds Raised (log)","Senate","Competitiveness","Challenger","Open Seat",
               "Compete*Challenger","Compete*Open","District/State Pop (per 1000)","District/State % >= H.S. Diploma",
               "District/State Median Fam Inc","High-Status Negativity")

      # Populate the DF
        # Coefficients, 2002
coefs.y$estimate1[coefs.y$var == "Personal Availability (sd)"] <- coef.02$Estimate[coef.02$var == "as.numeric(mca_sd)"]
coefs.y$estimate1[coefs.y$var == "Female"] <- coef.02$Estimate[coef.02$var == "factor(FEMALE)(1) Female"]
coefs.y$estimate1[coefs.y$var == "Availability*Female"] <- coef.02$Estimate[coef.02$var == "as.numeric(mca_sd):factor(FEMALE)(1) Female"]
coefs.y$estimate1[coefs.y$var == "Went Negative"] <- coef.02$Estimate[coef.02$var == "factor(GONEG)(1) negative towards/criticism of the opponent"]
coefs.y$estimate1[coefs.y$var == "Issue Ownership (sd)"] <- coef.02$Estimate[coef.02$var == "as.numeric(SDISSUEOWNER)"]
coefs.y$estimate1[coefs.y$var == "Issue Salience (%)"] <- coef.02$Estimate[coef.02$var == "as.numeric(ISSSAL)"]
coefs.y$estimate1[coefs.y$var == "Ownership*Salience"] <- coef.02$Estimate[coef.02$var == "as.numeric(SDISSUEOWNER):as.numeric(ISSSAL)"]
coefs.y$estimate1[coefs.y$var == "Democrat"] <- coef.02$Estimate[coef.02$var == "factor(DEMOCRAT2)1"]
# coefs.y$estimate1[coefs.y$var == "Non-White"] <- coef.02$Estimate[coef.02$var == "factor(NONWHITE)1"]
coefs.y$estimate1[coefs.y$var == "Funds Raised (log)"] <- coef.02$Estimate[coef.02$var == "as.numeric(LOGFRAISED)"]
coefs.y$estimate1[coefs.y$var == "Senate"] <- coef.02$Estimate[coef.02$var == "factor(SENATE)(1) Senate candidate"]
coefs.y$estimate1[coefs.y$var == "Competitiveness"] <- coef.02$Estimate[coef.02$var == "as.numeric(COOKS)"]
coefs.y$estimate1[coefs.y$var == "Challenger"] <- coef.02$Estimate[coef.02$var == "factor(CHALL)(1) challenger"]
coefs.y$estimate1[coefs.y$var == "Open Seat"] <- coef.02$Estimate[coef.02$var == "factor(OPEN)(1) open seat candidate"]
coefs.y$estimate1[coefs.y$var == "Compete*Challenger"] <- coef.02$Estimate[coef.02$var == "as.numeric(COOKS):factor(CHALL)(1) challenger"]
coefs.y$estimate1[coefs.y$var == "Compete*Open"] <- coef.02$Estimate[coef.02$var == "as.numeric(COOKS):factor(OPEN)(1) open seat candidate"]
coefs.y$estimate1[coefs.y$var == "District/State Pop (per 1000)"] <- coef.02$Estimate[coef.02$var == "as.numeric(DPOP2)"]
coefs.y$estimate1[coefs.y$var == "District/State % >= H.S. Diploma"] <- coef.02$Estimate[coef.02$var == "as.numeric(DEDPERHS)"]
coefs.y$estimate1[coefs.y$var == "District/State Median Fam Inc"] <- coef.02$Estimate[coef.02$var == "as.numeric(DINCMEDF)"]
# coefs.y$estimate1[coefs.y$var == "High-Status Negativity"] <- coef.02$Estimate[coef.02$var == "factor(PRESNEGATIVE)1"]

      # ll1, 2002
coefs.y$ll1[coefs.y$var == "Personal Availability (sd)"] <- coef.02$ll1[coef.02$var == "as.numeric(mca_sd)"]
coefs.y$ll1[coefs.y$var == "Female"] <- coef.02$ll1[coef.02$var == "factor(FEMALE)(1) Female"]
coefs.y$ll1[coefs.y$var == "Availability*Female"] <- coef.02$ll1[coef.02$var == "as.numeric(mca_sd):factor(FEMALE)(1) Female"]
coefs.y$ll1[coefs.y$var == "Went Negative"] <- coef.02$ll1[coef.02$var == "factor(GONEG)(1) negative towards/criticism of the opponent"]
coefs.y$ll1[coefs.y$var == "Issue Ownership (sd)"] <- coef.02$ll1[coef.02$var == "as.numeric(SDISSUEOWNER)"]
coefs.y$ll1[coefs.y$var == "Issue Salience (%)"] <- coef.02$ll1[coef.02$var == "as.numeric(ISSSAL)"]
coefs.y$ll1[coefs.y$var == "Ownership*Salience"] <- coef.02$ll1[coef.02$var == "as.numeric(SDISSUEOWNER):as.numeric(ISSSAL)"]
coefs.y$ll1[coefs.y$var == "Democrat"] <- coef.02$ll1[coef.02$var == "factor(DEMOCRAT2)1"]
# coefs.y$ll1[coefs.y$var == "Non-White"] <- coef.02$ll1[coef.02$var == "factor(NONWHITE)1"]
coefs.y$ll1[coefs.y$var == "Funds Raised (log)"] <- coef.02$ll1[coef.02$var == "as.numeric(LOGFRAISED)"]
coefs.y$ll1[coefs.y$var == "Senate"] <- coef.02$ll1[coef.02$var == "factor(SENATE)(1) Senate candidate"]
coefs.y$ll1[coefs.y$var == "Competitiveness"] <- coef.02$ll1[coef.02$var == "as.numeric(COOKS)"]
coefs.y$ll1[coefs.y$var == "Challenger"] <- coef.02$ll1[coef.02$var == "factor(CHALL)(1) challenger"]
coefs.y$ll1[coefs.y$var == "Open Seat"] <- coef.02$ll1[coef.02$var == "factor(OPEN)(1) open seat candidate"]
coefs.y$ll1[coefs.y$var == "Compete*Challenger"] <- coef.02$ll1[coef.02$var == "as.numeric(COOKS):factor(CHALL)(1) challenger"]
coefs.y$ll1[coefs.y$var == "Compete*Open"] <- coef.02$ll1[coef.02$var == "as.numeric(COOKS):factor(OPEN)(1) open seat candidate"]
coefs.y$ll1[coefs.y$var == "District/State Pop (per 1000)"] <- coef.02$ll1[coef.02$var == "as.numeric(DPOP2)"]
coefs.y$ll1[coefs.y$var == "District/State % >= H.S. Diploma"] <- coef.02$ll1[coef.02$var == "as.numeric(DEDPERHS)"]
coefs.y$ll1[coefs.y$var == "District/State Median Fam Inc"] <- coef.02$ll1[coef.02$var == "as.numeric(DINCMEDF)"]
# coefs.y$ll1[coefs.y$var == "High-Status Negativity"] <- coef.02$ll1[coef.02$var == "factor(PRESNEGATIVE)1"]

      # ul1, 2002
coefs.y$ul1[coefs.y$var == "Personal Availability (sd)"] <- coef.02$ul1[coef.02$var == "as.numeric(mca_sd)"]
coefs.y$ul1[coefs.y$var == "Female"] <- coef.02$ul1[coef.02$var == "factor(FEMALE)(1) Female"]
coefs.y$ul1[coefs.y$var == "Availability*Female"] <- coef.02$ul1[coef.02$var == "as.numeric(mca_sd):factor(FEMALE)(1) Female"]
coefs.y$ul1[coefs.y$var == "Went Negative"] <- coef.02$ul1[coef.02$var == "factor(GONEG)(1) negative towards/criticism of the opponent"]
coefs.y$ul1[coefs.y$var == "Issue Ownership (sd)"] <- coef.02$ul1[coef.02$var == "as.numeric(SDISSUEOWNER)"]
coefs.y$ul1[coefs.y$var == "Issue Salience (%)"] <- coef.02$ul1[coef.02$var == "as.numeric(ISSSAL)"]
coefs.y$ul1[coefs.y$var == "Ownership*Salience"] <- coef.02$ul1[coef.02$var == "as.numeric(SDISSUEOWNER):as.numeric(ISSSAL)"]
coefs.y$ul1[coefs.y$var == "Democrat"] <- coef.02$ul1[coef.02$var == "factor(DEMOCRAT2)1"]
# coefs.y$ul1[coefs.y$var == "Non-White"] <- coef.02$ul1[coef.02$var == "factor(NONWHITE)1"]
coefs.y$ul1[coefs.y$var == "Funds Raised (log)"] <- coef.02$ul1[coef.02$var == "as.numeric(LOGFRAISED)"]
coefs.y$ul1[coefs.y$var == "Senate"] <- coef.02$ul1[coef.02$var == "factor(SENATE)(1) Senate candidate"]
coefs.y$ul1[coefs.y$var == "Competitiveness"] <- coef.02$ul1[coef.02$var == "as.numeric(COOKS)"]
coefs.y$ul1[coefs.y$var == "Challenger"] <- coef.02$ul1[coef.02$var == "factor(CHALL)(1) challenger"]
coefs.y$ul1[coefs.y$var == "Open Seat"] <- coef.02$ul1[coef.02$var == "factor(OPEN)(1) open seat candidate"]
coefs.y$ul1[coefs.y$var == "Compete*Challenger"] <- coef.02$ul1[coef.02$var == "as.numeric(COOKS):factor(CHALL)(1) challenger"]
coefs.y$ul1[coefs.y$var == "Compete*Open"] <- coef.02$ul1[coef.02$var == "as.numeric(COOKS):factor(OPEN)(1) open seat candidate"]
coefs.y$ul1[coefs.y$var == "District/State Pop (per 1000)"] <- coef.02$ul1[coef.02$var == "as.numeric(DPOP2)"]
coefs.y$ul1[coefs.y$var == "District/State % >= H.S. Diploma"] <- coef.02$ul1[coef.02$var == "as.numeric(DEDPERHS)"]
coefs.y$ul1[coefs.y$var == "District/State Median Fam Inc"] <- coef.02$ul1[coef.02$var == "as.numeric(DINCMEDF)"]
# coefs.y$ul1[coefs.y$var == "High-Status Negativity"] <- coef.02$ul1[coef.02$var == "factor(PRESNEGATIVE)1"]

      # Coefficients, 2004
coefs.y$estimate2[coefs.y$var == "Personal Availability (sd)"] <- coef.04$Estimate[coef.04$var == "as.numeric(mca_sd)"]
coefs.y$estimate2[coefs.y$var == "Female"] <- coef.04$Estimate[coef.04$var == "factor(FEMALE)(1) Female"]
coefs.y$estimate2[coefs.y$var == "Availability*Female"] <- coef.04$Estimate[coef.04$var == "as.numeric(mca_sd):factor(FEMALE)(1) Female"]
coefs.y$estimate2[coefs.y$var == "Went Negative"] <- coef.04$Estimate[coef.04$var == "factor(GONEG)(1) negative towards/criticism of the opponent"]
coefs.y$estimate2[coefs.y$var == "Issue Ownership (sd)"] <- coef.04$Estimate[coef.04$var == "as.numeric(SDISSUEOWNER)"]
coefs.y$estimate2[coefs.y$var == "Issue Salience (%)"] <- coef.04$Estimate[coef.04$var == "as.numeric(ISSSAL)"]
coefs.y$estimate2[coefs.y$var == "Ownership*Salience"] <- coef.04$Estimate[coef.04$var == "as.numeric(SDISSUEOWNER):as.numeric(ISSSAL)"]
coefs.y$estimate2[coefs.y$var == "Democrat"] <- coef.04$Estimate[coef.04$var == "factor(DEMOCRAT2)1"]
coefs.y$estimate2[coefs.y$var == "Non-White"] <- coef.04$Estimate[coef.04$var == "factor(NONWHITE)1"]
coefs.y$estimate2[coefs.y$var == "Funds Raised (log)"] <- coef.04$Estimate[coef.04$var == "as.numeric(LOGFRAISED)"]
coefs.y$estimate2[coefs.y$var == "Senate"] <- coef.04$Estimate[coef.04$var == "factor(SENATE)(1) Senate candidate"]
coefs.y$estimate2[coefs.y$var == "Competitiveness"] <- coef.04$Estimate[coef.04$var == "as.numeric(COOKS)"]
coefs.y$estimate2[coefs.y$var == "Challenger"] <- coef.04$Estimate[coef.04$var == "factor(CHALL)(1) challenger"]
coefs.y$estimate2[coefs.y$var == "Open Seat"] <- coef.04$Estimate[coef.04$var == "factor(OPEN)(1) open seat candidate"]
coefs.y$estimate2[coefs.y$var == "Compete*Challenger"] <- coef.04$Estimate[coef.04$var == "as.numeric(COOKS):factor(CHALL)(1) challenger"]
coefs.y$estimate2[coefs.y$var == "Compete*Open"] <- coef.04$Estimate[coef.04$var == "as.numeric(COOKS):factor(OPEN)(1) open seat candidate"]
coefs.y$estimate2[coefs.y$var == "District/State Pop (per 1000)"] <- coef.04$Estimate[coef.04$var == "as.numeric(DPOP2)"]
coefs.y$estimate2[coefs.y$var == "District/State % >= H.S. Diploma"] <- coef.04$Estimate[coef.04$var == "as.numeric(DEDPERHS)"]
coefs.y$estimate2[coefs.y$var == "District/State Median Fam Inc"] <- coef.04$Estimate[coef.04$var == "as.numeric(DINCMEDF)"]
coefs.y$estimate2[coefs.y$var == "High-Status Negativity"] <- coef.04$Estimate[coef.04$var == "factor(PRESNEGATIVE)1"]

      # ll2, 2004
coefs.y$ll2[coefs.y$var == "Personal Availability (sd)"] <- coef.04$ll2[coef.04$var == "as.numeric(mca_sd)"]
coefs.y$ll2[coefs.y$var == "Female"] <- coef.04$ll2[coef.04$var == "factor(FEMALE)(1) Female"]
coefs.y$ll2[coefs.y$var == "Availability*Female"] <- coef.04$ll2[coef.04$var == "as.numeric(mca_sd):factor(FEMALE)(1) Female"]
coefs.y$ll2[coefs.y$var == "Went Negative"] <- coef.04$ll2[coef.04$var == "factor(GONEG)(1) negative towards/criticism of the opponent"]
coefs.y$ll2[coefs.y$var == "Issue Ownership (sd)"] <- coef.04$ll2[coef.04$var == "as.numeric(SDISSUEOWNER)"]
coefs.y$ll2[coefs.y$var == "Issue Salience (%)"] <- coef.04$ll2[coef.04$var == "as.numeric(ISSSAL)"]
coefs.y$ll2[coefs.y$var == "Ownership*Salience"] <- coef.04$ll2[coef.04$var == "as.numeric(SDISSUEOWNER):as.numeric(ISSSAL)"]
coefs.y$ll2[coefs.y$var == "Democrat"] <- coef.04$ll2[coef.04$var == "factor(DEMOCRAT2)1"]
coefs.y$ll2[coefs.y$var == "Non-White"] <- coef.04$ll2[coef.04$var == "factor(NONWHITE)1"]
coefs.y$ll2[coefs.y$var == "Funds Raised (log)"] <- coef.04$ll2[coef.04$var == "as.numeric(LOGFRAISED)"]
coefs.y$ll2[coefs.y$var == "Senate"] <- coef.04$ll2[coef.04$var == "factor(SENATE)(1) Senate candidate"]
coefs.y$ll2[coefs.y$var == "Competitiveness"] <- coef.04$ll2[coef.04$var == "as.numeric(COOKS)"]
coefs.y$ll2[coefs.y$var == "Challenger"] <- coef.04$ll2[coef.04$var == "factor(CHALL)(1) challenger"]
coefs.y$ll2[coefs.y$var == "Open Seat"] <- coef.04$ll2[coef.04$var == "factor(OPEN)(1) open seat candidate"]
coefs.y$ll2[coefs.y$var == "Compete*Challenger"] <- coef.04$ll2[coef.04$var == "as.numeric(COOKS):factor(CHALL)(1) challenger"]
coefs.y$ll2[coefs.y$var == "Compete*Open"] <- coef.04$ll2[coef.04$var == "as.numeric(COOKS):factor(OPEN)(1) open seat candidate"]
coefs.y$ll2[coefs.y$var == "District/State Pop (per 1000)"] <- coef.04$ll2[coef.04$var == "as.numeric(DPOP2)"]
coefs.y$ll2[coefs.y$var == "District/State % >= H.S. Diploma"] <- coef.04$ll2[coef.04$var == "as.numeric(DEDPERHS)"]
coefs.y$ll2[coefs.y$var == "District/State Median Fam Inc"] <- coef.04$ll2[coef.04$var == "as.numeric(DINCMEDF)"]
coefs.y$ll2[coefs.y$var == "High-Status Negativity"] <- coef.04$ll2[coef.04$var == "factor(PRESNEGATIVE)1"]

      # ul2, 2004
coefs.y$ul2[coefs.y$var == "Personal Availability (sd)"] <- coef.04$ul2[coef.04$var == "as.numeric(mca_sd)"]
coefs.y$ul2[coefs.y$var == "Female"] <- coef.04$ul2[coef.04$var == "factor(FEMALE)(1) Female"]
coefs.y$ul2[coefs.y$var == "Availability*Female"] <- coef.04$ul2[coef.04$var == "as.numeric(mca_sd):factor(FEMALE)(1) Female"]
coefs.y$ul2[coefs.y$var == "Went Negative"] <- coef.04$ul2[coef.04$var == "factor(GONEG)(1) negative towards/criticism of the opponent"]
coefs.y$ul2[coefs.y$var == "Issue Ownership (sd)"] <- coef.04$ul2[coef.04$var == "as.numeric(SDISSUEOWNER)"]
coefs.y$ul2[coefs.y$var == "Issue Salience (%)"] <- coef.04$ul2[coef.04$var == "as.numeric(ISSSAL)"]
coefs.y$ul2[coefs.y$var == "Ownership*Salience"] <- coef.04$ul2[coef.04$var == "as.numeric(SDISSUEOWNER):as.numeric(ISSSAL)"]
coefs.y$ul2[coefs.y$var == "Democrat"] <- coef.04$ul2[coef.04$var == "factor(DEMOCRAT2)1"]
coefs.y$ul2[coefs.y$var == "Non-White"] <- coef.04$ul2[coef.04$var == "factor(NONWHITE)1"]
coefs.y$ul2[coefs.y$var == "Funds Raised (log)"] <- coef.04$ul2[coef.04$var == "as.numeric(LOGFRAISED)"]
coefs.y$ul2[coefs.y$var == "Senate"] <- coef.04$ul2[coef.04$var == "factor(SENATE)(1) Senate candidate"]
coefs.y$ul2[coefs.y$var == "Competitiveness"] <- coef.04$ul2[coef.04$var == "as.numeric(COOKS)"]
coefs.y$ul2[coefs.y$var == "Challenger"] <- coef.04$ul2[coef.04$var == "factor(CHALL)(1) challenger"]
coefs.y$ul2[coefs.y$var == "Open Seat"] <- coef.04$ul2[coef.04$var == "factor(OPEN)(1) open seat candidate"]
coefs.y$ul2[coefs.y$var == "Compete*Challenger"] <- coef.04$ul2[coef.04$var == "as.numeric(COOKS):factor(CHALL)(1) challenger"]
coefs.y$ul2[coefs.y$var == "Compete*Open"] <- coef.04$ul2[coef.04$var == "as.numeric(COOKS):factor(OPEN)(1) open seat candidate"]
coefs.y$ul2[coefs.y$var == "District/State Pop (per 1000)"] <- coef.04$ul2[coef.04$var == "as.numeric(DPOP2)"]
coefs.y$ul2[coefs.y$var == "District/State % >= H.S. Diploma"] <- coef.04$ul2[coef.04$var == "as.numeric(DEDPERHS)"]
coefs.y$ul2[coefs.y$var == "District/State Median Fam Inc"] <- coef.04$ul2[coef.04$var == "as.numeric(DINCMEDF)"]
coefs.y$ul2[coefs.y$var == "High-Status Negativity"] <- coef.04$ul2[coef.04$var == "factor(PRESNEGATIVE)1"]

      # Coefficients, 2006
coefs.y$estimate3[coefs.y$var == "Personal Availability (sd)"] <- coef.06$Estimate[coef.06$var == "as.numeric(mca_sd)"]
coefs.y$estimate3[coefs.y$var == "Female"] <- coef.06$Estimate[coef.06$var == "factor(FEMALE)(1) Female"]
coefs.y$estimate3[coefs.y$var == "Availability*Female"] <- coef.06$Estimate[coef.06$var == "as.numeric(mca_sd):factor(FEMALE)(1) Female"]
coefs.y$estimate3[coefs.y$var == "Went Negative"] <- coef.06$Estimate[coef.06$var == "factor(GONEG)(1) negative towards/criticism of the opponent"]
coefs.y$estimate3[coefs.y$var == "Issue Ownership (sd)"] <- coef.06$Estimate[coef.06$var == "as.numeric(SDISSUEOWNER)"]
coefs.y$estimate3[coefs.y$var == "Issue Salience (%)"] <- coef.06$Estimate[coef.06$var == "as.numeric(ISSSAL)"]
coefs.y$estimate3[coefs.y$var == "Ownership*Salience"] <- coef.06$Estimate[coef.06$var == "as.numeric(SDISSUEOWNER):as.numeric(ISSSAL)"]
coefs.y$estimate3[coefs.y$var == "Democrat"] <- coef.06$Estimate[coef.06$var == "factor(DEMOCRAT2)1"]
coefs.y$estimate3[coefs.y$var == "Non-White"] <- coef.06$Estimate[coef.06$var == "factor(NONWHITE)1"]
coefs.y$estimate3[coefs.y$var == "Funds Raised (log)"] <- coef.06$Estimate[coef.06$var == "as.numeric(LOGFRAISED)"]
coefs.y$estimate3[coefs.y$var == "Senate"] <- coef.06$Estimate[coef.06$var == "factor(SENATE)(1) Senate candidate"]
coefs.y$estimate3[coefs.y$var == "Competitiveness"] <- coef.06$Estimate[coef.06$var == "as.numeric(COOKS)"]
coefs.y$estimate3[coefs.y$var == "Challenger"] <- coef.06$Estimate[coef.06$var == "factor(CHALL)(1) challenger"]
coefs.y$estimate3[coefs.y$var == "Open Seat"] <- coef.06$Estimate[coef.06$var == "factor(OPEN)(1) open seat candidate"]
coefs.y$estimate3[coefs.y$var == "Compete*Challenger"] <- coef.06$Estimate[coef.06$var == "as.numeric(COOKS):factor(CHALL)(1) challenger"]
coefs.y$estimate3[coefs.y$var == "Compete*Open"] <- coef.06$Estimate[coef.06$var == "as.numeric(COOKS):factor(OPEN)(1) open seat candidate"]
coefs.y$estimate3[coefs.y$var == "District/State Pop (per 1000)"] <- coef.06$Estimate[coef.06$var == "as.numeric(DPOP2)"]
coefs.y$estimate3[coefs.y$var == "District/State % >= H.S. Diploma"] <- coef.06$Estimate[coef.06$var == "as.numeric(DEDPERHS)"]
coefs.y$estimate3[coefs.y$var == "District/State Median Fam Inc"] <- coef.06$Estimate[coef.06$var == "as.numeric(DINCMEDF)"]
coefs.y$estimate3[coefs.y$var == "High-Status Negativity"] <- coef.06$Estimate[coef.06$var == "factor(PRESNEGATIVE)1"]

      # ll2, 2006
coefs.y$ll3[coefs.y$var == "Personal Availability (sd)"] <- coef.06$ll3[coef.06$var == "as.numeric(mca_sd)"]
coefs.y$ll3[coefs.y$var == "Female"] <- coef.06$ll3[coef.06$var == "factor(FEMALE)(1) Female"]
coefs.y$ll3[coefs.y$var == "Availability*Female"] <- coef.06$ll3[coef.06$var == "as.numeric(mca_sd):factor(FEMALE)(1) Female"]
coefs.y$ll3[coefs.y$var == "Went Negative"] <- coef.06$ll3[coef.06$var == "factor(GONEG)(1) negative towards/criticism of the opponent"]
coefs.y$ll3[coefs.y$var == "Issue Ownership (sd)"] <- coef.06$ll3[coef.06$var == "as.numeric(SDISSUEOWNER)"]
coefs.y$ll3[coefs.y$var == "Issue Salience (%)"] <- coef.06$ll3[coef.06$var == "as.numeric(ISSSAL)"]
coefs.y$ll3[coefs.y$var == "Ownership*Salience"] <- coef.06$ll3[coef.06$var == "as.numeric(SDISSUEOWNER):as.numeric(ISSSAL)"]
coefs.y$ll3[coefs.y$var == "Democrat"] <- coef.06$ll3[coef.06$var == "factor(DEMOCRAT2)1"]
coefs.y$ll3[coefs.y$var == "Non-White"] <- coef.06$ll3[coef.06$var == "factor(NONWHITE)1"]
coefs.y$ll3[coefs.y$var == "Funds Raised (log)"] <- coef.06$ll3[coef.06$var == "as.numeric(LOGFRAISED)"]
coefs.y$ll3[coefs.y$var == "Senate"] <- coef.06$ll3[coef.06$var == "factor(SENATE)(1) Senate candidate"]
coefs.y$ll3[coefs.y$var == "Competitiveness"] <- coef.06$ll3[coef.06$var == "as.numeric(COOKS)"]
coefs.y$ll3[coefs.y$var == "Challenger"] <- coef.06$ll3[coef.06$var == "factor(CHALL)(1) challenger"]
coefs.y$ll3[coefs.y$var == "Open Seat"] <- coef.06$ll3[coef.06$var == "factor(OPEN)(1) open seat candidate"]
coefs.y$ll3[coefs.y$var == "Compete*Challenger"] <- coef.06$ll3[coef.06$var == "as.numeric(COOKS):factor(CHALL)(1) challenger"]
coefs.y$ll3[coefs.y$var == "Compete*Open"] <- coef.06$ll3[coef.06$var == "as.numeric(COOKS):factor(OPEN)(1) open seat candidate"]
coefs.y$ll3[coefs.y$var == "District/State Pop (per 1000)"] <- coef.06$ll3[coef.06$var == "as.numeric(DPOP2)"]
coefs.y$ll3[coefs.y$var == "District/State % >= H.S. Diploma"] <- coef.06$ll3[coef.06$var == "as.numeric(DEDPERHS)"]
coefs.y$ll3[coefs.y$var == "District/State Median Fam Inc"] <- coef.06$ll3[coef.06$var == "as.numeric(DINCMEDF)"]
coefs.y$ll3[coefs.y$var == "High-Status Negativity"] <- coef.06$ll3[coef.06$var == "factor(PRESNEGATIVE)1"]

      # ul2, 2006
coefs.y$ul3[coefs.y$var == "Personal Availability (sd)"] <- coef.06$ul3[coef.06$var == "as.numeric(mca_sd)"]
coefs.y$ul3[coefs.y$var == "Female"] <- coef.06$ul3[coef.06$var == "factor(FEMALE)(1) Female"]
coefs.y$ul3[coefs.y$var == "Availability*Female"] <- coef.06$ul3[coef.06$var == "as.numeric(mca_sd):factor(FEMALE)(1) Female"]
coefs.y$ul3[coefs.y$var == "Went Negative"] <- coef.06$ul3[coef.06$var == "factor(GONEG)(1) negative towards/criticism of the opponent"]
coefs.y$ul3[coefs.y$var == "Issue Ownership (sd)"] <- coef.06$ul3[coef.06$var == "as.numeric(SDISSUEOWNER)"]
coefs.y$ul3[coefs.y$var == "Issue Salience (%)"] <- coef.06$ul3[coef.06$var == "as.numeric(ISSSAL)"]
coefs.y$ul3[coefs.y$var == "Ownership*Salience"] <- coef.06$ul3[coef.06$var == "as.numeric(SDISSUEOWNER):as.numeric(ISSSAL)"]
coefs.y$ul3[coefs.y$var == "Democrat"] <- coef.06$ul3[coef.06$var == "factor(DEMOCRAT2)1"]
coefs.y$ul3[coefs.y$var == "Non-White"] <- coef.06$ul3[coef.06$var == "factor(NONWHITE)1"]
coefs.y$ul3[coefs.y$var == "Funds Raised (log)"] <- coef.06$ul3[coef.06$var == "as.numeric(LOGFRAISED)"]
coefs.y$ul3[coefs.y$var == "Senate"] <- coef.06$ul3[coef.06$var == "factor(SENATE)(1) Senate candidate"]
coefs.y$ul3[coefs.y$var == "Competitiveness"] <- coef.06$ul3[coef.06$var == "as.numeric(COOKS)"]
coefs.y$ul3[coefs.y$var == "Challenger"] <- coef.06$ul3[coef.06$var == "factor(CHALL)(1) challenger"]
coefs.y$ul3[coefs.y$var == "Open Seat"] <- coef.06$ul3[coef.06$var == "factor(OPEN)(1) open seat candidate"]
coefs.y$ul3[coefs.y$var == "Compete*Challenger"] <- coef.06$ul3[coef.06$var == "as.numeric(COOKS):factor(CHALL)(1) challenger"]
coefs.y$ul3[coefs.y$var == "Compete*Open"] <- coef.06$ul3[coef.06$var == "as.numeric(COOKS):factor(OPEN)(1) open seat candidate"]
coefs.y$ul3[coefs.y$var == "District/State Pop (per 1000)"] <- coef.06$ul3[coef.06$var == "as.numeric(DPOP2)"]
coefs.y$ul3[coefs.y$var == "District/State % >= H.S. Diploma"] <- coef.06$ul3[coef.06$var == "as.numeric(DEDPERHS)"]
coefs.y$ul3[coefs.y$var == "District/State Median Fam Inc"] <- coef.06$ul3[coef.06$var == "as.numeric(DINCMEDF)"]
coefs.y$ul3[coefs.y$var == "High-Status Negativity"] <- coef.06$ul3[coef.06$var == "factor(PRESNEGATIVE)1"]

    # Plot it
coefs.y$var <- factor(coefs.y$var, levels = coefs.y$var)

coefplot2 <- ggplot(coefs.y, aes(x = estimate1, y = as.factor(var))) +
  geom_point(aes(color = "2002"), position = position_nudge(y = -.2)) +
  geom_errorbarh(aes(xmin = ll1, xmax = ul1, color = "2002"), height = 0.2, position = position_nudge(y = -.2)) +
  geom_point(aes(x = estimate2, y = as.factor(var), color = "2004"), position = position_nudge(y = .2)) +
  geom_errorbarh(aes(xmin = ll2, xmax = ul2, color = "2004"), height = 0.2, position = position_nudge(y = .2)) +
  geom_point(aes(x = estimate3, y = as.factor(var), color = "2006")) +
  geom_errorbarh(aes(xmin = ll3, xmax = ul3, color = "2006"), height = 0.2) +
  geom_vline(xintercept = 0, color = "black") +
  xlab("Probit Estimates of Negativity Reception") +
  ylab("") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.ticks.y = element_blank()) +
  scale_y_discrete(limits = rev(levels(coefs.y$var)),
                   labels = c("Independent Variables" = expression(bold("Independent Variables")),
                            "Personal Availability (sd)" = "Personal Availability (sd)",
                            "Female" = "Female",
                            " " = "",
                            "Controls" = expression(bold("Controls")),
                            "Went Negative" = "Went Negative",
                            "Issue Ownership (sd)" = "Issue Ownership (sd)",
                            "Issue Salience (%)" = "Issue Salience (%)",
                            "Ownership*Salience" = "Ownership*Salience",
                            "Democrat" = "Democrat",
                            "Non-White" = "Non-White",
                            "Funds Raised (log)" = "Funds Raised (log)",
                            "Senate" = "Senate",
                            "Competitiveness" = "Competitiveness",
                            "Challenger" = "Challenger",
                            "Open Seat" = "Open Seat",
                            "Compete*Challenger" = "Compete*Challenger",
                            "Compete*Open" = "Compete*Open",
                            "District/State Pop (per 1000)" = "District/State Pop (per 1000)",
                            "District/State % >= H.S. Diploma" = "District/State % >= H.S. Diploma",
                            "District/State Median Fam Inc" = "District/State Median Fam Inc",
                            "High-Status Negativity" = "High-Status Negativity")) +
  scale_color_manual(name="", values=c("2002"="#009E73", 
                                       "2004"="#F0E442",
                                       "2006"="#CC79A7"))

    # Predicted probabilities plots for the year subsets
predict02 <- ggplot(y2002.pred, aes(x = x, y = predicted, group = group)) +
  geom_line(aes(color = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .2) +
  ylim(0, 1) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.title = element_text(face="bold"),
        legend.position = "bottom") +
  scale_color_manual(name = "", values = c("(0) Male" = "#E69F00", 
                                       "(1) Female" = "#999999"),
                     labels = c("(0) Male" = "Male", "(1) Female" = "Female")) +
  scale_fill_manual(name="", values=c("(0) Male" = "#E69F00", 
                                      "(1) Female" = "#999999"),
                    labels = c("(0) Male" = "Male", "(1) Female" = "Female"))

predict04 <- ggplot(y2004.pred, aes(x = x, y = predicted, group = group)) +
  geom_line(aes(color = group), alpha = .2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .1) +
  ylim(0, 1) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.title = element_text(face="bold"),
        legend.position = "bottom") +
  scale_color_manual(name = "", values = c("(0) Male" = "#E69F00", 
                                           "(1) Female" = "#999999"),
                     labels = c("(0) Male" = "Male", "(1) Female" = "Female")) +
  scale_fill_manual(name="", values=c("(0) Male" = "#E69F00", 
                                      "(1) Female" = "#999999"),
                    labels = c("(0) Male" = "Male", "(1) Female" = "Female"))

predict06 <- ggplot(y2006.pred, aes(x = x, y = predicted, group = group)) +
  geom_line(aes(color = group), alpha = .2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .1) +
  ylim(0, 1) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.title = element_text(face="bold"),
        legend.position = "bottom") +
  scale_color_manual(name = "", values = c("(0) Male" = "#E69F00", 
                                           "(1) Female" = "#999999"),
                     labels = c("(0) Male" = "Male", "(1) Female" = "Female")) +
  scale_fill_manual(name="", values=c("(0) Male" = "#E69F00", 
                                      "(1) Female" = "#999999"),
                    labels = c("(0) Male" = "Male", "(1) Female" = "Female"))

    # Put all plots together
final.figure <- ggarrange(coefplot2, 
                          annotate_figure(ggarrange(predict02, predict04, predict06, nrow = 3, 
                                                    labels = c("2002","2004","2006"), vjust = 18,
                                                    common.legend = T, legend = "bottom"),
                                          left = text_grob("Predicted Probability of Negativity Reception", 
                                                         face = "bold", rot = 90),
                                          bottom = text_grob("Personal Availability (sd)", face = "bold")), 
                          ncol=2, widths=c(2,1))

pdf("figures/robustness_check.pdf", width = 14, height = 8.5)
final.figure
dev.off()

  # Additive index version of the personal availabiltiy measure
    # Get Cronbach's alpha
campaign.analysis <- campaign.analysis %>%
  mutate_at(vars(vars), funs(cat = as.numeric))

campaign.analysis[, paste0(vars, "_cat")] <- sapply(campaign.analysis[, paste0(vars, "_cat")],
                              function(x) ifelse(x == 2, 1, 0)) 

alpha(campaign.analysis[, paste0(vars, "_cat")]) #0.36 -- Low Cronbach's alpha; more than one dimension 
                                                 #(see eigenvectors for pca below)

  # Create index
campaign.analysis <- campaign.analysis %>%
  mutate(index = select(., paste0(vars, "_cat")) %>%
           rowSums(na.rm = F))

  # Get the models
glm(OPPGONEG ~ index + FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
                 DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
                 DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + relevel(factor(YEAR), ref = "2004"),
               family = binomial(link = "probit"), data = campaign.analysis) %>%
  coeftest(., vcovCL, type = "HC0", cluster =~ CANDNAME) %>% 
  round(3) #results stable

glm(OPPGONEG ~ index*FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
      DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
      DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + relevel(factor(YEAR), ref = "2004"),
    family = binomial(link = "probit"), data = campaign.analysis) %>%
  coeftest(., vcovCL, type = "HC0", cluster =~ CANDNAME) %>% 
  round(3) #results stable

  # PCA version of the personal availability measure
campaign.sub2 <- campaign.analysis[, c(paste0(vars, "_cat"), "names")]
campaign.pca <- PCA(campaign.sub2[, names(campaign.sub2) != "names"], graph = F)
campaign.pca$eig #Dim.1 = 26.83%; Dim.2 = 18.18% var explained
                 #Respective eigenvalues: 1.610 and 1.091

  # Add coordinate for Dim.1 back to the data frame
ind.coord2 <- as.data.frame(campaign.pca$ind$coord[,1])
colnames(ind.coord2) <- "pca_sd"
ind.coord2$pca_sd <- scale(ind.coord2$pca_sd)
ind.coord2$names <- campaign.sub2$names
campaign.analysis <- merge(campaign.analysis, ind.coord2, by = "names") #Note that FactoMineR returns principal coordinates
                                                                        #while Stata returns standard coordinates

  # Get the models
glm(OPPGONEG ~ pca_sd + FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
      DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
      DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + relevel(factor(YEAR), ref = "2004"),
    family = binomial(link = "probit"), data = campaign.analysis) %>%
  coeftest(., vcovCL, type = "HC0", cluster =~ CANDNAME) %>% 
  round(3) #results stable

glm(OPPGONEG ~ pca_sd*FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
      DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
      DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + relevel(factor(YEAR), ref = "2004"),
    family = binomial(link = "probit"), data = campaign.analysis) %>%
  coeftest(., vcovCL, type = "HC0", cluster =~ CANDNAME) %>% 
  round(3) #results stable

  # Logit models
glm(OPPGONEG ~ mca_sd + FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
      DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
      DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + relevel(factor(YEAR), ref = "2004"),
    family = binomial(link = "logit"), data = campaign.analysis) %>%
  coeftest(., vcovCL, type = "HC0", cluster =~ CANDNAME) %>% 
  round(3) #results stable

glm(OPPGONEG ~ mca_sd*FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
      DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
      DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + relevel(factor(YEAR), ref = "2004"),
    family = binomial(link = "logit"), data = campaign.analysis) %>%
  coeftest(., vcovCL, type = "HC0", cluster =~ CANDNAME) %>% 
  round(3) #results stable

  # LPMs
campaign.analysis$OPPGONEG2 <- ifelse(campaign.analysis$OPPGONEG == "(1) opponent went negative",
                                      1, 0)

lm.model1 <- lm(OPPGONEG2 ~ mca_sd + FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
      DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
      DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + relevel(factor(YEAR), ref = "2004"),
    data = campaign.analysis)

state.cl.1 <- vcovCL(lm.model1, type = "HC1", cluster =~ CANDNAME)

coeftest(lm.model1, state.cl.1) %>%
  round(3) #results stable

lm.model2 <- lm(OPPGONEG2 ~ mca_sd*FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
                  DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
                  DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + relevel(factor(YEAR), ref = "2004"),
                data = campaign.analysis)

state.cl.2 <- vcovCL(lm.model2, type = "HC1", cluster =~ CANDNAME)

coeftest(lm.model2, state.cl.2) %>%
  round(3) #results stable

  # Should we include constituency effects?
    # See the Stata code. Convergence issues here.
# glm(OPPGONEG ~ mca_sd*FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
#          DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
#          DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + 
#          relevel(factor(YEAR), ref = "2004") + factor(HOUSE),
#        family = binomial(link = "probit"), data = campaign.analysis,
#     maxit = 5000) %>%
#   coeftest(., vcovCL, type = 'HC0', cluster =~ CANDNAME) %>%
#   round(3) #does not converge
# 
# pacman::p_load(sampleSelection, install = T)
# probit(OPPGONEG ~ mca_sd*FEMALE + GONEG + SDISSUEOWNER*ISSSAL +
#          DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN +
#          DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE +
#          relevel(factor(YEAR), ref = "2004") + factor(HOUSE),
#        data = campaign.analysis) %>%
#   coeftest(., vcovCL, type = 'HC0', cluster =~ CANDNAME) %>%
#   round(3) #converges, bust std. errors are all infinity

  # Unconditional variance decomposition model
unc.model <- glmer(OPPGONEG ~ 1 + (1|CANDNAME), family = binomial(link = "probit"),
      data = campaign.analysis, nAGQ = 100)

icc(unc.model) #not much variance at the candidate level -- ~10%

  # Need a MMM?
    # RE
re.model.2 <- glmer(OPPGONEG ~ mca_sd*FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
                      DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
                      DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + relevel(factor(YEAR), ref = "2004") + 
                      (1|CANDNAME), family = binomial(link = "probit"),
                   data = campaign.analysis, nAGQ = 100)

summary(model.2)

lrtest(model.2, re.model.2) #no sig difference -- two levels fit no better

    # BE
campaign.analysis.be <- campaign.analysis %>%
  select(OPPGONEG,mca_sd,FEMALE,GONEG,SDISSUEOWNER,ISSSAL,DEMOCRAT2,NONWHITE,
         LOGFRAISED,SENATE,COOKS,CHALL,OPEN,DPOP2,DEDPERHS,DINCMEDF,PRESNEGATIVE,
         CANDNAME) %>%
  mutate_at(c("OPPGONEG","FEMALE","GONEG","DEMOCRAT2","NONWHITE","SENATE",
              "CHALL","OPEN"), as.numeric) %>%
  group_by(CANDNAME) %>%
  summarize_at(vars(OPPGONEG:PRESNEGATIVE), mean)

year.be <- campaign.analysis %>%
  tabyl(CANDNAME, YEAR) %>%
  adorn_percentages()

campaign.analysis.be <- merge(campaign.analysis.be, year.be, by = "CANDNAME")

campaign.analysis.be[,c("OPPGONEG","FEMALE","GONEG","SENATE","CHALL","OPEN")] <- 
  campaign.analysis.be[,c("OPPGONEG","FEMALE","GONEG","SENATE","CHALL","OPEN")] - 1
  
lm(OPPGONEG ~ mca_sd*FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
     DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
     DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + `2002` + `2006`,
   data = campaign.analysis.be) %>%
  coeftest(.) %>%
  round(3)


### Supplementary analysis: individual indicators for the main IV
glm(OPPGONEG ~ BIOFAMIL2*FEMALE + BIOTOWN*FEMALE + BIOOCCUP*FEMALE + BIOORG*FEMALE +
      BIOVOLUN*FEMALE + BIOVET*FEMALE + GONEG + SDISSUEOWNER*ISSSAL + 
      DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
      DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE + relevel(factor(YEAR), ref = "2004"), 
    family = binomial(link = "probit"), data = campaign.analysis) %>%
  coeftest(., vcovCL, type = "HC0", cluster =~ CANDNAME) %>% 
  round(3)


### Version of the model with DV limited only to those candidates who 
  ### received *personal* negativity explicitly
load("data/campaign.rda")
temp <- da34895.0001
rm(da34895.0001)

temp$names <- rownames(temp)

campaign.analysis <- merge(campaign.analysis, temp[,c("NEGPERSON","names")], by = "names")

campaign.analysis$pair <- paste(trimws(campaign.analysis$STATE), 
                                trimws(campaign.analysis$DISTRICT), 
                                trimws(campaign.analysis$YEAR), sep = "")

temp <- campaign.analysis %>%
  group_by(pair) %>%
  mutate_at(vars(NEGPERSON), funs(NEGPERSON_cat = as.numeric))
temp$NEGPERSON_cat <- temp$NEGPERSON_cat - 1 

temp <- temp %>%
  group_by(pair) %>%
  mutate(n = sum(NEGPERSON_cat)) %>%
  select(names, n)

campaign.analysis <- merge(campaign.analysis, temp[,c("names","n")], by = "names")

campaign.analysis$OPPNEGPERSON <- ifelse(((campaign.analysis$n == 1 & campaign.analysis$NEGPERSON == 
          "(1) personal negativity towards the opponent") | 
         campaign.analysis$n == 2), (campaign.analysis$n - 1), campaign.analysis$n)

campaign.analysis$n <- NULL

for (i in c("2004","2006")) {
  glm(OPPNEGPERSON ~ mca_sd*FEMALE + NEGPERSON + SDISSUEOWNER*ISSSAL + 
        DEMOCRAT2 + NONWHITE + LOGFRAISED + SENATE + COOKS*CHALL + COOKS*OPEN + 
        DPOP2 + DEDPERHS + DINCMEDF + PRESNEGATIVE, 
      family = binomial(link = "probit"), data = campaign.analysis[campaign.analysis$YEAR == i,]) %>%
    coeftest(., vcovCL, type = "HC0", cluster =~ CANDNAME) %>% 
    round(3) %>%
    print()
}


### END ###

