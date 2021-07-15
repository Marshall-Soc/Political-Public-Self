
##############################
##  prep.R: Code for prepping binding data
##  Note: See models.R for the regression code/
##        Also see data access note below.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###


######################################
#  Necessary packages and themes
######################################

#remotes::install_gitlab("culturalcartography/text2map")

pacman::p_load(ggeffects,
               tidyverse,
               ggpubr,
               text2map,
               contrast,
               psych,
               rlist,
               ggpubr,
               googlesheets4,
               text2map,
               install = T)

# Custom aesthetics
text2map.theme::set_theme()


######################################
#  Data
######################################

df <- readRDS("data/data_file_here.rds") # Please contact Terence E. McDonnell (terence.e.mcdonnell@nd.edu)
                                         # for details on how to access these data.
dim(df) # 777 by 1216


######################################
#  Creat condition variables
######################################

df <- df %>% 
  mutate_at(vars(contains("Submit")), as.numeric) %>%
  mutate(Condition = case_when(
    `biden94_st_tim1_Page Submit`>0 ~ "Norm, True", 
    `biden94_st_tim2_Page Submit`>0 ~ "Norm, True", 
    `biden94_sf_tim1_Page Submit`>0 ~ "Norm, False", 
    `biden94_sf_tim2_Page Submit`>0 ~ "Norm, False", 
    `biden94_at_tim1_Page Submit`>0 ~ "Def, True", 
    `biden94_at_tim2_Page Submit`>0 ~ "Def, True", 
    `biden94_af_tim1_Page Submit`>0 ~ "Def, False", 
    `biden94_af_tim2_Page Submit`>0 ~ "Def, False", 
    `biden94_affirm_tim1_Page Submit`>0 ~ "Affirmed", 
    `biden94_affirm_tim2_Page Submit`>0 ~ "Affirmed", 
    TRUE ~ "Control"
  ))

df <- df %>% 
  mutate(Conditionb = as.factor(case_when(
    Condition=="Norm, True" ~ "Normative", 
    Condition=="Norm, False" ~ "Normative",  
    Condition=="Def, True" ~ "Definitive", 
    Condition=="Def, False" ~ "Definitive",
    Condition=="Affirmed" ~ "Affirmed",
    Condition=="Control" ~ "Control"
  )))

df <- df %>% 
  mutate(Condition = relevel(as.factor(Condition), "Control")) %>%
  mutate(Conditionb = relevel(as.factor(Conditionb), "Control"))


######################################
#  Accuracy and Answer-Changing
######################################

  # extract baseline
base <- df %>%
  select(biden4, biden18, biden53,
         trump24, trump99, trump20) %>% 
  mutate_all(., .funs=tolower)

  # extract retest
rete <- df %>%
  select(biden4.2, biden18.2, biden53.2, 
         trump24.2, trump99.2, trump20.2) %>% 
  mutate_all(., .funs=tolower)

  # convert to TRUE/FALSE
base <- as.data.frame(gsub('[[:digit:]]+', '', colnames(base) ) == t(base)) %>% t()
rete <- as.data.frame(gsub('[0-9.]+', '', colnames(rete) ) == t(rete) ) %>% t()

  # rename
colnames(base) <- paste0("base_", colnames(base))
colnames(rete) <- paste0("retest_", colnames(rete))

  # how many are the same from baseline to retest
df.acc <- tibble( data.frame(ifelse(base == rete, 1, 0) ) ) 
colnames(df.acc) <- gsub("base_", "changed_", colnames(df.acc))
# sum for total the same
df.acc$same_answer <- rowSums(df.acc)
# create revision as inverse of same answers
df.acc <- df.acc %>% 
  mutate(revision = case_when(
    same_answer==0 ~ 6,
    same_answer==1 ~ 5,
    same_answer==2 ~ 4,
    same_answer==3 ~ 3,
    same_answer==4 ~ 2,
    same_answer==5 ~ 1,
    same_answer==6 ~ 0
  ))

df <- cbind(df, df.acc, base, rete)

df <- df %>%
  rowwise() %>%
  mutate(base_accuracy = sum(base_biden4, base_biden18, 
                             base_biden53, base_trump24, 
                             base_trump99, base_trump20, na.rm=TRUE))


######################################
#  Other predictors
######################################

df <- df %>% 
  mutate(recognize.n = case_when(
    recognize=="Up to 25%" ~ 0, 
    recognize=="Up to 50%" ~ 0, 
    recognize=="Up to 75%" ~ 1, 
    recognize=="Up to 100%" ~ 1
  )) %>%
  mutate(poli_interest.n = case_when(
    poli_interest=="Not at all interested" ~ 0, 
    poli_interest=="Not very interested" ~ 0, 
    poli_interest=="Fairly interested" ~ 1, 
    poli_interest=="Very interested" ~ 1, 
  ))


### END ###

