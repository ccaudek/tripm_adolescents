# Script name: 001_read_data.R
# Project: TRPM longitudinal study with adolescents
# Script purpose: import data
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Oct 11 10:08:56 2023
# Last Modified Date: Wed Oct 11 10:08:56 2023
# 
# Notes: 


library(tidyverse)
library(rio)
library(here)
library(mice)
library(careless)
library(reshape2)


d <- rio::import(
  here::here("data", "raw", "longitudinal_october_23.csv")
)

# we removed about the 7% of the data using TAPIR as criterion
df_clean <- d |> 
  dplyr::filter(
    TAPIRBrief_T0 <= 7 | TAPIRBrief_T1 <= 7 | TAPIRBrief_T2 <= 7
  )

scales <- c(
  "Meanness_T0"                    
  , "Disinhibition_T0"                , "Boldness_T0"                     , "STAB_Physical_T0"               
  , "STAB_Social_T0"                  , "STAB_Rulebreak_T0"               , "GRANDIOSITY_T0"                 
  , "VULNERABILITY_T0"                , "Punishiment_T0"                  , "Reward_T0"                      
  , "SDQ_prob_comp_T0"                , "SDQ_sintomi_emot_T0"             , "SDQ_adhd_T0"                    
  , "SDQ_prob_pari_T0"                , "SDQ_comp_prosoc_T0"              , "tot_difficoltà_T0"              
  , "ICU_T0"                          , "Meanness_T1"                     , "Disinhibition_T1"               
  , "Boldness_T1"                     , "SDQ_prob_comp_T1"                , "SDQ_sintomi_emot_T1"            
  , "SDQ_adhd_T1"                     , "SDQ_prob_pari_T1"                , "SDQ_comp_prosoc_T1"             
  , "tot_difficoltà_T1"               , "STAB_Physical_T1"                , "STAB_Social_T1"                 
  , "STAB_Rulebreak_T1"               , "GRANDIOSITY_T1"                  , "VULNERABILITY_T1"               
  , "Punishiment_T1"                  , "Reward_T1"                       , "ICU_T1"                         
  , "Meanness_T2"                     , "Disinhibition_T2"                , "Boldness_T2"                    
  , "SDQ_prob_comp_T2"                , "SDQ_sintomi_emot_T2"             , "SDQ_adhd_T2"                    
  , "SDQ_prob_pari_T2"                , "SDQ_comp_prosoc_T2"              , "tot_difficoltà_T2"              
  , "STAB_Physical_T2"                , "STAB_Social_T2"                  , "STAB_Rulebreak_T2"              
  , "GRANDIOSITY_T2"                  , "VULNERABILITY_T2"                , "Punishiment_T2"                 
  , "Reward_T2"                       , "ICU_T2"
)

# Select the specified columns from df_clean
selected_df <- df_clean %>%
  select(all_of(scales))

# Assuming each row in selected_df represents a unique observation,
# Create a unique identifier for each observation
selected_df <- selected_df %>%
  mutate(id = row_number())

long_df <- selected_df %>%
  pivot_longer(
    cols = -id, 
    names_to = c(".value", "Time"), 
    names_pattern = "(.+)_(T\\d+)$"
  )

glimpse(long_df)

# number and proportion of missing values per variable
cbind("# NA" = sort(colSums(is.na(long_df))),
      "% NA" = round(sort(colMeans(is.na(long_df))) * 100, 2)) |> 
  as.data.frame()


imp <- mice(long_df, maxit = 5)
dfcomp <- mice::complete(imp, 1)


cor(df, use='pairwise.complete.obs') |> 
  round(2)


set.seed(123)  # optional, for reproducibility


# Randomly select 10 values from the range 1:864
random_ids <- sample(1:864, size = 12)

# Filter dfcomp to only include rows where id matches one of the random_ids
temp <- dfcomp %>%
  dplyr::filter(id %in% random_ids)

temp %>%
  ggplot(aes(x = Time, y = Meanness)) +
  geom_point() +
  geom_line(aes(group = id)) +  # Add group aesthetic here
  # coord_cartesian(ylim = c(1, 4)) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ id)

temp_long <- temp %>%
  pivot_longer(cols = c(Meanness, Disinhibition, Boldness),
               names_to = "Trait",
               values_to = "Value")

dfcomp %>%
  ggplot(aes(x = Time, y = Value, color = Trait, group = Trait)) +
  geom_point() +
  geom_line() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ id)


# Dishinibition and rule breaking


Disinhibition STAB_Rulebreak


# Assuming your dataframe is named df

ggplot(dfcomp, aes(x = Disinhibition, y = STAB_Rulebreak)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ Time, scales = "free") +
  labs(y = "STAB_Rulebreak", title = "Regression of STAB_Rulebreak on Disinhibition")


t0 <- temp_long |> 
  dplyr::filter(Time == "T0")



plot(t0$Disinhibition, t0$STAB_Rulebreak)





cor(dfcomp) |> 
  round(2)

tripm_df <- d |> 
  dplyr::select(starts_with("TRIPM_BRIEF_"))

imp <- mice(tripm_df, maxit = 5)
dfcomp <- mice::complete(imp, 1)


careless_long <- longstring(dfcomp, avg = FALSE)

mydf <- data.frame(
  id = 1:932,
  long = careless_long
)

mydf[mydf$long > 10, ]




df <- temp |> 
  dplyr::select(
    Disinhibition_T0, Disinhibition_T1, Disinhibition_T2, 
    #STAB_Rulebreak_T0, STAB_Rulebreak_T1, STAB_Rulebreak_T2,
    tot_difficoltà_T0, tot_difficoltà_T1, tot_difficoltà_T2
  )

cor(df, use='pairwise.complete.obs') |> 
  round(2)

plot(
  df$tot_difficoltà_T0, df$Disinhibition_T0
)

imp <- mice(df, maxit = 5)
dfcomp <- mice::complete(imp, 1)

cor(dfcomp) |> 
  round(2)

plot(
  dfcomp$tot_difficoltà_T0, dfcomp$Disinhibition_T0
)
