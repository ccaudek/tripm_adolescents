# Script name: 001_read_data.R
# Project: TRPM longitudinal study with adolescents
# Script purpose: import data
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Oct 11 10:08:56 2023
# Last Modified Date: Wed Oct 11 10:08:56 2023
# 
# Notes: 

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("rio")
  library("mice")
  library("careless")
  library("reshape2")
  library("tidyr")
  library("psych")
  # library("plyr")
  library("lavaan")
  library("semTools")
  library("JWileymisc")
  library("multilevelTools")
  # library("lm.beta")
  # library("apaTables")
})

options(max.print = 10000)
options(scipen=999)


# Import data ------------------------------------------------------------------

d <- rio::import(
  here::here("data", "raw", "longitudinal_october_23.csv")
)

# we removed about the 7% of the data using TAPIR as criterion
df_clean <- d |> 
  dplyr::filter(
    TAPIRBrief_T0 <= 7 | TAPIRBrief_T1 <= 7 | TAPIRBrief_T2 <= 7
  )

scales <- c(
  "Meanness_T0",
  "Disinhibition_T0", "Boldness_T0", "STAB_Physical_T0",
  "STAB_Social_T0", "STAB_Rulebreak_T0", "GRANDIOSITY_T0",
  "VULNERABILITY_T0", "Punishiment_T0", "Reward_T0",
  "SDQ_prob_comp_T0", "SDQ_sintomi_emot_T0", "SDQ_adhd_T0",
  "SDQ_prob_pari_T0", "SDQ_comp_prosoc_T0", "tot_difficoltà_T0",
  "ICU_T0", "Meanness_T1", "Disinhibition_T1",
  "Boldness_T1", "SDQ_prob_comp_T1", "SDQ_sintomi_emot_T1",
  "SDQ_adhd_T1", "SDQ_prob_pari_T1", "SDQ_comp_prosoc_T1",
  "tot_difficoltà_T1", "STAB_Physical_T1", "STAB_Social_T1",
  "STAB_Rulebreak_T1", "GRANDIOSITY_T1", "VULNERABILITY_T1",
  "Punishiment_T1", "Reward_T1", "ICU_T1",
  "Meanness_T2", "Disinhibition_T2", "Boldness_T2",
  "SDQ_prob_comp_T2", "SDQ_sintomi_emot_T2", "SDQ_adhd_T2",
  "SDQ_prob_pari_T2", "SDQ_comp_prosoc_T2", "tot_difficoltà_T2",
  "STAB_Physical_T2", "STAB_Social_T2", "STAB_Rulebreak_T2",
  "GRANDIOSITY_T2", "VULNERABILITY_T2", "Punishiment_T2",
  "Reward_T2", "ICU_T2"
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
    cols = -c(id, SEX_STUDENT), 
    names_to = c(".value", "Time"), 
    names_pattern = "(.+)_(T\\d+)$"
  )

glimpse(long_df)

# number and proportion of missing values per variable
cbind("# NA" = sort(colSums(is.na(long_df))),
      "% NA" = round(sort(colMeans(is.na(long_df))) * 100, 2)) |> 
  as.data.frame()


# Imputation -------------------------------------------------------------------

# imp <- mice(long_df, maxit = 5)
# dfcomp <- mice::complete(imp, 1)
imputed_cart = complete(mice(long_df, method = "cart"))
df_comp <- imputed_cart

df_comp$time <- 
  ifelse(df_comp$Time == "T0", 0, ifelse(df_comp$Time == "T1", 1, 2))


# Latent Growth Curve ----------------------------------------------------------

df_comp |> 
  group_by(time) |> 
  summarize(
    m = mean(Meanness, trim = 0.1),
    b = mean(Boldness, trim = 0.1),
    d = mean(Disinhibition, trim = 0.1),
    difficulty = mean(tot_difficoltà, trim = 0.1)
  )


df_comp |> 
  ggplot(aes(x = time, y = Boldness, group = id)) + 
  geom_point(size = .5) + 
  geom_line(alpha=0.2)  

df_comp |> 
  ggplot(aes(x = time, y = Meanness, group = id)) + 
  geom_point(size = .5) + 
  geom_line(alpha=0.2)  

df_comp |> 
  ggplot(aes(x = time, y = Disinhibition, group = id)) + 
  geom_point(size = .5) + 
  geom_line(alpha=0.2)  

df_comp |> 
  ggplot(aes(x = time, y = tot_difficoltà, group = id)) + 
  geom_point(size = .5) + 
  geom_line(alpha=0.2)  

iccMixed(
  dv = "Boldness",
  id = c("id"),
  data = df_comp
) |>
  print()


plot(density(df_comp$Boldness))
df_comp$Time <- factor(df_comp$Time)

df_comp$sex <- factor(df_comp$SEX_STUDENT)

m1 <- brm(
  Reward ~ sex * time * (Meanness + Boldness + Disinhibition) +
    (1 + time | id),
  algorithm = "meanfield",
  family = asym_laplace(),
  data = df_comp
)
pp_check(m1)
bayes_R2(m1)
summary(m1)
marginal_effects(m1, "time:Boldness")

conditions <- make_conditions(m1, "sex")
conditional_effects(m1, "time:Meanness", conditions = conditions)


m2 <- brm(
  Reward ~ time + (1 + time | id),
  algorithm = "meanfield",
  family = asym_laplace(),
  data = df_comp
)
pp_check(m2)
bayes_R2(m2)
summary(m2)
marginal_effects(m2, "time")

# eof ----


cor(df_comp[, 3:19], use="pairwise.complete.obs") |> 
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
