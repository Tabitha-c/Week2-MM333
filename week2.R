# change working directory
setwd("~/Desktop/Data Analytics & Digital Business/Year 3/MM333/Workshops")

#load libraries
library(tidyverse)
library(readr)
library(broom)
library(effectsize)
library(rstatix)

#import a csv file
campaigns <- read_csv("MM333_week2_campaigns.csv", show_col_types = FALSE) |>
  mutate(channel = factor(channel, levels = c("Email", "Social", "In-store")))

productivity <- read_csv("MM333_week2_productivity_paired.csv", show_col_types = FALSE)
# view campaigns and channels
glimpse(campaigns)
summary(campaigns)
count(campaigns, channel)
# summarise any missing values and duplicated ids
campaigns |>
  summarise(
    n = n(),
    missing_values = sum(is.na(pick(everything()))),
    duplicated_ids = n() - n_distinct(observation_id)
  )

glimpse(productivity)
summary(productivity)

# filter the two groups into email and social
two_channels <- campaigns |>
  filter(channel %in% c("Email", "Social")) |>
  droplevels()
# summarise the mean, sandard deviation and median of sales revenue,
two_channels |>
  group_by(channel) |>
  summarise(
    n = n(),
    mean_sales = mean(sales_revenue_gbp),
    sd_sales = sd(sales_revenue_gbp),
    median_sales = median(sales_revenue_gbp),
    .groups = "drop"
)

#Plot the box plot
ggplot(two_channels, aes(x = channel, y = sales_revenue_gbp, fill = channel)) +
  geom_boxplot(alpha = 0.6, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.35, show.legend = FALSE) +
  labs(x = "Marketing channel", y = "Sales revenue (£)") +
  theme_minimal()

# plot and complete the Shapiro- Wilk normality test for both channels
ggplot(two_channels, aes(sample = sales_revenue_gbp)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ channel) +
  theme_minimal()

by(two_channels$sales_revenue_gbp, two_channels$channel, shapiro.test)

# Parametric result - welch t-test independent
independent_t <- t.test(
  sales_revenue_gbp ~ channel,
  data = two_channels,
  var.equal = FALSE,
  alternative = "two.sided"
)

independent_t
broom::tidy(independent_t)

#calculate an effect size - cohens d

effectsize::cohens_d(
  sales_revenue_gbp ~ channel,
  data = two_channels,
  pooled_sd = FALSE
)

#non-parametric check - wilcoxon test
wilcox.test(
  sales_revenue_gbp ~ channel,
  data = two_channels,
  exact = FALSE,
  alternative = "two.sided"
)


# Paired Productivity
# inspect the paired data
productivity |>
  mutate(change = productivity_after - productivity_before) |>
  summarise(
    n = n(),
    mean_before = mean(productivity_before),
    mean_after = mean(productivity_after),
    mean_change = mean(change),
    sd_change = sd(change),
    median_change = median(change)
  )

productivity |>
  pivot_longer(
    cols = c(productivity_before, productivity_after),
    names_to = "time",
    values_to = "productivity"
  ) |>
# line plot
  ggplot(aes(x = time, y = productivity, group = employee_id)) +
  geom_line(alpha = 0.25) +
  geom_point(alpha = 0.5) +
  theme_minimal()

#paired t-tes
paired_t <- t.test(
  productivity$productivity_before,
  productivity$productivity_after,
  paired = TRUE,
  alternative = "two.sided"
)

paired_t
broom::tidy(paired_t)

#check the normality of the diferences scores
differences <- productivity |>
  mutate(change = productivity_after - productivity_before)

ggplot(differences, aes(sample = change)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal()

shapiro.test(differences$change)

# Wilcoxon signed rank
wilcox.test(
  productivity$productivity_before,
  productivity$productivity_after,
  paired = TRUE,
  exact = FALSE,
  alternative = "two.sided"
)

#one way ANOVA and descriptive plots
campaigns |>
  group_by(channel) |>
  summarise(
    n = n(),
    mean_conversion = mean(conversion_rate),
    sd_conversion = sd(conversion_rate),
    median_conversion = median(conversion_rate),
    .groups = "drop"
  )

ggplot(campaigns, aes(x = channel, y = conversion_rate, fill = channel)) +
  geom_boxplot(alpha = 0.65, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.35, show.legend = FALSE) +
  labs(x = "Marketing channel", y = "Conversion rate (%)") +
  theme_minimal()

# assumptions on independence, outliers, normality and homogeneity of variance

ggplot(campaigns, aes(sample = conversion_rate)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ channel) +
  theme_minimal()

campaigns |>
  group_by(channel) |>
  summarise(
    variance = var(conversion_rate),
    .groups = "drop"
  )

# run ANOVA
anova_model <- aov(conversion_rate ~ channel, data = campaigns)
summary(anova_model)

# effect size
effectsize::eta_squared(anova_model)

# post- hoc - tukey hsd
tukey_results <- TukeyHSD(anova_model)
tukey_results
plot(tukey_results)

# Kruskal-Wallis summaries
campaigns |>
  group_by(channel) |>
  summarise(
    n = n(),
    median_satisfaction = median(customer_satisfaction),
    IQR_satisfaction = IQR(customer_satisfaction),
    .groups = "drop"
  )

ggplot(campaigns, aes(x = channel, y = customer_satisfaction, fill = channel)) +
  geom_boxplot(alpha = 0.65, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.25, show.legend = FALSE) +
  scale_y_continuous(breaks = 1:5) +
  labs(x = "Marketing channel", y = "Customer satisfaction rating") +
  theme_minimal()

#run the test
kruskal_result <- kruskal.test(
  customer_satisfaction ~ channel,
  data = campaigns
)
kruskal_result

#Pair-wise follow up
pairwise_wilcox <- campaigns |>
  pairwise_wilcox_test(
    customer_satisfaction ~ channel,
    p.adjust.method = "holm",
    exact = FALSE
  )

pairwise_wilcox

#pearson correlation

ggplot(campaigns, aes(x = advertising_spend_gbp,
                      y = sales_revenue_gbp)) +
  geom_point(alpha = 0.65) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Advertising spend (£)", y = "Sales revenue (£)") +
  theme_minimal()

pearson_result <- cor.test(
  campaigns$advertising_spend_gbp,
  campaigns$sales_revenue_gbp,
  method = "pearson"
)

pearson_result
broom::tidy(pearson_result)

#spearman sensitivity analysis
spearman_result <- cor.test(
  campaigns$advertising_spend_gbp,
  campaigns$sales_revenue_gbp,
  method = "spearman",
  exact = FALSE
)

spearman_result




