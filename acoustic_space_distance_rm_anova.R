# Load required packages
library(tidyverse) # install.packages(tidyverse)
library(rstatix)
library(ggpubr)
library(ggsignif)  # Load ggsignif for statistical significance indicators

# Define variables
distance_variable <- "distance_3D"  # Change this variable name as needed
selected_vowels <- c("bap", "beep", "boop", "bop", "bape", "bup", "bope")

# Read the data
data_path <- "C:/Users/Kristin Teplansky/Box/SDTL - Lab Volunteers/Emily Rangel/data-analysis/statistics/all-data.csv"
# data_path <- "C:/Users/Kristin Teplansky/Box/SDTL - Lab Volunteers/Emily Rangel/data-analysis/statistics/all-data.csv"
long_data <- read.csv(data_path)

# Preprocessing
long_data <- long_data %>%
  mutate(group_speaking_rate = case_when(
    group == "Control" ~ "Healthy",
    group == "ALS" & speaking_rate > 120 ~ "ALS greater than 120",
    group == "ALS" & speaking_rate <= 120 ~ "ALS less than or equal to 120"
  ))

subset_long_data <- long_data %>%
  filter(vowel %in% selected_vowels) %>%
  mutate(subject_id = factor(subject_id),
         vowel = factor(vowel),
         group = factor(group))

# Summary statistics
summary_stats_distance <- subset_long_data %>%
  group_by(vowel) %>%
  summarise(mean_distance = mean(.data[[distance_variable]]),
            sd_distance = sd(.data[[distance_variable]]))

# Outlier identification
outlier_identification <- subset_long_data %>%
  group_by(vowel) %>%
  identify_outliers(!!sym(distance_variable)) %>%
  select(vowel, !!sym(distance_variable), is.outlier, is.extreme)

# Normality testing
normality_test <- subset_long_data %>%
  group_by(vowel) %>%
  shapiro_test(variable = !!sym(distance_variable))

# ANOVA
model <- anova_test(data = subset_long_data, 
                    dv = .data[[distance_variable]], 
                    wid = subject_id, 
                    within = vowel,
                    between = group)

# Perform pairwise comparisons with pairwise t-tests
pairwise_results <- list()

for (vowel_level in levels(subset_long_data$vowel)) {
  subset_data <- subset_long_data[subset_long_data$vowel == vowel_level, ]
  pairwise_result <- pairwise.t.test(subset_data$distance_3D, subset_data$group, p.adjust.method = "bonferroni")
  pairwise_results[[vowel_level]] <- pairwise_result
}

# Display pairwise comparison results
pairwise_results

# Mean and SD calculation
summary_data <- subset_long_data %>%
  group_by(vowel, group) %>%
  summarise(mean_distance = mean(.data[[distance_variable]]),
            sd_distance = sd(.data[[distance_variable]])) %>%
  ungroup()

# Extracting contrasts data from pairwise_results
contrasts_data <- lapply(names(pairwise_results), function(vowel_level) {
  result <- pairwise_results[[vowel_level]]
  data.frame(
    vowel = rep(vowel_level, nrow(result$p.value)),
    significance = ifelse(result$p.value < 0.05, "*", "")
  )
}) %>%
  bind_rows()

# Rename the second column to 'significance'
names(contrasts_data)[2] <- "significance"

# Merge summary_data with contrasts_data to include mean_distance
merged_data <- merge(summary_data, contrasts_data, by = "vowel", all.x = TRUE)

# Plot with significance indicators using geom_text
distance_plot <- ggplot(merged_data, aes(x = vowel, y = mean_distance, fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = mean_distance - sd_distance, ymax = mean_distance + sd_distance),
                position = position_dodge(width = 0.8), width = 0.3) +
  scale_fill_manual(values = c("Control" = "deepskyblue4", "ALS" = "slategray3")) +
  labs(x = "Vowel", y = "Mean Distance", fill = "Group") +
  ggtitle("Mean Distance by Vowel and Group") +
  theme_classic() +
  geom_text(data = merged_data %>% filter(significance == "*"), 
            aes(x = as.numeric(vowel), y = mean_distance + sd_distance + 6, label = significance),
            size = 4, position = position_dodge(width = 0.8)) +
  geom_text(data = merged_data %>% filter(significance == "*"), 
            aes(x = as.numeric(vowel), y = mean_distance + sd_distance + 1.5, label = "*"),
            size = 4, position = position_dodge(width = 0.8))

# Show the plot
distance_plot

