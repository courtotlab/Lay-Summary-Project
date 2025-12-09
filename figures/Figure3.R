library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(patchwork)

# Read the CSV
data <- read.csv("./data/Summary_statistics.csv", check.names=FALSE)

Abstracts <- data %>%
  select(`Article #`, 
         `Abstract ChatGPT`, 
         `Abstract Grade Score`, 
         `Grader 1`,
         `Grader 2`,
         `Grader 3`,
         `Grader 4`,
         `Grader 5`) %>%  # select columns
  filter(`Article #` <= 20)  # keep only rows where Article # 1-20 (Abstracts)

FullText <- data %>%
  select(`Article #`, 
         `Full Text ChatGPT`, 
         `Full Text Grade Score`, 
         `Grader 1`,
         `Grader 2`,
         `Grader 3`,
         `Grader 4`,
         `Grader 5`) %>%  # select columns
  filter(`Article #` >=21 & `Article #` <= 40)  # keep only rows where Article # 1-20 (Abstracts)

# Reshape grader columns to long format (drops NAs automatically)
Abstracts_long <- Abstracts %>%
  select(`Article #`, `Grader 1`, `Grader 2`, `Grader 3`, `Grader 4`, `Grader 5`) %>%
  pivot_longer(cols = starts_with("Grader"),
               names_to = "Rater",
               values_to = "Score",
               values_drop_na = TRUE)  # Remove empty cells for NA

# Reshape grader columns to long format (drops NAs automatically)
FullText_long <- FullText %>%
  select(`Article #`, `Grader 1`, `Grader 2`, `Grader 3`, `Grader 4`, `Grader 5`) %>%
  pivot_longer(cols = starts_with("Grader"),
               names_to = "Rater",
               values_to = "Score",
               values_drop_na = TRUE)  # This removes empty cells!

# Plot all individual rater points
AbstractPlot <- ggplot(Abstracts, aes(x = `Article #`)) +
  geom_boxplot(data = Abstracts_long, aes(y = Score, group = `Article #`, color = "Human Grade"),
               alpha = 0.5, width = 0.3) +
  # All individual human rater points
  geom_point(data = Abstracts_long, aes(y = Score, color = "Human Grade"), 
             size = 2, shape = 16, alpha = 0.6) +
  # ChatGPT scores
  geom_point(aes(y = `Abstract ChatGPT`, color = "GPT Grade"), 
             size = 3, shape = 17) +
  # FK Grade scores
  geom_point(aes(y = `Abstract Grade Score`, color = "Flesch-Kincaid Grade"), 
             size = 3, shape = 15) +
  scale_x_continuous(breaks = seq(1, 20, by = 1)) +
  scale_y_continuous(breaks = 1:12, limits = c(1, 12)) +
  labs(x = "Article #", 
       y = "Grade Score",
       color = "Grade Score Type",
       title = "Grade Level Scoring - Abstracts") +
  theme_bw() +  # Clean white background
  theme(
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    legend.position = "none",  # Position on right of the plot
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    
  )

# Plot all individual rater points
FullTextPlot <- ggplot(FullText, aes(x = `Article #`)) +
  geom_boxplot(data = FullText_long, aes(y = Score, group = `Article #`, color = "Human Grade"),
              alpha = 0.5, width = 0.3) +
  # All individual human rater points
  geom_point(data = FullText_long, aes(y = Score, color = "Human Grade"), 
             size = 2, shape = 16, alpha = 0.6) +
  # ChatGPT scores
  geom_point(aes(y = `Full Text ChatGPT`, color = "GPT4 Grade"), 
             size = 3, shape = 17) +
  # Full Text Grade scores
  geom_point(aes(y = `Full Text Grade Score`, color = "Flesch-Kincaid Grade"), 
             size = 3, shape = 15) +
  scale_x_continuous(breaks = seq(21, 40, by = 1)) +
  scale_y_continuous(breaks = 1:12, limits = c(1, 12)) +
  labs(x = "Article #", 
       y = "Grade Score",
       color = "Grade Score Type",
       title = "Grade Level Scoring - Full Text") +
  theme_bw() +  # Clean white background
  theme(
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    legend.position = "right",  # Position on right of the plot
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    
  )


CombinedPlot <- AbstractPlot + FullTextPlot
CombinedPlot
# ggsave("Figure3Abstract.png", AbstractPlot, width = 9, height = 6, dpi = 300)
# ggsave("Figure3FullText.png", FullTextPlot, width = 9, height = 6, dpi = 300)
#ggsave("Figure3.png", CombinedPlot, width = 12, height = 6, dpi = 300)
