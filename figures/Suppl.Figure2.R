library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(ggsignif)

# Read the CSV
data <- read.csv("./data/Summary_statistics.csv", check.names=FALSE)

AccuracyRatings <- data %>%
  select(`Article #`,
         `Rater 1`,
         `Rater 2`,
         `Rater 3`,
         `Rater 4`,
         `Rater 5`) %>%
filter(`Article #` <= 40) #Only 40 articles with human ratings
  
AccuracyRatings$Type <- ifelse(AccuracyRatings$`Article #`<= 20, "Abstract", "Full Text")

AccuracyRatings

df_long <- AccuracyRatings %>%
  pivot_longer(
    cols = starts_with("Rater"),
    names_to = "Rater",
    values_to = "rating",
    values_drop_na = TRUE  # This removes NA values
  ) %>%
  select(rating, Type)  # Keep only rating and Type columns

df_long

HumanAccuracyPlot <- ggplot(df_long, aes(x = Type, y = rating, fill = Type)) +
  geom_violin(alpha = 0.7, trim = TRUE) +
  # geom_beeswarm(alpha = 0.3, size = 2.5, cex = 1) +
  # geom_boxplot(width=0.2, fill = "white") +
  geom_jitter(height = 0.1, width = 0.05, alpha = 0.3) +
  geom_signif(comparisons = list(c("Abstract", "Full Text")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              y_position = 9.2) +
  labs(title = "Human Accuracy Ratings between Article Types",
       y = "Accuracy Ratings by Human Review",
       x = "Source Article Type") +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
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
HumanAccuracyPlot
#ggsave("Figure5.1.png", HumanAccuracyPlot, width = 6, height = 5, dpi = 300)