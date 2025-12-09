library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(patchwork)
library(ggsignif)

# Read the CSV
data <- read.csv("./data/Summary_statistics.csv", check.names=FALSE)


FKGrade <- data %>% select(`Abstract Grade Score`, `Full Text Grade Score`)
FKEase <- data %>% select(`Abstract Reading Ease`,`Full Text Reading Ease`)
RAGASFaith <- data %>% select(`Abstract Faithfulness`, `Full Text Faithfulness`)


FKGrade_long <- FKGrade %>%
  pivot_longer(cols = c(`Abstract Grade Score`,`Full Text Grade Score`),
               names_to = "Abstract vs Full Text",
               values_to = "Flesch-Kincaid Grade Score")

FKEase_long <- FKEase %>%
  pivot_longer(cols = c(`Abstract Reading Ease`, `Full Text Reading Ease`),
               names_to = "Abstract vs Full Text",
               values_to = "Flesch Reading Ease Score")

RAGASFaith_long <- RAGASFaith %>%
  pivot_longer(cols = c(`Abstract Faithfulness`, `Full Text Faithfulness`),
               names_to = "Abstract vs Full Text",
               values_to = "RAGAS Faithfulness Score")

global_theme <- theme(
  panel.grid.minor = element_blank(),  # Remove minor gridlines
  legend.position = "none",  # Position on right of the plot
  #legend.background = element_rect(fill = "white", color = "black"),
  axis.title = element_text(size = 12, face = "bold"),
  axis.text = element_text(size = 10),
  axis.title.y = element_blank(),
  #legend.title = element_text(size = 11, face = "bold"),
  #legend.text = element_text(size = 10),
  plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  
)


FKGrade_Plot <- ggplot(FKGrade_long, aes(x = fct_rev(`Abstract vs Full Text`), y = `Flesch-Kincaid Grade Score`, fill = `Abstract vs Full Text`)) +
    geom_violin(position = position_dodge(width = 0.8), alpha = 0.5) +
    geom_boxplot(width=0.2, fill = "white") +
    geom_signif(comparisons = list(c("Abstract Grade Score", "Full Text Grade Score")),
              test = "wilcox.test",
              map_signif_level = FALSE) +
    geom_jitter(position = position_dodge(width = 0.8), alpha = 0.5) +
    coord_flip() +
    theme_bw() +  # Clean white background
    global_theme

FKEase_Plot <- ggplot(FKEase_long, aes(x = fct_rev(`Abstract vs Full Text`), y = `Flesch Reading Ease Score`, fill = `Abstract vs Full Text`)) +
  geom_violin(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_boxplot(width=0.2, fill = "white") +
  geom_signif(comparisons = list(c("Abstract Reading Ease", "Full Text Reading Ease")),
              test = "wilcox.test",
              map_signif_level = FALSE) +
  geom_jitter(position = position_dodge(width = 0.8), alpha = 0.5) +
  theme_bw() +  # Clean white background
  coord_flip() +
  global_theme

RAGASFaith_Plot <- ggplot(RAGASFaith_long, aes(x = fct_rev(`Abstract vs Full Text`), y = `RAGAS Faithfulness Score`, fill = `Abstract vs Full Text`)) +
  geom_violin(position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_boxplot(width=0.2, fill = "white") +
  geom_signif(comparisons = list(c("Abstract Faithfulness", "Full Text Faithfulness")),
              test = "wilcox.test",
              map_signif_level = FALSE) +
  geom_jitter(position = position_dodge(width = 0.8), alpha = 0.5) +
  theme_bw() +  # Clean white background
  coord_flip() +
  global_theme

Combined <- FKGrade_Plot / FKEase_Plot / RAGASFaith_Plot
Combined

ggsave("Figure2.2.png", Combined, width = 8, height = 10, dpi = 600)

