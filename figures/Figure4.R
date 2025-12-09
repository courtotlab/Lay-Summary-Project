library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(ggbeeswarm)
library(patchwork)


Clarity <- read.csv('./data/Lay Summary Evaluation Assignments - Clarity.csv', skip = 1)
GradeScore <- read.csv('./data/Lay Summary Evaluation Assignments - Grade Level.csv', skip = 1)
Accuracy <- read.csv('./data/Lay Summary Evaluation Assignments - Accuracy.csv', skip = 1)

Clarity$Category <- "Clarity"
GradeScore$Category <- "Grade Level"
Accuracy$Category <- "Accuracy"
data_combined <- rbind(Clarity, GradeScore, Accuracy)

data_long <- data_combined %>%
  pivot_longer(cols = c(X1, X2, X3, X4, X5),
               names_to = "Experience_Level",
               values_to = "Score") %>%
  filter(!is.na(Score))  # Remove NA values


# Clean up the Experience_Level labels (remove 'X')
data_long$Experience_Level <- gsub("X", "", data_long$Experience_Level)
data_long
ExperiencePlot<-ggplot(data_long, aes(x = Experience_Level, y = Score, color = Category)) +
  geom_boxplot(data = data_long,aes(group = interaction(Experience_Level, Category), fill = Category),
               alpha = 0.15, width = 0.6, position = position_dodge(width = 0.8)) +
  geom_jitter(aes(color = Category),
              position = position_jitterdodge(jitter.width = 0.5, jitter.height = 0.1, dodge.width = 0.8),
              size = 1.5, alpha = 0.35)  +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), drop = FALSE) +
  # scale_color_manual(values = c("Clarity" = "#00A86B",
  #                               "Grade Level" = "#003366",
  #                               "Accuracy" = "#90EE90")) +
  scale_size_continuous(range = c(2, 10)) +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  theme_minimal() +
  labs(x = "Experience Level", y = "Human Rater Score", size = "Count") +
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

counts <- data.frame(expertise = 1:5, n = c(0, 1, 1, 11, 5))

p2 <- ggplot(counts, aes(x = expertise, y = 1, size = n, label = n)) +
  geom_point(aes(size = n), shape = 21, fill = "lightgrey") +
  geom_text(size = 3, fontface = "bold") +
  labs(x = "Number of Human Reviewers Self-Assessed at the Corresponding Experience Level") +
  scale_size_continuous(range = c(5, 20))+
  scale_x_continuous(limits = c(0.65, 5.35)) +
  theme_void() +
  
  theme(legend.position = "none",
        axis.title.x = element_text(size = 12, face = "italic"))

p2
Combined <- ExperiencePlot / p2 + plot_layout(heights = c(8, 1))
Combined 
#ggsave("Figure4.png", Combined, width = 8, height = 7, dpi = 300)