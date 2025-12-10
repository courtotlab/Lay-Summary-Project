library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("./data/Grade_Level_Comparison.csv", check.names=FALSE)

df_long <- df %>% 
  pivot_longer(cols = c("Expected","Actual"),
             names_to = "Expected vs Actual",
             values_to = "Flesch-Kincaid Grade Level")

df_final <- df_long %>% 
  group_by(`Expected vs Actual`, `Specified Grade Level`) %>%
    summarise(
      mean_grade = mean(`Flesch-Kincaid Grade Level`),
      sd_grade = sd(`Flesch-Kincaid Grade Level`)
    )

plt <- ggplot(df_final, aes(x = `Specified Grade Level`, y = mean_grade, 
                     color = `Expected vs Actual`, group = `Expected vs Actual`)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(data = df_final %>% filter(`Expected vs Actual` == "Actual"),
                aes(ymin = mean_grade - sd_grade, ymax = mean_grade + sd_grade), 
                width = 0.2, size = 0.8) +
  labs(
    x = "Specified Grade Level",
    y = "Flesch-Kincaid Grade Level",
    color = "Grade Level"
  ) +
  scale_x_continuous(breaks = 2:8) +
  scale_y_continuous(limits = c(0,15),breaks = 0:15) +
  theme_bw() +  # Clean white background
  theme(
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    legend.position = "right",  # Position on right of the plot
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

plt <- plt + scale_color_manual(
  labels = c("Actual (GPT-4 Summary)", "Expected (Specified in Prompt)"),
  values = c("#F8766D","#00BFC4")
)


ggsave("SuppFig1.png", plt, width = 8, height = 6, dpi = 300)