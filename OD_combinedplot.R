# LOAD PACKAGES ----------------------------------------------------------------
library(ggplot2)
library(dplyr)

# READ AND LABEL EACH TRIAL ---------------------------------------------------
data1 <- read.csv("OD_trial1.csv") %>%
  mutate(Trial = "Trial1")

data2 <- read.csv("OD_trial2.csv") %>%
  mutate(Trial = "Trial2")

data3 <- read.csv("OD_trial3.csv") %>%
  mutate(Trial = "Trial3")

# COMBINE DATASETS ------------------------------------------------------------
combined_data <- bind_rows(data1, data2, data3) %>%
  filter(Treatment %in% c("SSE", "P4_SSE", "Ch5_SSE")) %>%
  mutate(
    Time = as.numeric(as.character(Time)),
    Treatment = factor(Treatment),
    Trial = factor(Trial)
  ) %>% 
  filter(Time <= 22)

# BLANK CORRECTION ------------------------------------------------------------
sse_means <- combined_data %>%
  filter(Treatment == "SSE") %>%
  group_by(Time, Trial) %>%
  summarise(sse_mean = mean(OD_600), .groups = "drop")

corrected_data <- combined_data %>%
  left_join(sse_means, by = c("Time", "Trial")) %>%
  mutate(
    OD_corrected = ifelse(Treatment != "SSE", OD_600 - sse_mean, NA)
  ) %>%
  filter(Treatment %in% c("P4_SSE", "Ch5_SSE")) %>%
  group_by(Treatment, Time, Trial) %>%
  mutate(Replicate = row_number())

# SUMMARIZE FOR PLOTTING ------------------------------------------------------
plot_data <- corrected_data %>%
  group_by(Time, Treatment) %>%
  summarise(
    mean_OD = mean(OD_corrected, na.rm = TRUE),
    sd_OD = sd(OD_corrected, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# PLOT ------------------------------------------------------------------------
ggplot(plot_data, aes(x = Time, y = mean_OD, color = Treatment, group = Treatment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean_OD - sd_OD, ymax = mean_OD + sd_OD), width = 0.5, alpha = 0.7) +
  labs(
    x = "Time (hours)",
    y = "Corrected OD600",
    title = "Combined Growth Curves (All Trials)",
    color = "Isolate"
  ) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  theme_bw() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
