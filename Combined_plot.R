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
  filter(Time <= 22)  # ⬅️ Only use time points ≤ 22

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

# NOW: Instead of summarizing, just visualize all replicates -------------------
# This keeps all points from all trials visible on the plot

ggplot(corrected_data, aes(x = Time, y = OD_corrected, color = Treatment)) +
  # Show individual data points clearly with shape + jitter + outline
  geom_point(
    position = position_jitter(width = 0.2, height = 0), 
    size = 1.25, 
    alpha = 0.5,
    stroke = 0.7,
    fill = "white"
  ) +
  
  # Mean trend line
  stat_summary(
    fun = mean, 
    geom = "line", 
    aes(group = Treatment), 
    linewidth = 1
  ) +
  
  # Error bars (SD)
  stat_summary(
    fun.data = mean_sdl, 
    geom = "errorbar", 
    width = 0.2, 
    fun.args = list(mult = 1),
    linewidth = 0.4
  ) +
  
  # Aesthetic settings
  labs(
    x = "Time (hours)",
    y = "Corrected OD600",
    title = "Growth Curves with Trials as Replicates",
    color = "Isolate"
  ) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  theme_bw() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )+
  geom_vline(xintercept = 20, linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.8)


