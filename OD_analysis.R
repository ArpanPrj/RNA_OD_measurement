# LOAD REQUIRED PACKAGES ------------------------------------------------------
library(ggplot2)   # For creating publication-quality graphics
library(dplyr)     # For data manipulation using intuitive syntax
library(nlme)  
library(broom)
# For advanced linear mixed-effects modeling

# DATA IMPORT AND PREPROCESSING -----------------------------------------------
# Read raw OD data from CSV file
# Structure: Each row contains Time (hr), Treatment, and OD_600 measurement
raw_data <- read.csv("OD_trial2.csv") %>%
  # FILTERING STRATEGY:
  # 1. Remove all water controls (not needed for final analysis)
  # 2. Keep SSE (soybean seed extract blank), P4_SSE, and Ch5_SSE
  filter(Treatment %in% c("SSE", "P4_SSE", "Ch5_SSE")) %>%
  
  # DATA TYPE CONVERSIONS:
  # - Convert Time from factor to numeric for modeling
  # - Convert Treatment to factor for proper statistical modeling
  mutate(
    Time = as.numeric(as.character(Time)),  # Handle potential factor issues
    Treatment = factor(Treatment)           # Ensure proper categorical handling
  )

# BLANK CORRECTION PROTOCOL ---------------------------------------------------
# Calculate mean OD of SSE (blank) at each time point
sse_means <- raw_data %>%
  filter(Treatment == "SSE") %>%        # Select only blank measurements
  group_by(Time) %>%                    # Group by time point
  summarise(
    sse_mean = mean(OD_600),            # Calculate mean blank OD
    .groups = "drop"                    # Prevent unexpected grouping retention
  )

# Create corrected dataset for fungal isolates
corrected_data <- raw_data %>%
  # MERGE BLANK MEANS TO MAIN DATA:
  # Match SSE means to corresponding time points via left join
  left_join(sse_means, by = "Time") %>%
  
  # APPLY BLANK CORRECTION:
  # Only correct treatment groups (P4_SSE/Ch5_SSE), not SSE itself
  mutate(
    OD_corrected = ifelse(
      Treatment != "SSE", 
      OD_600 - sse_mean,                # Subtract blank OD from sample OD
      NA                                # Keep SSE values as NA (will be removed)
    )
  ) %>%
  # FINAL DATA FILTERING:
  # 1. Remove SSE controls after using them for correction
  # 2. Ensure only experimental groups remain
  filter(Treatment %in% c("P4_SSE", "Ch5_SSE")) %>%
  
  # REPLICATE IDENTIFICATION:
  # Create Replicate IDs since original data doesn't contain them
  # Assumes 3 replicates per time-treatment combination
  group_by(Treatment, Time) %>%
  mutate(Replicate = row_number())      # Number replicates sequentially (1-3)

# Perform t-tests at each time point
ttest_results <- corrected_data %>%
  group_by(Time) %>%
  do(tidy(t.test(OD_corrected ~ Treatment, data = .))) %>%
  select(Time, estimate1, estimate2, p.value) %>%
  mutate(
    p.adj = p.adjust(p.value, method = "BH"), # Benjamini-Hochberg correction
    significance = case_when(
      p.adj < 0.001 ~ "***",
      p.adj < 0.01 ~ "**",
      p.adj < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

# MODEL OUTPUT INTERPRETATION -------------------------------------------------
# Generate ANOVA table to evaluate significance of terms
model_summary <- summary(final_model)

# Key outputs to check:
# 1. "Time:Treatment" p-value: Significant = growth rates differ between isolates
# 2. Time coefficient: Overall growth rate across both isolates
# 3. Treatment coefficient: Baseline OD difference between isolates

print(model_summary)  # Show full model output in console

# GROWTH CURVE VISUALIZATION --------------------------------------------------
# Create summary statistics for plotting
plot_data <- corrected_data %>%
  group_by(Time, Treatment) %>%
  summarise(
    mean_OD = mean(OD_corrected),   # Central tendency measure
    sd_OD = sd(OD_corrected),       # Measure of variability
    n = n(),                        # Verify 3 replicates per group
    .groups = "drop"
  )

# Construct publication-quality plot
growth_plot <- ggplot(
  plot_data, 
  aes(
    x = Time, 
    y = mean_OD, 
    color = Treatment,              # Color-code by isolate
    group = Treatment               # Ensure separate lines per treatment
  )
) +
  # PLOT ELEMENTS:
  geom_line(linewidth = 1) +        # Main growth curve
  geom_point(size = 2.5) +          # Individual time points
  geom_errorbar(  # Add error bars
    aes(ymin = mean_OD - sd_OD, ymax = mean_OD + sd_OD),
    width = 0.5,
    alpha = 0.7
  ) +
 
  # AESTHETIC CUSTOMIZATIONS:
  labs(
    x = "Time (hours)", 
    y = "Corrected OD600", 
    title = "Growth Kinetics of Fusarium Isolates",
    color = "Isolate"
  ) +
  scale_color_manual(
    values = c("#E69F00", "#56B4E9") # Color-blind friendly palette
  ) +
  theme_bw() +                  # Clean background
  theme(
    legend.position = "top",         # Optimize space usage
    panel.grid.minor = element_blank(), # Remove distracting gridlines
    plot.title = element_text(hjust = 0.5) # Center title
  )

# Display final plot
print(growth_plot)

