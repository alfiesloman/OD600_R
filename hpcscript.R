# Load Packages
library(tidyverse)
library(readxl)

# Get arguments passed from the command line
# trailingOnly = TRUE keeps only the user-supplied arguments
args <- commandArgs(trailingOnly = TRUE)

# Assign the first argument to a variable
file_path <- args[1]

# Read the Excel file using the provided path
df <- read_excel(file_path)

print("Data successfully read from the provided Excel file.")

# Function to extract each cycle block from the raw data
extract_cycle <- function(df, start_row, cycle_label){
  block <- df[(start_row +3) : (start_row +10), 1:13]
  colnames(block) <- c("Row", as.character(1:12))
  block [, 2:13] <- lapply(block[, 2:13], \(x) suppressWarnings(as.numeric(x)))
  block_long <- pivot_longer(block,
                             cols = -Row,
                             names_to = "Col",
                             values_to = "OD") %>%
    mutate(
      well = paste0(Row, Col),
      Cycle = cycle_label
    )
  return(block_long)
}

cycle_starts <- which(str_detect(df[[1]], "Cycle"))

# Loop through each cycle block and extract

all_cycles <- lapply(cycle_starts, function(i) {
  cycle_label <- df[[1]][i]
  extract_cycle(df, i, cycle_label)
}) %>%
  bind_rows()

all_cycles <- all_cycles %>%
  mutate(Time_min = as.numeric(str_extract(Cycle, "(?<=\\().*?(?=h)")) * 60)

# Remove rows A and H as they are not needed
filtered_cycles <- all_cycles %>%
  mutate(
    Row = substr(well, 1, 1),
    Col = as.numeric(substr(well, 2, 3))
  ) %>%
  filter(!(Row %in% c("A", "H")))


# Give correct names to each of the well conditions
filtered_cycles <- filtered_cycles %>%
  mutate(
    Row = substr(well, 1, 1),
    Col = as.numeric(substr(well, 2, 3)),
    Condition = case_when(
      # Row B
      Row == "B" ~ "009", # Covers B 1-12

      # Row C
      Row == "C" & Col %in% 1:6  ~ "009+1",
      Row == "C" & Col %in% 7:12 ~ "009+2",

      # Row D
      Row == "D" & Col %in% 1:6  ~ "009+3",
      Row == "D" & Col %in% 7:12 ~ "009+4", # Assuming this instead of C 7-12

      # Row E
      Row == "E" ~ "Lysogen", # Covers E 1-12

      # Row F
      Row == "F" & Col %in% 1:6  ~ "lysogen+1",
      Row == "F" & Col %in% 7:12 ~ "Lysogen+2",

      # Row G
      Row == "G" & Col %in% 1:6  ~ "lysogen+3",
      Row == "G" & Col %in% 7:12 ~ "lysogen+4",

      # everything else (if any)
      TRUE ~ NA_character_
    )
  )

# Seperate the lysogen and 009 data to plot a standard growth curve and examine data
plain_data <- filtered_cycles %>%
  filter(Condition %in% c("Lysogen", "009"))

# Plot the plain data
plot_one<-ggplot(plain_data, aes(x = Time_min, y = OD, color = Condition)) +
  geom_point(alpha = 0.5) +  # raw data
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, size = 1.2) +
  # ^ cubic polynomial fit (adjust degree as needed)
  theme_minimal(base_size = 14) +
  labs(
    title = "Growth curves: Lysogen vs 009",
    x = "Time (minutes)",
    y = "OD (raw)",
    color = "Condition"
  )

ggsave("growth_curve_009_vs_lysogen.pdf", plot = plot_one, width = 8, height = 6, dpi = 300)
