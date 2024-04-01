### boxplot initiation und LR count

# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(multcompView)


#convert delimiter , to .
data_updated <- lapply(data_initiation, function(x) gsub(",", ".", x))

# Combine columns back into a data frame
data_updated <- data.frame(data_updated)


###Initiation



# Define color scheme
my_colors <- c("yellow", "lightgreen", "orange")


##holt1

# Subset the data to include only the first 48 values 
subset_data_holt1 <- data_updated[49:91, ]

# Analysis of variance (ANOVA) for 'holt1' data
anova_holt1 <- aov(gesamt.initiation ~ factor(KNO3..mM.), data = subset_data_holt1)

# Tukey's HSD test
tukey_holt1 <- TukeyHSD(anova_holt1)

# Compact letter display for 'holt1'
cld_holt1 <- multcompLetters4(anova_holt1, tukey_holt1)

# Table with factors and 3rd quantile for 'holt1'
dt_holt1 <- subset_data_holt1 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarise(w = mean(gesamt.initiation), sd = sd(gesamt.initiation)) %>%
  arrange(desc(w))

# Extracting compact letter display and adding to the table
cld_holt1 <- as.data.frame.list(cld_holt1$'factor(KNO3..mM.)')
dt_holt1$cld_holt1 <- cld_holt1$Letters

# Reordering rows based on 'factor(KNO3..mM.)'
dt_holt1 <- dt_holt1 %>%
  arrange(`factor(KNO3..mM.)`)

# Combine data frames, filling missing columns with NA
combined_data_holt1 <- bind_rows(subset_data_holt1, dt_holt1)

# Calculate ymax using dplyr
max_values_holt1 <- combined_data_holt1 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarize(ymax = max(as.numeric(gesamt.initiation, na.rm = FALSE)))

# Delete the 4th value from max_values (assuming appropriate indexing for 'holt1')
max_values_holt1 <- max_values_holt1[-4, ]

# Remove "factor(KNO3..mM.)" column from max_values
max_values_holt1 <- max_values_holt1 %>%
  select(-`factor(KNO3..mM.)`)



# Create new columns with NA filled with x and max values
combined_data_holt1$x_axis <- NA
combined_data_holt1$x_axis[44:46] <- 1:3

combined_data_holt1$max_values_holt1 <- NA
combined_data_holt1$max_values_holt1[44:46] <- max_values_holt1$ymax





# Move values from rows 49:51 to rows 1:3 (assuming numeric columns)
combined_data_holt1[1:3, 7:12] <- combined_data_holt1[44:46, 7:12]

# Remove rows 49:51
combined_data_holt1 <- combined_data_holt1[-(44:46), ]

# Convert columns to appropriate data types (consider actual data)
combined_data_holt1$gesamt.initiation <- as.integer(combined_data_holt1$gesamt.initiation)
combined_data_holt1$KNO3..mM. <- as.numeric(combined_data_holt1$KNO3..mM.)  # Assuming numeric
combined_data_holt1$x_axis <- as.integer(combined_data_holt1$x_axis)  # Assuming numeric
combined_data_holt1$max_values_holt1 <- as.numeric(combined_data_holt1$max_values_holt1)  # Rename and convert

# Create boxplot (assuming 'cld_holt1' exists for labels)
inu_holt1 <- ggplot(data = combined_data_holt1, aes(x = factor(KNO3..mM.), y = as.numeric(gesamt.initiation))) +
  geom_boxplot(aes(fill = factor(KNO3..mM.)), show.legend = FALSE) +
  geom_text(aes(label = cld_holt1, y = max_values_holt1, x = x_axis, vjust = -0.5)) +
  scale_y_continuous(name = "Initiation", limits = c(0, 15)) +
  scale_x_discrete(name = "Nitrate (mM)") +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  ggtitle("Initiation holt1") + xlab("Nitrate (mM)") + ylab("Initiation")+
  scale_fill_manual(values = my_colors)

inu_holt1



###6909##

# Subset the data to include only the last 43 values (assuming '6909' data exists)
subset_data_6909 <- data_updated[92:126, ]

# Analysis of variance (ANOVA) for '6909' data
anova_6909 <- aov(gesamt.initiation ~ factor(KNO3..mM.), data = subset_data_6909)

# Tukey's HSD test
tukey_6909 <- TukeyHSD(anova_6909)

# Compact letter display for '6909'
cld_6909 <- multcompLetters4(anova_6909, tukey_6909)

# Table with factors and 3rd quantile for '6909'
dt_6909 <- subset_data_6909 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarise(w = mean(gesamt.initiation), sd = sd(gesamt.initiation)) %>%
  arrange(desc(w))

# Extracting compact letter display and adding to the table
cld_6909 <- as.data.frame.list(cld_6909$'factor(KNO3..mM.)')
dt_6909$cld_6909 <- cld_6909$Letters

# Reordering rows based on 'factor(KNO3..mM.)'
dt_6909 <- dt_6909 %>%
  arrange(`factor(KNO3..mM.)`)

# Combine data frames, filling missing columns with NA
combined_data_6909 <- bind_rows(subset_data_6909, dt_6909)

# Calculate ymax using dplyr
max_values_6909 <- combined_data_6909 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarize(ymax = max(as.numeric(gesamt.initiation, na.rm = FALSE)))

# Delete the 4th value from max_values (assuming appropriate indexing for '6909')
max_values_6909 <- max_values_6909[-4, ]

# Remove "factor(KNO3..mM.)" column from max_values
max_values_6909 <- max_values_6909 %>%
  select(-`factor(KNO3..mM.)`)

# Create new columns with NA filled with x and max values
combined_data_6909$x_axis <- NA
combined_data_6909$x_axis[36:38] <- 1:3

combined_data_6909$max_values_6909 <- NA
combined_data_6909$max_values_6909[36:38] <- max_values_6909$ymax



# Move values from rows 44:46 to rows 1:3 (assuming numeric columns)
combined_data_6909[1:3, 7:12] <- combined_data_6909[36:38, 7:12]

# Remove rows 44:46
combined_data_6909 <- combined_data_6909[-(36:38), ]

# Convert columns to appropriate data types (consider actual data)
combined_data_6909$gesamt.initiation <- as.integer(combined_data_6909$gesamt.initiation)
combined_data_6909$KNO3..mM. <- as.numeric(combined_data_6909$KNO3..mM.)  # Assuming numeric
combined_data_6909$x_axis <- as.integer(combined_data_6909$x_axis)  # Assuming numeric
combined_data_6909$max_values_6909 <- as.numeric(combined_data_6909$max_values_6909)  # Rename and convert



# Create boxplot 
inu_6909 <- ggplot(data = combined_data_6909, aes(x = factor(KNO3..mM.), y = as.numeric(gesamt.initiation))) +
  geom_boxplot(aes(fill = factor(KNO3..mM.)), show.legend = FALSE) +
  geom_text(aes(label = cld_6909, y = max_values_6909, x = x_axis, vjust = -0.5)) +
  scale_y_continuous(name = "Initiation", limits = c(0, 15)) +
  scale_x_discrete(name = "Nitrate (mM)") +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  ggtitle("Initiation Col-0") + xlab("Nitrate (mM)") + ylab("Initiation")+
  scale_fill_manual(values = my_colors)

inu_6909



###LR count###




##holt1

# Subset the data to include only the first 48 values (assuming 'holt1' data exists)
subset_data_holt1 <- data_updated[49:91, ]

# Analysis of variance (ANOVA) for 'holt1' data
anova_holt1 <- aov(LR.count ~ factor(KNO3..mM.), data = subset_data_holt1)

# Tukey's HSD test
tukey_holt1 <- TukeyHSD(anova_holt1)

# Compact letter display for 'holt1'
cld_holt1 <- multcompLetters4(anova_holt1, tukey_holt1)

# Table with factors and 3rd quantile for 'holt1'
dt_holt1 <- subset_data_holt1 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarise(w = mean(LR.count), sd = sd(LR.count)) %>%
  arrange(desc(w))

# Extracting compact letter display and adding to the table
cld_holt1 <- as.data.frame.list(cld_holt1$'factor(KNO3..mM.)')
dt_holt1$cld_holt1 <- cld_holt1$Letters

# Reordering rows based on 'factor(KNO3..mM.)'
dt_holt1 <- dt_holt1 %>%
  arrange(`factor(KNO3..mM.)`)

# Combine data frames, filling missing columns with NA
combined_data_holt1 <- bind_rows(subset_data_holt1, dt_holt1)

# Calculate ymax using dplyr
max_values_LR_holt1 <- combined_data_holt1 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarize(ymax = max(as.numeric(LR.count, na.rm = FALSE)))

# Delete the 4th value from max_values (assuming appropriate indexing for 'holt1')
max_values_LR_holt1 <- max_values_LR_holt1[-4, ]

# Remove "factor(KNO3..mM.)" column from max_values
max_values_LR_holt1 <- max_values_LR_holt1 %>%
  select(-`factor(KNO3..mM.)`)



# Create new columns with NA filled with x and max values
combined_data_holt1$x_axis <- NA
combined_data_holt1$x_axis[44:46] <- 1:3

combined_data_holt1$max_values_LR_holt1 <- NA
combined_data_holt1$max_values_LR_holt1[44:46] <- max_values_LR_holt1$ymax





# Move values from rows 49:51 to rows 1:3 (assuming numeric columns)
combined_data_holt1[1:3, 7:12] <- combined_data_holt1[44:46, 7:12]

# Remove rows 49:51
combined_data_holt1 <- combined_data_holt1[-(44:46), ]

# Convert columns to appropriate data types (consider actual data)
combined_data_holt1$LR.count <- as.integer(combined_data_holt1$LR.count)
combined_data_holt1$KNO3..mM. <- as.numeric(combined_data_holt1$KNO3..mM.)  # Assuming numeric
combined_data_holt1$x_axis <- as.integer(combined_data_holt1$x_axis)  # Assuming numeric
combined_data_holt1$max_values_LR_holt1 <- as.numeric(combined_data_holt1$max_values_LR_holt1)  # Rename and convert

# Create boxplot (assuming 'cld_holt1' exists for labels)
inuLR_holt1 <- ggplot(data = combined_data_holt1, aes(x = factor(KNO3..mM.), y = as.numeric(LR.count))) +
  geom_boxplot(aes(fill = factor(KNO3..mM.)), show.legend = FALSE) +
  geom_text(aes(label = cld_holt1, y = max_values_LR_holt1, x = x_axis, vjust = -0.5)) +
  scale_y_continuous(name = "LR count", limits = c(0, 15)) +
  scale_x_discrete(name = "Nitrate (mM)") +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  ggtitle("LR count holt1") + xlab("Nitrate (mM)") + ylab("LR count")+
  scale_fill_manual(values = my_colors)

inuLR_holt1



###6909##

# Subset the data to include only the last 43 values (assuming '6909' data exists)
subset_data_6909 <- data_updated[92:126, ]

# Analysis of variance (ANOVA) for '6909' data
anova_6909 <- aov(LR.count ~ factor(KNO3..mM.), data = subset_data_6909)

# Tukey's HSD test
tukey_6909 <- TukeyHSD(anova_6909)

# Compact letter display for '6909'
cld_6909 <- multcompLetters4(anova_6909, tukey_6909)

# Table with factors and 3rd quantile for '6909'
dt_6909 <- subset_data_6909 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarise(w = mean(LR.count), sd = sd(LR.count)) %>%
  arrange(desc(w))

# Extracting compact letter display and adding to the table
cld_6909 <- as.data.frame.list(cld_6909$'factor(KNO3..mM.)')
dt_6909$cld_6909 <- cld_6909$Letters

# Reordering rows based on 'factor(KNO3..mM.)'
dt_6909 <- dt_6909 %>%
  arrange(`factor(KNO3..mM.)`)

# Combine data frames, filling missing columns with NA
combined_data_6909 <- bind_rows(subset_data_6909, dt_6909)

# Calculate ymax using dplyr
max_values_LR_6909 <- combined_data_6909 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarize(ymax = max(as.numeric(LR.count, na.rm = FALSE)))

# Delete the 4th value from max_values (assuming appropriate indexing for '6909')
max_values_LR_6909 <- max_values_LR_6909[-4, ]

# Remove "factor(KNO3..mM.)" column from max_values
max_values_LR_6909 <- max_values_LR_6909 %>%
  select(-`factor(KNO3..mM.)`)

# Create new columns with NA filled with x and max values
combined_data_6909$x_axis <- NA
combined_data_6909$x_axis[36:38] <- 1:3

combined_data_6909$max_values_LR_6909 <- NA
combined_data_6909$max_values_LR_6909[36:38] <- max_values_LR_6909$ymax



# Move values from rows 44:46 to rows 1:3 (assuming numeric columns)
combined_data_6909[1:3, 7:12] <- combined_data_6909[36:38, 7:12]

# Remove rows 44:46
combined_data_6909 <- combined_data_6909[-(36:38), ]

# Convert columns to appropriate data types (consider actual data)
combined_data_6909$LR.count <- as.integer(combined_data_6909$LR.count)
combined_data_6909$KNO3..mM. <- as.numeric(combined_data_6909$KNO3..mM.)  # Assuming numeric
combined_data_6909$x_axis <- as.integer(combined_data_6909$x_axis)  # Assuming numeric
combined_data_6909$max_values_LR_6909 <- as.numeric(combined_data_6909$max_values_LR_6909)  # Rename and convert

# Create boxplot (assuming 'cld_6909' exists for labels)
inuLR_6909 <- ggplot(data = combined_data_6909, aes(x = factor(KNO3..mM.), y = as.numeric(LR.count))) +
  geom_boxplot(aes(fill = factor(KNO3..mM.)), show.legend = FALSE) +
  geom_text(aes(label = cld_6909, y = max_values_LR_6909, x = x_axis, vjust = -0.5)) +
  scale_y_continuous(name = "LR count", limits = c(0, 15)) +
  scale_x_discrete(name = "Nitrate (mM)") +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  ggtitle("LR count Col-0") + xlab("Nitrate (mM)") + ylab("LR count")+
  scale_fill_manual(values = my_colors)

inuLR_6909



