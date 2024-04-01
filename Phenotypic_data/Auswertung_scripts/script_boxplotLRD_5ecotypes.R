###LRD plot


##6909##

# Subset the data to include only the first 120 values
subset_data_6909 <- table_5ecotypes[1:120, ]

# Remove rows with missing values in LR.count
subset_data_6909 <- subset_data_6909[!is.na(subset_data_6909$LR.count), ]

#change comma to point in LRD column
subset_data_6909$LRD..1.cm. <- as.numeric(gsub(",", ".", subset_data_6909$LRD..1.cm.))

#set as numeric data type
subset_data_6909$LRD..1.cm. <- as.numeric(subset_data_6909$LRD..1.cm.)




# analysis of variance
anova_LRD_6909 <- aov(LRD..1.cm.~factor(KNO3..mM.), data=subset_data_6909)

# Tukey's test
tukey_LRD_6909 <- TukeyHSD(anova_LRD_6909)

# compact letter display
cld_LRD_6909 <- multcompLetters4(anova_LRD_6909, tukey_LRD_6909)

# table with factors and 3rd quantile
dt_LRD_6909 <- subset_data_6909 %>% group_by(factor(KNO3..mM.)) %>%
  summarise(w=mean(LRD..1.cm.), sd = sd(LRD..1.cm.)) %>%
  arrange(desc(w))

# extracting the compact letter display and adding to the Tk table
cld_LRD_6909 <- as.data.frame.list(cld_LRD_6909$'factor(KNO3..mM.)')
dt_LRD_6909$cld <- cld_LRD_6909$Letters


# Reordering the rows in ascending order based on 'factor(KNO3..mM.)'
dt_LRD_6909 <- dt_LRD_6909 %>%
  arrange(`factor(KNO3..mM.)`)


# Replace 'a' with 'b' and 'b' with 'a' in the 'cld' column
dt_LRD_6909$cld <- ifelse(dt_LRD_6909$cld == "a", "b", "a")


# Combine the data frames by row, filling missing columns with NA
combined_data_LRD_6909 <- bind_rows(subset_data_6909,dt_LRD_6909)




# Calculate ymax using dplyr
max_values_LRD_6909 <- combined_data_LRD_6909 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarize(ymax = max(LRD..1.cm., na.rm = FALSE))

# Delete the 7th value from max_values
max_values_LRD_6909 <- max_values_LRD_6909[-7, ]

# Remove the "factor(KNO3..mM.)" column from max_values
max_values_LRD_6909 <- max_values_LRD_6909 %>% 
  select(-`factor(KNO3..mM.)`)


# Create a new column filled with NA and fill with x values
combined_data_LRD_6909$x_axis <- NA  
combined_data_LRD_6909$x_axis[117:122] <- 1:6 

#create new column filled with NA and fill max values
combined_data_LRD_6909$max_values <- NA 
combined_data_LRD_6909$max_values[117:122] <- max_values_LRD_6909$ymax  # Assign ymax values to rows 117:122


# Move values from rows 117:122 to rows 1:6
combined_data_LRD_6909[1:6, 7:12] <- combined_data_LRD_6909[117:122, 7:12]

# Remove rows 49:51
combined_data_LRD_6909 <- combined_data_LRD_6909[-(117:122), ]



#create boxplot
ulu_LRD_6909<- ggplot( data = combined_data_LRD_6909, aes(x = factor(KNO3..mM.), y = LRD..1.cm.)) + 
  geom_boxplot (fill = "darkgrey", show.legend = FALSE) +
  geom_text(aes(label = cld, y = max_values, x= x_axis, vjust = -0.5)) +
  scale_y_continuous(name = "LRD", limits = c(0, 5)) +
  scale_x_discrete(name = "Nitrate (mM)") +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  ggtitle("LRD Col-0") + xlab("Nitrate (mM)") +ylab("LRD") 

ulu_LRD_6909








##6145##

# Subset the data to include only the first 120 values
subset_data_6145 <- table_5ecotypes[121:240, ]

# Remove rows with missing values in LR.count
subset_data_6145 <- subset_data_6145[!is.na(subset_data_6145$LR.count), ]

#change comma to point in LRD column
subset_data_6145$LRD..1.cm. <- as.numeric(gsub(",", ".", subset_data_6145$LRD..1.cm.))

#set as numeric data type
subset_data_6145$LRD..1.cm. <- as.numeric(subset_data_6145$LRD..1.cm.)




# analysis of variance
anova_LRD_6145 <- aov(LRD..1.cm.~factor(KNO3..mM.), data=subset_data_6145)

# Tukey's test
tukey_LRD_6145 <- TukeyHSD(anova_LRD_6145)

# compact letter display
cld_LRD_6145 <- multcompLetters4(anova_LRD_6145, tukey_LRD_6145)

# table with factors and 3rd quantile
dt_LRD_6145 <- subset_data_6145 %>% group_by(factor(KNO3..mM.)) %>%
  summarise(w=mean(LRD..1.cm.), sd = sd(LRD..1.cm.)) %>%
  arrange(desc(w))

# extracting the compact letter display and adding to the Tk table
cld_LRD_6145 <- as.data.frame.list(cld_LRD_6145$'factor(KNO3..mM.)')
dt_LRD_6145$cld <- cld_LRD_6145$Letters


# Reordering the rows in ascending order based on 'factor(KNO3..mM.)'
dt_LRD_6145 <- dt_LRD_6145[order(as.numeric(levels(dt_LRD_6145$'factor(KNO3..mM.)'))), ]





# Combine the data frames by row, filling missing columns with NA
combined_data_LRD_6145 <- bind_rows(subset_data_6145,dt_LRD_6145)





# Calculate ymax using dplyr
max_values_LRD_6145 <- combined_data_LRD_6145 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarize(ymax = max(LRD..1.cm., na.rm = FALSE))

# Delete the 7th value from max_values
max_values_LRD_6145 <- max_values_LRD_6145[-7, ]

# Remove the "factor(KNO3..mM.)" column from max_values
max_values_LRD_6145 <- max_values_LRD_6145 %>% 
  select(-`factor(KNO3..mM.)`)


# Create a new column filled with NA and fill with x values
combined_data_LRD_6145$x_axis <- NA  
combined_data_LRD_6145$x_axis[120:125] <- 1:6 

#create new column filled with NA and fill max values
combined_data_LRD_6145$max_values <- NA 
combined_data_LRD_6145$max_values[120:125] <- max_values_LRD_6145$ymax  # Assign ymax values to rows 117:122



# Move values from rows 117:122 to rows 1:6
combined_data_LRD_6145[1:6, 7:12] <- combined_data_LRD_6145[120:125, 7:12]

# Remove rows 49:51
combined_data_LRD_6145 <- combined_data_LRD_6145[-(120:125), ]



#create boxplot
ulu_LRD_6145<- ggplot( data = combined_data_LRD_6145, aes(x = factor(KNO3..mM.), y = LRD..1.cm.)) + 
  geom_boxplot (fill = "green", show.legend = FALSE) +
  geom_text(aes(label = cld, y = max_values, x= x_axis, vjust = -0.5)) +
  scale_y_continuous(name = "LRD", limits = c(0, 5)) +
  scale_x_discrete(name = "Nitrate (mM)") +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  ggtitle("LRD 6145") + xlab("Nitrate (mM)") +ylab("LRD") 

ulu_LRD_6145




##6188##

# Subset the data to include only the first 120 values
subset_data_6188 <- table_5ecotypes[141:360, ]

# Remove rows with missing values in LR.count
subset_data_6188 <- subset_data_6188[!is.na(subset_data_6188$LR.count), ]

#change comma to point in LRD column
subset_data_6188$LRD..1.cm. <- as.numeric(gsub(",", ".", subset_data_6188$LRD..1.cm.))

#set as numeric data type
subset_data_6188$LRD..1.cm. <- as.numeric(subset_data_6188$LRD..1.cm.)




# analysis of variance
anova_LRD_6188 <- aov(LRD..1.cm.~factor(KNO3..mM.), data=subset_data_6188)

# Tukey's test
tukey_LRD_6188 <- TukeyHSD(anova_LRD_6188)

# compact letter display
cld_LRD_6188 <- multcompLetters4(anova_LRD_6188, tukey_LRD_6188)

# table with factors and 3rd quantile
dt_LRD_6188 <- subset_data_6188 %>% group_by(factor(KNO3..mM.)) %>%
  summarise(w=mean(LRD..1.cm.), sd = sd(LRD..1.cm.)) %>%
  arrange(desc(w))

# extracting the compact letter display and adding to the Tk table
cld_LRD_6188 <- as.data.frame.list(cld_LRD_6188$'factor(KNO3..mM.)')
dt_LRD_6188$cld <- cld_LRD_6188$Letters


# Reordering the rows in ascending order based on 'factor(KNO3..mM.)'
dt_LRD_6188 <- dt_LRD_6188[order(as.numeric(levels(dt_LRD_6188$'factor(KNO3..mM.)'))), ]






# Combine the data frames by row, filling missing columns with NA
combined_data_LRD_6188 <- bind_rows(subset_data_6188,dt_LRD_6188)





# Calculate ymax using dplyr
max_values_LRD_6188 <- combined_data_LRD_6188 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarize(ymax = max(LRD..1.cm., na.rm = FALSE))

# Delete the 7th value from max_values
max_values_LRD_6188 <- max_values_LRD_6188[-7, ]

# Remove the "factor(KNO3..mM.)" column from max_values
max_values_LRD_6188 <- max_values_LRD_6188 %>%
  select(-`factor(KNO3..mM.)`)


# Create a new column filled with NA and fill with x values
combined_data_LRD_6188$x_axis <- NA
combined_data_LRD_6188$x_axis[214:219] <- 1:6

# Create a new column filled with NA and fill max values
combined_data_LRD_6188$max_values <- NA
combined_data_LRD_6188$max_values[214:219] <- max_values_LRD_6188$ymax  # Assign ymax values to rows 117:122


# Move values from rows 117:122 to rows 1:6
combined_data_LRD_6188[1:6, 7:12] <- combined_data_LRD_6188[214:219, 7:12]

# Remove rows 49:51
combined_data_LRD_6188 <- combined_data_LRD_6188[-(214:219), ]


# Create boxplot
ulu_LRD_6188 <- ggplot(data = combined_data_LRD_6188, aes(x = factor(KNO3..mM.), y = LRD..1.cm.)) +
  geom_boxplot(fill = "blue", show.legend = FALSE) +
  geom_text(aes(label = cld, y = max_values, x = x_axis, vjust = -0.5)) +
  scale_y_continuous(name = "LRD", limits = c(0, 5)) +
  scale_x_discrete(name = "Nitrate (mM)") +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  ggtitle("LRD 6188") + xlab("Nitrate (mM)") + ylab("LRD")

ulu_LRD_6188



##7382##

# Subset the data to include only the first 120 values
subset_data_7382 <- table_5ecotypes[361:480, ]

# Remove rows with missing values in LR.count
subset_data_7382 <- subset_data_7382[!is.na(subset_data_7382$LR.count), ]

# Change comma to point in LRD column
subset_data_7382$LRD..1.cm. <- as.numeric(gsub(",", ".", subset_data_7382$LRD..1.cm.))

# Set as numeric data type
subset_data_7382$LRD..1.cm. <- as.numeric(subset_data_7382$LRD..1.cm.)



# Analysis of variance
anova_LRD_7382 <- aov(LRD..1.cm.~factor(KNO3..mM.), data=subset_data_7382)

# Tukey's test
tukey_LRD_7382 <- TukeyHSD(anova_LRD_7382)

# Compact letter display
cld_LRD_7382 <- multcompLetters4(anova_LRD_7382, tukey_LRD_7382)

# Table with factors and 3rd quantile
dt_LRD_7382 <- subset_data_7382 %>% group_by(factor(KNO3..mM.)) %>%
  summarise(w=mean(LRD..1.cm.), sd = sd(LRD..1.cm.)) %>%
  arrange(desc(w))

# Extracting the compact letter display and adding to the Tk table
cld_LRD_7382 <- as.data.frame.list(cld_LRD_7382$'factor(KNO3..mM.)')
dt_LRD_7382$cld <- cld_LRD_7382$Letters


# Reordering the rows in ascending order based on 'factor(KNO3..mM.)'
dt_LRD_7382 <- dt_LRD_7382[order(as.numeric(levels(dt_LRD_7382$'factor(KNO3..mM.)'))), ]







# Combine the data frames by row, filling missing columns with NA
combined_data_LRD_7382 <- bind_rows(subset_data_7382, dt_LRD_7382)


# Calculate ymax using dplyr
max_values_LRD_7382 <- combined_data_LRD_7382 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarize(ymax = max(LRD..1.cm., na.rm = FALSE))

# Delete the 7th value from max_values
max_values_LRD_7382 <- max_values_LRD_7382[-7, ]

# Remove the "factor(KNO3..mM.)" column from max_values
max_values_LRD_7382 <- max_values_LRD_7382 %>%
  select(-`factor(KNO3..mM.)`)


# Create a new column filled with NA and fill with x values
combined_data_LRD_7382$x_axis <- NA
combined_data_LRD_7382$x_axis[106:111] <- 1:6

# Create a new column filled with NA and fill max values
combined_data_LRD_7382$max_values <- NA
combined_data_LRD_7382$max_values[106:111] <- max_values_LRD_7382$ymax  # Assign ymax values to rows 117:122



# Move values from rows 117:122 to rows 1:6
combined_data_LRD_7382[1:6, 7:12] <- combined_data_LRD_7382[106:111, 7:12]

# Remove rows 49:51
combined_data_LRD_7382 <- combined_data_LRD_7382[-(106:111), ]




# Create boxplot
ulu_LRD_7382 <- ggplot(data = combined_data_LRD_7382, aes(x = factor(KNO3..mM.), y = LRD..1.cm.)) +
  geom_boxplot(fill = "orange", show.legend = FALSE) +
  geom_text(aes(label = cld, y = max_values, x = x_axis, vjust = -0.5)) +
  scale_y_continuous(name = "LRD", limits = c(0, 5)) +
  scale_x_discrete(name = "Nitrate (mM)") +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  ggtitle("LRD 7382") + xlab("Nitrate (mM)") + ylab("LRD")

ulu_LRD_7382







##5832##

# Subset the data to include only the first 120 values
subset_data_5832 <- table_5ecotypes[481:600, ]

# Remove rows with missing values in LR.count
subset_data_5832 <- subset_data_5832[!is.na(subset_data_5832$LR.count), ]

# Change comma to point in LRD column
subset_data_5832$LRD..1.cm. <- as.numeric(gsub(",", ".", subset_data_5832$LRD..1.cm.))

# Set as numeric data type
subset_data_5832$LRD..1.cm. <- as.numeric(subset_data_5832$LRD..1.cm.)



# Analysis of variance
anova_LRD_5832 <- aov(LRD..1.cm.~factor(KNO3..mM.), data=subset_data_5832)

# Tukey's test
tukey_LRD_5832 <- TukeyHSD(anova_LRD_5832)

# Compact letter display
cld_LRD_5832 <- multcompLetters4(anova_LRD_5832, tukey_LRD_5832)

# Table with factors and 3rd quantile
dt_LRD_5832 <- subset_data_5832 %>% group_by(factor(KNO3..mM.)) %>%
  summarise(w=mean(LRD..1.cm.), sd = sd(LRD..1.cm.)) %>%
  arrange(desc(w))

# Extracting the compact letter display and adding to the Tk table
cld_LRD_5832 <- as.data.frame.list(cld_LRD_5832$'factor(KNO3..mM.)')
dt_LRD_5832$cld <- cld_LRD_5832$Letters


# Reordering the rows in ascending order based on 'factor(KNO3..mM.)'
dt_LRD_5832 <- dt_LRD_5832[order(as.numeric(levels(dt_LRD_5832$'factor(KNO3..mM.)'))), ]







# Combine the data frames by row, filling missing columns with NA
combined_data_LRD_5832 <- bind_rows(subset_data_5832, dt_LRD_5832)






# Calculate ymax using dplyr
max_values_LRD_5832 <- combined_data_LRD_5832 %>%
  group_by(factor(KNO3..mM.)) %>%
  summarize(ymax = max(LRD..1.cm., na.rm = FALSE))

# Delete the 7th value from max_values
max_values_LRD_5832 <- max_values_LRD_5832[-7, ]

# Remove the "factor(KNO3..mM.)" column from max_values
max_values_LRD_5832 <- max_values_LRD_5832 %>%
  select(-`factor(KNO3..mM.)`)


# Create a new column filled with NA and fill with x values
combined_data_LRD_5832$x_axis <- NA
combined_data_LRD_5832$x_axis[102:107] <- 1:6

# Create a new column filled with NA and fill max values
combined_data_LRD_5832$max_values <- NA
combined_data_LRD_5832$max_values[102:107] <- max_values_LRD_5832$ymax  # Assign ymax values to rows 117:122



# Move values from rows 117:122 to rows 1:6
combined_data_LRD_5832[1:6, 7:12] <- combined_data_LRD_5832[102:107, 7:12]

# Remove rows 49:51
combined_data_LRD_5832 <- combined_data_LRD_5832[-(102:107), ]



# Create boxplot
ulu_LRD_5832 <- ggplot(data = combined_data_LRD_5832, aes(x = factor(KNO3..mM.), y = LRD..1.cm.)) +
  geom_boxplot(fill = "red", show.legend = FALSE) +
  geom_text(aes(label = cld, y = max_values, x = x_axis, vjust = -0.5)) +
  scale_y_continuous(name = "LRD", limits = c(0, 5)) +
  scale_x_discrete(name = "Nitrate (mM)") +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  ggtitle("LRD 5832") + xlab("Nitrate (mM)") + ylab("LRD")

ulu_LRD_5832
