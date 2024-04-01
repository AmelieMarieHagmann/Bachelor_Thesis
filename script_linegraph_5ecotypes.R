##line graph script


#create subset of the correct rows of the specific ecotype
subset_data_6909 <- data_5ecotypes_mean[1:6, ]

# Replace commas with periods and convert to numeric
subset_data_6909$Mean.biol.LR <- as.numeric(gsub(",", ".", subset_data_6909$Mean.biol.LR))
# Remove commas and convert to numeric
subset_data_6909$concentrations <- as.numeric(gsub(",", ".", subset_data_6909$concentrations))






## combine two or more lines in one graph with a legend
plot_combined <- ggplot() +
  geom_line(data = subset_data_6909, aes(x = concentrations, y = Mean.biol.LR, color = "6909")) +
  geom_line(data = subset_data_5832, aes(x = concentrations, y = Mean.biol.LR, color = "5832")) +
  labs(x = "Nitrate Concentration [mM]", y = "LR count") +
  xlim(0, 22) +
  ylim(0, 32) +
  ggtitle("Natural HOLT knockout") +
  scale_color_manual(values = c("6909" = "black", "5832" = "red"), 
                     labels = c("5832", "Col-0"))

print(plot_combined)

##combine highHOLT lines
plot_combined_highHOLT <- ggplot() +
  +     geom_line(data = subset_data_6909, aes(x = concentrations, y = Mean.biol.LR, color = "6909")) +
  +     geom_line(data = subset_data_7382, aes(x = concentrations, y = Mean.biol.LR, color = "7382")) + 
  +     geom_line(data = subset_data_6188, aes(x = concentrations, y = Mean.biol.LR, color = "6188")) +
  +     geom_line(data = subset_data_6145, aes(x = concentrations, y = Mean.biol.LR, color = "6145")) +
  +     labs(x = "Nitrate Concentration [mM]", y = "LR count") +
  +     xlim(0, 22) +
  +     ylim(0, 32) +
  +     ggtitle("highHOLT ecotypes") +
  +     scale_color_manual(values = c("6909" = "black", "7382" = "orange", "6188" = "blue", "6145" = "green"),
                           +                        breaks = c("7382", "6909", "6188", "6145"),
                           +                        labels = c("7382", "Col-0", "6188", "6145"))
 print(plot_combined_highHOLT)
 
 
 ##plot all lines improved layout
 plot_combined_all <- ggplot() +
   geom_line(data = subset_data_5832, aes(x = concentrations, y = Mean.biol.LR, color = "5832"), size = 1) +
   geom_line(data = subset_data_6909, aes(x = concentrations, y = Mean.biol.LR, color = "6909"), size = 1) +
   geom_line(data = subset_data_7382, aes(x = concentrations, y = Mean.biol.LR, color = "7382"), size = 1) + 
   geom_line(data = subset_data_6188, aes(x = concentrations, y = Mean.biol.LR, color = "6188"), size = 1) +
   geom_line(data = subset_data_6145, aes(x = concentrations, y = Mean.biol.LR, color = "6145"), size = 1) +
   labs(x = "Nitrate concentration [mM]", y = "LR count") +
   xlim(0, 22) +
   ylim(0, 32) +
   ggtitle("natural accessions") +
   scale_color_manual(values = c("5832" = "red", "6909" = "black", "7382" = "orange", "6188" = "blue", "6145" = "green"),
                      breaks = c("5832", "7382", "6909", "6188", "6145"),
                      labels = c("5832", "7382", "Col-0", "6188", "6145"))
 
 plot_combined_all
 
 