

##plot datapoint in colour on top ##

#LRp75

# Subset the row where 'accession_id' is 6909
row_with_6909 <- LRpMR75[LRpMR75$accession_id == 6909, ]

# Extract the value of 'phenotype_value' from the extracted row
phenovalue_75_6909 <- row_with_6909$phenotype_value




# Create an empty plot with the desired axis limits and labels
plot(x = c(0, 20), y = c(0, 40), type = "n",
     xlab = "LR count ct", ylab = "LR count",
     main = "Scatterplot 75mM NaCl")

# Add points from the all accessions plot
points(phenotype_salt_ct, phenotype_salt_LRp75, pch = 1, col = "black")

# Add points from the colored plot
points(phenovalue_75_5832ct, phenovalue_75_5832, pch = 19, col = "red")
points(phenovalue_75_7382ct, phenovalue_75_7382, pch = 19, col = "orange")
points(phenovalue_75_6188ct, phenovalue_75_6188, pch = 19, col = "blue")
points(phenovalue_75_6909ct, phenovalue_75_6909, pch = 19, col = "black")

# Add legend
legend("topright", legend = c("all", "5832","7382","6188","6909"),
       col = c("black", "red","orange","blue","black"), pch = c(1, 19,19,19,19),
       title = "legend", box.lty = 0, cex = 0.8, text.width = 2)
#add diagonal line
abline(a = 0, b = 1, col = "black", lwd = 2) 




#LRp125

# Subset the row where 'accession_id' is 6909
row_with_125_6909 <- LRpMR125[LRpMR125$accession_id == 6909, ]

# Extract the value of 'phenotype_value' from the extracted row
phenovalue_125_6909 <- row_with_125_6909$phenotype_value





# Create an empty plot with the desired axis limits and labels
plot(x = c(0, 20), y = c(0, 40), type = "n",
     xlab = "LR count ct", ylab = "LR count",
     main = "Scatterplot 125mM NaCl")

# Add points from the all accessions plot
points(phenotype_salt_ct, phenotype_salt_LRp125, pch = 1, col = "black")

# Add points from the colored plot
points(phenovalue_75_5832ct, phenovalue_125_5832, pch = 19, col = "red")
points(phenovalue_75_7382ct, phenovalue_125_7382, pch = 19, col = "orange")
points(phenovalue_75_6188ct, phenovalue_125_6188, pch = 19, col = "blue")
points(phenovalue_75_6909ct, phenovalue_125_6909, pch = 19, col = "black")

# Add legend
legend("topright", legend = c("all", "5832","7382","6188","6909"),
       col = c("black", "red","orange","blue","black"), pch = c(1, 19,19,19,19),
       title = "legend", box.lty = 0, cex = 0.8, text.width = 2)
#add line
abline(a = 0, b = 1, col = "black", lwd = 2) 
