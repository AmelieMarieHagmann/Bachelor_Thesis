##create heat map 5x5 heritability

library(gplots)

# Accessions to be included in the subset
accessions <- c("5832", "7382", "6188", "6145", "6909")
 
# Subset the rows and columns corresponding to the specified accessions
subset_K <- K[accessions, accessions]
  
#round values in subset
subset_K <- round(subset_K, 4)



###create heat map##

# Create a mirrored version of the matrix along the anti-diagonal
mirror_matrix <- subset_K[, ncol(subset_K):1]

# Reverse the order of rows in the mirrored matrix
mirror_matrix <- mirror_matrix[nrow(mirror_matrix):1, ]

# Define a custom color palette with lighter blue for value 1
custom_colors <- colorRampPalette(c("lightblue", "blue", "orange"))(35)

# Set the color for value 1 to a lighter shade of blue
custom_colors[1] <- "lightblue"

# Plot heatmap 
heatmap.2(mirror_matrix, 
          Rowv = NA, 
          Colv = NA, 
          col = custom_colors,  # Use the custom color palette
          scale = "none", 
          trace = "none", 
          dendrogram = "none", 
          cellnote = mirror_matrix, 
          notecol = "black", 
          symbreaks = TRUE,
          key = FALSE,  # Remove the legend
          keysize = 1.5, 
          key.title = NA,
          cexRow = 0.8, 
          cexCol = 0.8)

