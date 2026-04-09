### Load Required Libraries ###
library(tidyverse)    #Data manipulation and plotting
library(pheatmap)     #Heatmaps
library(RColorBrewer) #Color palettes

### Load the Data ###

expression_data <- read.csv("expression_matrix.csv", header = TRUE, row.names =1)
rows_data <- read.csv("rows_metadata.csv", header = TRUE)
columns_data <- read.csv("columns_metadata.csv", header = TRUE)

### Check the Data ###

head(expression_data)[,1:5] #First 5 samples
head(rows_data)
head(columns_data)

### Fix Gene Row Names ###
rownames(expression_data) <- make.unique(rows_data$gene_symbol)
any(duplicated(rownames(expression_data)))  #Should be FALSE

### Fix Column Names & Align Metadata ###
columns_data$column_num <- as.character(columns_data$column_num)
colnames(expression_data) <- columns_data$column_num      #Align column numbers as names for expression data
columns_data <- columns_data[match(colnames(expression_data), columns_data$column_num),]
all(colnames(expression_data) == columns_data$column_num) #Should be TRUE

### Convert Age to Numeric for Plotting ###
columns_data$age_numeric <- as.numeric(gsub(" pwd", "", columns_data$age))

### Now Convert to Long Format ###
genes_to_view <- c("SOX2", "NEUROD6", "GAPDH")

### OPTIONAL: Subset Genes to View ###
expression_long <- expression_data[genes_to_view, ] %>%
  as.data.frame() %>%
  rownames_to_column(var = "gene_symbol") %>%
  pivot_longer(-gene_symbol, names_to = "sample_id", values_to = "expression") %>%
  left_join(columns_data, by = c("sample_id" = "column_num"))

head(expression_long)

### OPTIONAL: Friendly Names for Genes & Brain Regions ###
expression_long <- expression_long %>%
  mutate(friendly_gene = case_when(
    gene_symbol == "SOX2" ~ "Stem Cell Marker SOX2",
    gene_symbol == "NEUROD6" ~ "Neuron Differentiation NEUROD6",
    gene_symbol == "GAPDH" ~ "Housekeeping GAPDH",
    TRUE ~ gene_symbol
  )) %>%
  mutate(friendly_region = structure_name)

expression_long

### PLOTTING! ###

