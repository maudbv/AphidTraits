# Import environmental data for each location

# import data for all cityscapelabs
plot_data <- fread("data/Environment_data.csv",
                   data.table = FALSE)

plot_data <- plot_data[,-1] # remove useless first columm
rownames(plot_data) <- plot_data$ID_plot

# Select only plots useful for the aphid survey
plot_data <- plot_data[which(plot_data$ID_plot %in% aphid_traits$ID_plot),]

dim(plot_data) 
# => 9 plots in total (without Südkreuz which needs to be added to the table)

# add urban vs. rural category:
# below 20 % sealing = rural
# above 20 % sealing = urban

plot_data$urban_rural <- c("urban", "rural")[(plot_data$Seal_500<20) +1]
