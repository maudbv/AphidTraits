# Import environmental data for each location

# import data for all cityscapelabs
plot_data <- fread("data/Environment_data.csv",
                   data.table = FALSE)

plot_data <- plot_data[,-1] # remove useless first columm
rownames(plot_data) <- plot_data$ID_plot
