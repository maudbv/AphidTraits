# Import environmental data for each location

# import data for all cityscapelabs
plot_data <- fread("data/Environment_data.csv",
                   data.table = FALSE)

plot_data <- plot_data[,-1] # remove useless first columm

# add Hanski indices

hanski_plots <- fread("data/Hanski index per plot.csv",
                   data.table = FALSE)

plot_data <- merge(plot_data, hanski_plots[, c("ID_plot","DryGr_Hanksi_a1", "Herb_Hanski_a1")])

# simplify column name for hanski indices:
colnames(plot_data)[colnames(plot_data) == "DryGr_Hanksi_a1"] <- "Hanski_DryGr"
colnames(plot_data)[colnames(plot_data) == "Herb_Hanski_a1"] <- "Hanski_Herb"

# Select only plots useful for the aphid survey
plot_data <- plot_data[which(plot_data$ID_plot %in% aphid_traits$ID_plot),]

dim(plot_data) 
# => 9 plots in total (without Südkreuz which needs to be added to the table)

## Add new Suedkreuz plot information:
Suedkreuz_data <- fread("data/Maud_AdditionalPlotSuedkreuz.csv",
                   data.table = FALSE)

# give the plot a unique ID matching the aphid_trait tables:
Suedkreuz_data$ID_plot <- "Sk_01"

# replace column name for Tree cover to match the name in plot_data:
colnames(Suedkreuz_data)[colnames(Suedkreuz_data) == "TCD_patch"] <- "TreeCover_patch"

# merge tables
plot_data <- merge(plot_data, Suedkreuz_data, all = TRUE)

# add urban vs. rural category:
# below 20 % sealing = rural
# above 20 % sealing = urban

plot_data$urban_rural <- c("urban", "rural")[(plot_data$Seal_500<20) +1]

# Create row names with ID_plot
rownames(plot_data) <- plot_data$ID_plot
