# Archive versions of the clean data

# Aphid trait data ####
BIBS_MBV_2020_01_aphid_traits <- select(aphid_traits, Individual:tibia_length)
names(BIBS_MBV_2020_01_aphid_traits) <- sub("\\.","_",names(BIBS_MBV_2020_01_aphid_traits))

write.csv(BIBS_MBV_2020_01_aphid_traits , file = "clean data/BIBS_MBV_2020_01_aphid_traits.csv", row.names = FALSE)

# Plant trait data ####
BIBS_MBV_2020_01_plant_traits <- plant_traits_raw
names(BIBS_MBV_2020_01_plant_traits) <- sub("\\.","_",names(BIBS_MBV_2020_01_plant_traits))
BIBS_MBV_2020_01_plant_traits <- select(BIBS_MBV_2020_01_plant_traits, -image)
write.csv(BIBS_MBV_2020_01_plant_traits , file = "clean data/BIBS_MBV_2020_01_plant_traits.csv", row.names = FALSE)

# Colony parameters data ####
BIBS_MBV_2020_01_colony_parameters <- colony_parameters
names(BIBS_MBV_2020_01_colony_parameters) <- sub("\\.","_",names(BIBS_MBV_2020_01_colony_parameters))
names(BIBS_MBV_2020_01_colony_parameters) <- sub(" ","_",names(BIBS_MBV_2020_01_colony_parameters))

BIBS_MBV_2020_01_colony_parameters <- rename(BIBS_MBV_2020_01_colony_parameters, ID_plot = Plot )
write.csv(BIBS_MBV_2020_01_colony_parameters, file = "clean data/BIBS_MBV_2020_01_colony_parameters.csv", row.names = FALSE)

# Plot information ####
BIBS_MBV_2020_01_plot_information <- select(plot_data, ID_plot, Long, Lat, Seal_500, urban_rural)
write.csv(BIBS_MBV_2020_01_plot_information, file = "clean data/BIBS_MBV_2020_01_plot_information.csv", row.names = FALSE)


