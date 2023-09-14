# Archive versions of the clean data

# Aphid trait data ####
BIBS_MBV_2020_01_aphid_traits <- select(aphid_traits, Individual:tibia_length) %>%
  rename(Plot_name = ID_plot)
names(BIBS_MBV_2020_01_aphid_traits) <- sub("\\.","_",names(BIBS_MBV_2020_01_aphid_traits))

write.csv(BIBS_MBV_2020_01_aphid_traits , file = "clean data/BIBS_MBV_2020_01_aphid_traits.csv", row.names = FALSE, na = "")

# Plant trait data ####
BIBS_MBV_2020_01_plant_traits <- plant_traits_raw
names(BIBS_MBV_2020_01_plant_traits) <- sub("\\.","_",names(BIBS_MBV_2020_01_plant_traits))
BIBS_MBV_2020_01_plant_traits <- select(BIBS_MBV_2020_01_plant_traits, -image) %>%
  rename(Plot_name = Plot)
write.csv(BIBS_MBV_2020_01_plant_traits , file = "clean data/BIBS_MBV_2020_01_plant_traits.csv", row.names = FALSE, na = "")

# Colony parameters data ####
BIBS_MBV_2020_01_colony_parameters <- colony_parameters
names(BIBS_MBV_2020_01_colony_parameters) <- sub("\\.","_",names(BIBS_MBV_2020_01_colony_parameters))
names(BIBS_MBV_2020_01_colony_parameters) <- sub(" ","_",names(BIBS_MBV_2020_01_colony_parameters))

BIBS_MBV_2020_01_colony_parameters <- rename(BIBS_MBV_2020_01_colony_parameters, Plot_name = Plot )
write.csv(BIBS_MBV_2020_01_colony_parameters, file = "clean data/BIBS_MBV_2020_01_colony_parameters.csv", row.names = FALSE, na = "")

# Plot information ####
BIBS_MBV_2020_01_plot_information <- select(plot_data, Plot_name = ID_plot, Long, Lat, Seal_500, urban_rural)
write.csv(BIBS_MBV_2020_01_plot_information, file = "clean data/BIBS_MBV_2020_01_plot_information.csv", row.names = FALSE)


library(sp)
coordinates(BIBS_MBV_2020_01_plot_information)<- ~Long+Lat
class(BIBS_MBV_2020_01_plot_information)
sp::bbox(BIBS_MBV_2020_01_plot_information)
