# Import plant trait data

# Import lab measurements
plant_traits <- fread("data/Tanacetum lab measurements 2020.csv",
                      data.table = FALSE, fill = TRUE, encoding = "Latin-1")

# Import leaf area measurements form Image J
leaf_areas <- fread("data/Summary leaf area measurements.txt",
                      data.table = FALSE, fill = TRUE, encoding = "Latin-1")

# extract unique image number:
leaf_areas$image.number <- as.numeric(str_extract(leaf_areas$Slice,pattern = "\\d+"))

# remove rows corresponding to repeats:
leaf_areas <- leaf_areas[which(
  !leaf_areas$Slice %in% c(
    "leafscan.jpeg", 
    "leaf scan 1.jpeg")
  ),]

# Merge with trait data: (area is in cm2)
plant_traits$total.area <- leaf_areas[match(plant_traits$image,
                                            leaf_areas$image.number),
                                            "Total Area"]

# Calculate mean leaf area (ie dividing per number of leaves - here we have only 1)
plant_traits$LA <- plant_traits$total.area/plant_traits$number


# Remove the outlier values >100
plant_traits <- plant_traits[ - which(plant_traits$total.area >100),] 

# calculate SLA (specific leaf area - in mm2/mg-1) 
# SLA = leaf area (cm2) * 100 / Dry mass (mg) 
plant_traits$SLA <- plant_traits$total.area/plant_traits$DM *100
# there are some big outliers there!!



# Calculate Leaf and stem DMC 
plant_traits$DMC <- plant_traits$FM/(plant_traits$DM/100)

# Calculate plant mean traits per individual plant:

plant_traits %>%
  filter(organ == "leaf") %>%
  group_by(Plot, Individual) %>%
  summarise(LA = mean(LA, na.rm = TRUE),
            SLA = mean(SLA, na.rm = TRUE),
            LDMC = mean(DMC, na.rm = TRUE)
