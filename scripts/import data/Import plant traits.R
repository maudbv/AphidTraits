# Import plant trait data

# Import lab measurements
plant_traits_raw <- fread("data/Tanacetum lab measurements 2020.csv",
                      data.table = FALSE,
                      fill = TRUE,
                      encoding = "Latin-1")

# correct typos in the plot names
unique(plant_traits_raw$Plot)
sub(plant_traits_raw$Plot, pattern = "Oh0", replacement = "Oh_0")
sub(plant_traits_raw$Plot, pattern = "SÃ¼dkreuz", replacement = "Suedkreuz")



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
plant_traits_raw$total.area <- leaf_areas[match(plant_traits_raw$image,
                                            leaf_areas$image.number),
                                            "Total Area"]

# Calculate mean leaf area (ie dividing per number of leaves - here we have only 1)
plant_traits_raw$LA <- plant_traits_raw$total.area/plant_traits_raw$number


# Remove the outlier values >100
plant_traits_raw <- plant_traits_raw[ - which(plant_traits_raw$total.area >100),] 

# calculate SLA (specific leaf area - in mm2/mg-1) 
# SLA = leaf area (cm2) * 100 / Dry mass (mg) 
plant_traits_raw$SLA <- plant_traits_raw$total.area/plant_traits_raw$DM *100
# there are some big outliers there!!

# Calculate Leaf and stem DMC (mg per g)
plant_traits_raw$DMC <- plant_traits_raw$DM/plant_traits_raw$FM *1000

# Calculate stem volume in mm2
plant_traits_raw$stem.volume <- plant_traits_raw$length*
    pi*(plant_traits_raw$width/2)^2

# Calculate stem tissue density in mg/mm2
plant_traits_raw$stem.density <- plant_traits_raw$DM/plant_traits_raw$stem.volume

# Remove one stem outlier (stem very dense, too old?)
plant_traits_raw <-
  plant_traits_raw[ - which(plant_traits_raw$Plot == "Nh_10" &
                         plant_traits_raw$Individual == 2 &
                         plant_traits_raw$replicate == "e"),]

  
# Calculate plant mean traits per individual plant:

# mean leaf traits:
plant_traits <- plant_traits_raw %>%
  filter(organ == "leaf") %>%
  group_by(Plot, Individual) %>%
  summarise(LA.mean = mean(LA, na.rm = TRUE),
            LA.sd = sd(LA, na.rm = TRUE),
            SLA.mean = mean(SLA, na.rm = TRUE),
            SLA.sd = sd(SLA, na.rm = TRUE),
            LDMC.mean = mean(DMC, na.rm = TRUE),
            LDMC.sd = sd(DMC, na.rm = TRUE),
            nb.leaf = n()
            )

# mean stem traits:
tmp <- plant_traits_raw %>%
  filter(organ == "stem") %>%
  group_by(Plot, Individual) %>%
  summarise(SDMC.mean = mean(DMC, na.rm = TRUE), # stem dry matter content
            SDMC.sd = sd(DMC, na.rm = TRUE), 
            stem.density.mean = mean(stem.density, na.rm = TRUE), # stem density
            stem.density.sd = sd(stem.density, na.rm = TRUE), 
            stem.diameter.mean = mean(width, na.rm = TRUE), # stem diameter
            stem.diameter.sd = sd(width, na.rm = TRUE), 
            nb.stem = n()
            )

# Assemble leaf and stem traits:
plant_traits <- merge(plant_traits, tmp)
rm(tmp)
