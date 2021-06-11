# IMPORT APHID TRAIT DATA
# Script to import, clean and format the aphid trait length measurements
# from microscope photography

#### OUTPUT: three main data tables for analyses:
#  - aphid_df: dataframe with all trait measurements including repetitions
#  - aphid_traits_long:  mean trait values per individuals, all in one column
#  - aphid_traits: aphid_traits_long transformed as one column per trait
#  - missing_measurements: table of missing imageJ results (only 16)

# Import length measurements from image analyses ####

## import and assemble files

# get all paths to text files
list_of_files = list.files(path = "data/imageJ results/",
                           recursive = TRUE,
                           pattern = "\\.txt$", 
                           full.names = TRUE)

# read and bind all text files
aphid_df <-   purrr::map_dfr(.f = data.table::fread,
                              .x = list_of_files,
                              data.table = FALSE)

# Change to latin-1 encoding to deal with german special characters
Encoding(aphid_df$Label) <- "latin1"

## extract information from label
aphid_df = aphid_df %>%
  separate(Label,
           into = c("PhotoName", "TraitLabel"),
           sep = ":",
           remove = FALSE)

# import plot name conversion table:
plot_names <- fread("data/table of spotnames_maud.csv", data.table = FALSE)

# Add new columns for ID_plot, collector and date of collection
for (i in unique(plot_names$`name of image`)){
  ind <- grep(pattern = i, x = aphid_df$PhotoName)
  
  # Only for judith plot names
  if( plot_names[plot_names$`name of image` == i,
                 "collector"] =="Judith"){
    # Account for some of the Judith plot with ambiguous names
    # e.g. U2_ vs. U2_2
    ind1 <- grep(pattern = paste(i,"1", sep = ""),
                 x = aphid_df$PhotoName)
    if (length(ind1) >0) ind <- ind[which(!ind %in% ind1)]
    
    ind2 <- grep(pattern = paste(i,"2", sep = ""),
                 x = aphid_df$PhotoName)
    if (length(ind2) >0) ind <- ind[which(!ind %in% ind2)]
    
  }
  
  # extract info for the plot:
  aphid_df[ind, "ID_plot"] <- 
    plot_names[plot_names$`name of image` == i, "ID_plot"]
  aphid_df[ind, "collector"] <- 
    plot_names[plot_names$`name of image` == i, "collector"]
  aphid_df[ind, "date_collected"] <- 
    plot_names[plot_names$`name of image` == i, "date collected"]
  aphid_df[ind, "colony_judith"] <- 
    plot_names[plot_names$`name of image` == i, "Colony"]
}

# Divide the photo name into 5 columns:
aphid_df <- aphid_df %>%
  separate(PhotoName,
           into = c("PlotA", "PlotB",
                    "ColonyNumber","Individual_letter", "PhotoType"),
           fill = "warn",
           sep = "_",
           remove = FALSE
  )

# Correct columns for Sudkreuz plot
# replace with the correct columns:
aphid_df[which(aphid_df$ID_plot == "Sk_01"),
             c("ColonyNumber","Individual_letter", "PhotoType")] <-
  aphid_df[which(aphid_df$ID_plot == "Sk_01"),
               c("PlotB", "ColonyNumber","Individual_letter")]

# Correct columns for some of the judith samples, 
# Spandau R2B is special::
aphid_df[which(aphid_df$PlotA == "Spandau R2B"),
         c("Individual_letter", "PhotoType")] <-
  aphid_df[which(aphid_df$PlotA == "Spandau R2B"),
           c("PlotB", "ColonyNumber")]

# since they have no colony number :
aphid_df[which(aphid_df$collector == "Judith" & 
                 !aphid_df$ColonyNumber %in% c(1,2) &
                 aphid_df$PlotA != "Spandau R2B"),
         c("Individual_letter", "PhotoType")] <-
  aphid_df[which(aphid_df$collector == "Judith" & 
                   !aphid_df$ColonyNumber %in% c(1,2)&
                   aphid_df$PlotA != "Spandau R2B"),
           c("ColonyNumber","Individual_letter")]


# replace colony number by identifier 
aphid_df[aphid_df$collector == "Judith","ColonyNumber"] <- aphid_df[aphid_df$collector == "Judith","colony_judith"]

# Create a unique identifier for each individual aphid:
aphid_df$Colony <- paste(aphid_df$ID_plot, 
                                 aphid_df$ColonyNumber,
                                 sep = "-")


# Create a unique identifier for each individual aphid:
aphid_df$Individual <- paste(aphid_df$Colony,
                                 aphid_df$Individual_letter,
                                 sep = "-")



# Extract name of trait and "part" = when trait measure was cut into two parts
aphid_df <- aphid_df %>%
  separate(TraitLabel, into = c("Trait","Part"),
           sep = "_part", remove = FALSE)

# extract "Rep": the replication number of each trait measurement
# (should be three unique numbers per trait)
aphid_df$Rep = str_extract(aphid_df$Trait, "\\d+")


# Check for left-right inconsistencies between photo name and trait label
false_right <- intersect(grep("left", aphid_df$PhotoName), 
                         grep( "right",aphid_df$Trait))
aphid_df[false_right, "Trait"] <- sub(pattern = "right",
                                      replacement = "left",
                                      aphid_df[false_right, "Trait"])

false_left <- intersect(grep("right", aphid_df$PhotoName), 
                        grep( "left",aphid_df$Trait))
aphid_df[false_left, "Trait"] <- sub(pattern = "left",
                                      replacement = "right",
                                      aphid_df[false_left, "Trait"])

# Extract "side": when appropriate, left or right measurement
aphid_df$side <- NA
aphid_df$side[grep( "left",aphid_df$Trait)] <- "left"
aphid_df$side[grep( "right",aphid_df$Trait)] <- "right"

# extract "Trait.type": the generic type of trait (e.g. "antenna length")
# without the information about left or right
aphid_df$Trait.type = sapply(aphid_df$Trait, function(x) {
  paste(str_split(x,pattern = "_",simplify = TRUE)[1:2], collapse = "_")
})

# extract "Trait": the full trait name including "left" or "right"
aphid_df$Trait = sapply(aphid_df$Trait, function(x) {
  y = str_split(x,pattern = "_",simplify = TRUE)
  return(paste(y[1:length(y)-1], collapse = "_"))
})

# Check if trait names make sense:
sort(unique(aphid_df$Trait))
# replace "flag" by ANT3 for the third antenna segment measured?
aphid_df$Trait <- str_replace_all(aphid_df$Trait,
                pattern = "flag",
                replacement = "ant3")

aphid_df$Trait.type <- str_replace_all(aphid_df$Trait.type,
                pattern = "flag",
                replacement = "ant3")

# Clean up unnecessary columns:
aphid_df <- aphid_df[, -which(colnames(aphid_df) %in% c( "V1", "colony_judith","PlotA", "PlotB"))] 

# Import and reformat metadata on photo magnification and scale ####

## import and reformat excel data
library(rio)
data_list <- rio::import_list("data/Checklist 2020_maud edits.xlsx",
                         setclass = "data.table")
# Sheet 17 is empty ?
#  data_list[17] is "Südgelände U3 29.8.19"
# CHOICE: fill the missing info by copying the previous datasheet
# all of judith's plot appear to have similar settings
# so it is a reasonable assumption
data_list[17] <- data_list[16] 
data_list[[17]]$comment <- " missing info was filled by copying  `Schöneweide U2.2 7.8.19`"
# bind together all data sheets
magnif_data <- rbindlist(data_list, fill = TRUE, id = TRUE)

# remove empty lines
magnif_data <- magnif_data[-which(apply(magnif_data[,-1],1,
                                    function(x) all(is.na(x)))),]

# merge columns for ocular tubercules
colnames(magnif_data)
# col 17 : "ocular tubercules"
# col 14 : "ocular tubercles (Kontrast 40, Hell. 0, Sättigung 77)"
magnif_data[which(!is.na(magnif_data[,14])), 17] <- 
  magnif_data[which(!is.na(magnif_data[,14])), 14]

# transform in one column data frame for magnification
magnif_df <- data.frame(
  magnif_data[, c(".id", "colony","individual")],
  stack(x = magnif_data[, c("dorsal","ventral",
                    "left leg","right leg",
                    "left antenna","right antenna",
                    "rostrum", "ocular tubercles")])
  )

# rename the stacked columns:
magnif_df <- rename(magnif_df,
                    PhotoType = ind,
                    Magnification = values)
magnif_df$PhotoType = as.character(magnif_df$PhotoType)
# rename photo types to match photo names
# "rostrum" = mouthpart
magnif_df$PhotoType[which(magnif_df$PhotoType == "rostrum")] <- "mouthpart"

# rename colony to match photo names
magnif_df$colony <- sub(pattern = "_",
                        replacement  = ".",
                        x = magnif_df$colony)

# Try to match plot id with name in plot_names

# problematic plot names:
tmp <- unique(magnif_df$.id)[which(unique(magnif_df$.id) %in% plot_names$`original name of spot`)]

tmp <- rbind(
  data.frame(checklist_name = c("NB 10_1 R3 4.9.19","NB 6_7 U1 4.9.19",
                     "WS 9_3 R1 20.8.19",
                     "Schöneweide U2.2 7.8.19",
                     "Müggelsee R3B 30.8.19","Müggelsee R3 7.8.19",
                     "Spandau R2B 29.8.19","Spandau R2 6.8.19"),
             correct_name = c("NB10_1 R3","NB6_7 U1",
                   "WS 9_3 R1",
                   "Schöneweide U2 7.8.19",
                   "Müggelsee R3B","Müggelsee R3",
                   "Spandau R2B","Spandau R2")
             ), 
  cbind(checklist_name = tmp,
        correct_name =tmp)
  )

magnif_df$name_match <- tmp$correct_name[
  match(magnif_df$.id, tmp$checklist_name)]

              
magnif_df$NamePlotPhoto <- plot_names$`name of image`[
                match(magnif_df$name_match, plot_names$`original name of spot`)]

magnif_df$ID_plot <- plot_names$ID_plot[
  match(magnif_df$name_match, plot_names$`original name of spot`)]          
              
magnif_df$collector<- plot_names$collector[
  match(magnif_df$name_match, plot_names$`original name of spot`)]          

# remove colony number for Judith

magnif_df$colony[magnif_df$collector == "Judith"] <-""

# Add unique photo index to match to aphid_traits_long table
magnif_df$PhotoName <- apply(
  as.data.frame(magnif_df[, c("NamePlotPhoto", "colony","individual","PhotoType")]),
  MARGIN = 1, 
  FUN = paste, collapse = "_")

magnif_df$PhotoName <- gsub(pattern = "___", replacement = "_", magnif_df$PhotoName)
magnif_df$PhotoName <- gsub(pattern = "__", replacement = "_", magnif_df$PhotoName)
magnif_df$PhotoName <- paste(magnif_df$PhotoName, ".jpg", sep = "")

# Merge tables by photo name ####

aphid_df = merge( aphid_df,
       magnif_df[,c("PhotoName", "Magnification")] ,
       by = "PhotoName",
       all.x = TRUE)

# Correcting the magnification (outliers)

#Changing magnification of outliers in femur+tarsus+tibia+ant3 length of Oh_01-3-E
which(aphid_df$Individual == "Oh_01-3-E" & aphid_df$Trait.type == "femur_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-3-E" & 
          aphid_df$Trait.type == "femur_length"),
  "Magnification"] <- "2"

which(aphid_df$Individual == "Oh_01-3-E" & aphid_df$Trait.type == "tarsus_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-3-E" & 
          aphid_df$Trait.type == "tarsus_length"),
  "Magnification"] <- "2"

which(aphid_df$Individual == "Oh_01-3-E" & aphid_df$Trait.type == "tibia_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-3-E" & 
          aphid_df$Trait.type == "tibia_length"),
  "Magnification"] <- "2"

which(aphid_df$Individual == "Oh_01-3-E" & aphid_df$Trait.type == "ant3_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-3-E" & 
          aphid_df$Trait.type == "ant3_length"),
  "Magnification"] <- "2"

#Changing magnification of outliers in rostrum length of Oh_01-2-B and Oh_01-2-C
which(aphid_df$Individual == "Oh_01-2-B" & aphid_df$Trait.type == "rostrum_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-2-B" & 
          aphid_df$Trait.type == "rostrum_length"),
  "Magnification"] <- "4"

which(aphid_df$Individual == "Oh_01-2-C" & aphid_df$Trait.type == "rostrum_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-2-C" & 
          aphid_df$Trait.type == "rostrum_length"),
  "Magnification"] <- "4"

#Changing magnification of outliers in ventral.jpg of Oh_01-3-D and Nh_10-1-B
which(aphid_df$Individual == "Oh_01-3-D" & aphid_df$PhotoType == "ventral.jpg")

aphid_df[
  which(aphid_df$Individual == "Oh_01-3-D" & 
          aphid_df$PhotoType == "ventral.jpg"),
  "Magnification"] <- "1"

which(aphid_df$Individual == "Nh_10-1-B" & aphid_df$PhotoType == "ventral.jpg")

aphid_df[
  which(aphid_df$Individual == "Nh_10-1-B" & 
          aphid_df$PhotoType == "ventral.jpg"),
  "Magnification"] <- "1"
# Import pixel scale conversion table ####
magnif_conversion <- fread("data/calibration 2020_maud edits.csv")

# Convert pixel measurements in mm ####

aphid_df$Length.mm <- aphid_df$Length/
  magnif_conversion$mean.pixel.per.mm[match(
    as.numeric(aphid_df$Magnification),
    magnif_conversion$magnification)]

# Correcting wrong names (outliers)
# Changing outliers by giving correct name to measurement
#(in "Trait" and "Trait.type" but not in "TraitLabel", because that's the original label)

which(aphid_df$Individual == "Nl_55-1-A" & aphid_df$Trait.type == "siph_length" &
        aphid_df$Trait == "siph_length")

aphid_df[
  which(aphid_df$Individual == "Nl_55-1-A" & 
          aphid_df$Trait.type == "siph_length" &
          aphid_df$Trait == "siph_length"),
  "Trait.type" ] <- "head_width"

aphid_df[
  which(aphid_df$Individual == "Nl_55-1-A" & 
          aphid_df$Trait == "siph_length"),
  "Trait" ] <- "head_width"

# WS9_3_A is individual Ol_55-A-A
which(aphid_df$Individual == "Ol_55-A-A" & aphid_df$Trait.type == "siph_length" &
        aphid_df$Trait == "siph_length")

aphid_df[
  which(aphid_df$Individual == "Ol_55-A-A" & 
          aphid_df$Trait.type == "siph_length" &
          aphid_df$Trait == "siph_length"),
  "Trait.type" ] <- "head_width"

aphid_df[
  which(aphid_df$Individual == "Ol_55-A-A" & 
          aphid_df$Trait == "siph_length"),
  "Trait" ] <- "head_width"


# SS_U3_1_C is individual Nl_55-1-C

which(aphid_df$Individual == "Nl_55-1-C" & aphid_df$Trait.type == "siph_length" &
        aphid_df$Trait == "siph_length")

aphid_df[
  which(aphid_df$Individual == "Nl_55-1-C" & 
          aphid_df$Trait.type == "siph_length" &
          aphid_df$Trait == "siph_length"),
  "Trait.type" ] <- "head_width"

aphid_df[
  which(aphid_df$Individual == "Nl_55-1-C" & 
          aphid_df$Trait == "siph_length"),
  "Trait" ] <- "head_width"

# MS_R3B_E is individual Ol_11-E-E

which(aphid_df$Individual == "Ol_11-E-E" & aphid_df$Trait.type == "siph_length" &
        aphid_df$Trait == "siph_length")

aphid_df[
  which(aphid_df$Individual == "Ol_11-E-E" & 
          aphid_df$Trait.type == "siph_length" &
          aphid_df$Trait == "siph_length"),
  "Trait.type" ] <- "head_width"

aphid_df[
  which(aphid_df$Individual == "Ol_11-E-E" & 
          aphid_df$Trait == "siph_length"),
  "Trait" ] <- "head_width"

# NA values (outliers)

which(aphid_df$Individual == "Oh_01-1.2-E" & aphid_df$Trait.type == "abdomen_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-1.2-E" & 
          aphid_df$Trait.type == "abdomen_length"),
  "Length.mm"] <- "NA"

which(aphid_df$Individual == "Oh_02-3-B" & aphid_df$Trait.type == "body_width")

aphid_df[
  which(aphid_df$Individual == "Oh_02-3-B" & 
          aphid_df$Trait.type == "body_width"),
  "Length.mm"] <- "NA"

which(aphid_df$Individual == "Oh_01-2-C" & aphid_df$Trait.type == "head_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-2-C" & 
          aphid_df$Trait.type == "head_length"),
  "Length.mm"] <- "NA"

which(aphid_df$Individual == "Om_06-1-D" & aphid_df$Trait.type == "rostrum_length")

aphid_df[
  which(aphid_df$Individual == "Om_06-1-D" & 
          aphid_df$Trait.type == "rostrum_length"),
  "Length.mm"] <- "NA"

which(aphid_df$Individual == "Om_06-1-C" & aphid_df$Trait.type == "rostrum_length")

aphid_df[
  which(aphid_df$Individual == "Om_06-1-C" & 
          aphid_df$Trait.type == "rostrum_length"),
  "Length.mm"] <- "NA"

which(aphid_df$Individual == "Om_02-1-B" & aphid_df$Trait == "ant3_length_right")

aphid_df[
  which(aphid_df$Individual == "Om_02-1-B" & 
          aphid_df$Trait == "ant3_length_right"),
  "Length.mm"] <- "NA"

which(aphid_df$Individual == "Oh_01-3-E" & aphid_df$Trait.type == "rostrum_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-3-E" & 
          aphid_df$Trait.type == "rostrum_length"),
  "Length.mm"] <- "NA"
# Calculate mean trait values across 3 replicate measures ####
aphid_traits_long <- aphid_df %>% 
  group_by(collector, date_collected, PhotoName, ID_plot,
           Colony,Individual, Trait.type,Trait, side, Part) %>%
  summarise(count.reps = n(),
            Length.mean = mean(Length.mm, na.rm = TRUE))

# sum parts 1 and 2 of traits when relevant
aphid_traits_long <- aphid_traits_long %>%
  group_by(collector, date_collected, PhotoName, ID_plot,
           Colony,Individual, Trait.type,Trait, side, count.reps) %>%
  summarise(count.parts = n(),
            Length.mean = sum(Length.mean, na.rm = TRUE))

# make sure aphid_trait is a data.frame
aphid_traits_long <- as.data.frame(aphid_traits_long)

# Import and add rhinaria counts ####
rhin_count_left <- fread("data/rhinaria_LA.csv", data.table = FALSE)
rhin_count_right <- fread("data/rhinaria_RA.csv", data.table = FALSE)
# merge with aphid_trait table by photo name
# add both to the right hand antenna
aphid_traits_long$Rhinaria_left <-  rhin_count_left[
  match(aphid_traits_long$PhotoName, rhin_count_right$name),
  "#rhinaria"]

aphid_traits_long$Rhinaria_right <-  rhin_count_right[
  match(aphid_traits_long$PhotoName, rhin_count_right$name),
  "#rhinaria"]

# Check for duplicates and mistakes #######

# identify individuals with multiple trait measurements:
x = table(aphid_traits_long$Individual, aphid_traits_long$Trait)

# Which are apparently measured more than once ?
length(duplicated_individuals <- rownames(x) [rowSums(x >1)>0])
# NO more obvious REMAINING DUPLICATED

## STILL MISSING: Photos which should have been measured (according to the checklist) but do not appear in aphid_traits_long:
missing_measurements <- magnif_df[which(
  !(magnif_df$PhotoName %in% aphid_traits_long$PhotoName) &
    !is.na(magnif_df$Magnification) &
    (magnif_df$Magnification!= "NA")),]

nrow(missing_measurements) #34 
# BUT: 2 individuals (D and E) are actually really missing from Judith's SG_U3_2, plus a few body parts of other individuals, so not real mistakes we can fix.
missing_measurements <- 
  missing_measurements[
    -which(missing_measurements$name_match == "Südgelände U3 29.8.19"),]

nrow(missing_measurements)
# 16 missing measurements for some photos with non-standard names
# -------> to check and update only if we have time.

# SOME mistakes due to 2 parts (left and right) on same picture:
# e.g. "Ol_55_1_B_left leg" and "Ol_55_1_B_right leg" 
# do not appear because the pictures had a different name:
# "Ol_55_1_B_legs L&R" which was not ignored by the aphid trait plugin...

# Two due to typos on "ocular tubercles"


# Create a "wide" version of aphid_traits_long  ####
# wide = organised with traits as columns
aphid_traits <- reshape(
  aphid_traits_long[,c("ID_plot",
                  "Colony","Individual",
                  "date_collected","collector",
                  "Trait", "Length.mean")],
  v.names = "Length.mean",
  idvar = c("ID_plot",
            "Colony","Individual",
            "date_collected","collector"),
  timevar = "Trait",
  direction = "wide")

# correct names to simplify
names(aphid_traits) <- sub(pattern = "Length.mean.", replacement = "",
                                names(aphid_traits))

# add the rhinaria count:
aphid_traits <- merge(aphid_traits, 
      aphid_traits_long[aphid_traits_long$Trait == "ant3_length_right",
             c("Individual","Rhinaria_left","Rhinaria_right")],
      by = "Individual",
      all = TRUE)


# Calculate estimated mean number of rhinaria
aphid_traits$Rhinaria.mean <- apply(
  aphid_traits[, c("Rhinaria_left","Rhinaria_right")],
  1, mean, na.rm = TRUE)

# Calculate mean trait per individual for left and right values:
aphid_traits$ant3_length <- rowMeans(
  cbind(aphid_traits$ant3_length_left,
        aphid_traits$ant3_length_right),
  na.rm = TRUE)

aphid_traits$tarsus_length <- rowMeans(
  cbind(aphid_traits$tarsus_length_left,
        aphid_traits$tarsus_length_right),
  na.rm = TRUE)

aphid_traits$femur_length <- rowMeans(
  cbind(aphid_traits$femur_length_left,
        aphid_traits$femur_length_right),
  na.rm = TRUE)

aphid_traits$tibia_length <- rowMeans(
  cbind(aphid_traits$tibia_length_left,
        aphid_traits$tibia_length_right),
  na.rm = TRUE)


#calculate fluctuating left-right asymetry 
aphid_traits$Rhinaria.asym <- rowSums(
  cbind(aphid_traits$Rhinaria_left,
        - aphid_traits$Rhinaria_right),
  na.rm = TRUE)

aphid_traits$ant3_length_asym <- rowSums(
  cbind(aphid_traits$ant3_length_left,
        - aphid_traits$ant3_length_right),
  na.rm = TRUE)

aphid_traits$tarsus_length_asym <- rowSums(
  cbind(aphid_traits$tarsus_length_left,
        - aphid_traits$tarsus_length_right),
  na.rm = TRUE)

aphid_traits$femur_length_asym <- rowSums(
  cbind(aphid_traits$femur_length_left,
        - aphid_traits$femur_length_right),
  na.rm = TRUE)

aphid_traits$tibia_length_asym <- rowSums(
  cbind(aphid_traits$tibia_length_left,
        - aphid_traits$tibia_length_right),
  na.rm = TRUE)

# Still some missing info to check!! ---> elena ?




# clean up ####

rm(i,ind, ind1, ind2, tmp,x)

