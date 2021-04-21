# Import aphid trait data

# Length measurements from image analyses ####

## import and assemble files

# get all paths to text files
list_of_files = list.files(path = "data/imageJ results/",
                           recursive = TRUE,
                           pattern = "\\.txt$", 
                           full.names = TRUE)

# read and bind all text files
df_results <-   purrr::map_dfr(.f = data.table::fread,
                              .x = list_of_files,
                              data.table = FALSE)

# Change to latin-1 encoding to deal with german special characters
Encoding(df_results$Label) <- "latin1"

## extract informatino from label
aphid_traits = df_results %>%
  separate(Label,
           into = c("PhotoName", "TraitLabel"),
           sep = ":",
           remove = FALSE)

# import plot name conversion table:
plot_names <- fread("data/table of spotnames_maud.csv", data.table = FALSE)

# Add new columns for ID_plot, collector and date of collection
for (i in unique(plot_names$`name of image`)){
  ind <- grep(pattern = i, x = aphid_traits$PhotoName)
  aphid_traits[ind, "ID_plot"] <- 
    plot_names[plot_names$`name of image` == i, "ID_plot"]
  aphid_traits[ind, "collector"] <- 
    plot_names[plot_names$`name of image` == i, "collector"]
  aphid_traits[ind, "date_collected"] <- 
    plot_names[plot_names$`name of image` == i, "date collected"]
  aphid_traits[ind, "colony_judith"] <- 
    plot_names[plot_names$`name of image` == i, "Colony"]
}

# Divide the photo name into 5 columns:
aphid_traits <- aphid_traits %>%
  separate(PhotoName,
           into = c("PlotA", "PlotB",
                    "ColonyNumber","Individual_letter", "PhotoType"),
           fill = "warn",
           sep = "_",
           remove = FALSE
  )

# Correct columns for Sudkreuz plot
# replace with the correct columns:
aphid_traits[which(aphid_traits$ID_plot == "Sk_01"),
             c("ColonyNumber","Individual_letter", "PhotoType")] <-
  aphid_traits[which(aphid_traits$ID_plot == "Sk_01"),
               c("PlotB", "ColonyNumber","Individual_letter")]

# Correct columns for the judith samples, 
# since they have no colony number :
aphid_traits[aphid_traits$collector == "Judith",
             c("Individual_letter", "PhotoType")] <-
  aphid_traits[aphid_traits$collector == "Judith",
               c("ColonyNumber","Individual_letter")]

# replace colony number by identifier f
aphid_traits[aphid_traits$collector == "Judith","ColonyNumber"] <- aphid_traits[aphid_traits$collector == "Judith","colony_judith"]

# Create a unique identifier for each individual aphid:
aphid_traits$Colony <- paste(aphid_traits$ID_plot, 
                                 aphid_traits$ColonyNumber,
                                 sep = "-")


# Create a unique identifier for each individual aphid:
aphid_traits$Individual <- paste(aphid_traits$Colony,
                                 aphid_traits$Individual_letter,
                                 sep = "-")



# Extract name of trait and "part" = when trait measure was cut into two parts
aphid_traits <- aphid_traits %>%
  separate(TraitLabel, into = c("Trait","Part"),
           sep = "_part", remove = FALSE)

# extract "Rep": the replication number of each trait measurement
# (should be three unique numbers per trait)
aphid_traits$Rep = str_extract(aphid_traits$Trait, "\\d+")

# Extract "side": when appropriate, left or right measurement
aphid_traits$side <- NA
aphid_traits$side[grep( "left",aphid_traits$Trait)] <- "left"
aphid_traits$side[grep( "right",aphid_traits$Trait)] <- "right"


# extract "Trait.type": the generic type of trait (e.g. "antenna length")
# without the information about left or right
aphid_traits$Trait.type = sapply(aphid_traits$Trait, function(x) {
  paste(str_split(x,pattern = "_",simplify = TRUE)[1:2], collapse = "_")
})

# extract "Trait": the full trait name including "left" or "right"
aphid_traits$Trait = sapply(aphid_traits$Trait, function(x) {
  y = str_split(x,pattern = "_",simplify = TRUE)
  return(paste(y[1:length(y)-1], collapse = "_"))
})

# Check if trait names make sense:
sort(unique(aphid_traits$Trait))
# replace "flag" by ANT3 for the third antenna segment measured?
aphid_traits$Trait <- str_replace_all(aphid_traits$Trait,
                pattern = "flag",
                replacement = "ant3")

aphid_traits$Trait.type <- str_replace_all(aphid_traits$Trait.type,
                pattern = "flag",
                replacement = "ant3")

# Clean up unnecessary columns:
aphid_traits <- aphid_traits[, -which(colnames(aphid_traits) %in% c( "V1", "colony_judith","PlotA", "PlotB"))] 

# Calculate mean across 3 replicate measures ####
aphid_traits <- aphid_traits %>% 
  group_by(collector, date_collected, PhotoName, ID_plot,Colony,Individual, Trait.type,Trait, side, Part) %>%
summarise(count.reps = n(),
            Length.pix.mean = mean(Length))

# sum parts 1 and 2
aphid_traits <- aphid_traits %>%
  group_by(collector, date_collected, PhotoName, ID_plot,Colony,Individual, Trait.type,Trait, side, count.reps) %>%
  summarise(count.parts = n(),
            Length.pix.mean = sum(Length.pix.mean))

# import and reformat metadata on photo magnification and scale ####

## import and reformat excel data
library(rio)
data_list <- rio::import_list("data/Checklist 2020_maud edits.xlsx",
                         setclass = "data.table")
# Sheet 17 is empty ?
data_list <- data_list[-17]

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

# Add unique photo index to match to aphid_traits table
magnif_df$PhotoName <- apply(
  as.data.frame(magnif_df[, c("NamePlotPhoto", "colony","individual","PhotoType")]),
  MARGIN = 1, 
  FUN = paste, collapse = "_")

magnif_df$PhotoName <- gsub(pattern = "___", replacement = "_", magnif_df$PhotoName)
magnif_df$PhotoName <- gsub(pattern = "__", replacement = "_", magnif_df$PhotoName)
magnif_df$PhotoName <- paste(magnif_df$PhotoName, ".jpg", sep = "")

## MISSING: 16 Photos which should have been measured (according to the checklist) but do not appear in aphid_traits:
missing_measurements <- magnif_df[which(
  !(magnif_df$PhotoName %in% aphid_traits$PhotoName) &
    !is.na(magnif_df$Magnification) &
    (magnif_df$Magnification!= "NA")),]
# e.g. "Ol_55_1_B_left leg" and "Ol_55_1_B_right leg" 
# do not appear because the pictures had a different name:
# "Ol_55_1_B_legs L&R"
# which was not automatically selectionned by the aphid trait plugin...

# merge tables by photo index ####

aphid_traits = merge( aphid_traits,
       magnif_df[,c("PhotoName", "Magnification")] ,
       by = "PhotoName",
       all.x = TRUE)

# Import conversion table ####
magnif_conversion <- fread("data/calibration 2020_maud edits.csv")

# Convert length measurements in mm ####

aphid_traits$Length.mm <- aphid_traits$Length.pix.mean/
  magnif_conversion$mean.pixel.per.mm[match(
    as.numeric(aphid_traits$Magnification),
    magnif_conversion$magnification)]

# clean up columns


#>>>>>>> still some weird stuff with Judith's indiviual names
