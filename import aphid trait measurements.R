# Import aphid trait data
Sys.setlocale("LC_ALL", "de_DE.UTF-8")
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
aphid_traits$side = str_extract(aphid_traits$Trait, pattern = c("left", "right"))

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
tmp <- doBy::summaryBy(formula = Length ~ ID_plot + Colony + Individual + Trait.type + Trait + side + Part,
                id = ~ collector + date_collected + PhotoName,
                data = aphid_traits,
                FUN = mean,
                keep.names = TRUE)

# Sum part A and B when useful 
tmp <- doBy::summaryBy(formula = Length ~ ID_plot + Colony + Individual + Trait.type + Trait + side,
                       id = ~ collector + date_collected + PhotoName,
                       data = tmp,
                       FUN = sum,
                       keep.names = TRUE)


# check out data quickly
boxplot(Length ~ ID_plot:collector,
        data = tmp[tmp$Trait.type=="body_width",], las = 2)

# import and reformat metadata on photo magnification and scale ####

## import and reformat excel data
## re-format photo data to match unique photo index

# merge tables by photo index ####

# Import conversion table ####

# Convert length measurements in mm ####

# clean up columns
