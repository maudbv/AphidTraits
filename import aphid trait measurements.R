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

# Divide the photo name into 5 columns
aphid_traits <- aphid_traits %>%
  separate(PhotoName,
           into = c("PlotA", "PlotB",
                    "ColonyNumber","Individual", "PhotoType"),
           fill = "warn",
           sep = "_",
           remove = FALSE
           )

# Correct columns for the judith samples, 
# since they have no colony number :

# identify judith's samples with problematic columns:
aphid_traits$judiths <- 0
aphid_traits$judiths[which(is.na(aphid_traits$PhotoType))] <- 1

# replace with the correct columns:
aphid_traits[aphid_traits$judiths ==1,
             c("Individual", "PhotoType")] <-
aphid_traits[aphid_traits$judiths ==1,
             c("ColonyNumber","Individual")]

# replace colony number by a generic value of 1 :
aphid_traits[aphid_traits$judiths ==1,"ColonyNumber"] <- 1


# Paste together the two sides of the plot name which were divided by "_"
aphid_traits <- unite(data = aphid_traits, PlotA:PlotB,
                      col = "Plot_ID",
                      sep = "_",
                      remove = TRUE, na.rm = FALSE)

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
      
# import and reformat metadata on photo magnification and scale ####

## import and reformat excel data
## re-format photo data to match unique photo index

# merge tables by photo index ####

# Import conversion table ####

# Convert length measurements in mm ####

# clean up columns
