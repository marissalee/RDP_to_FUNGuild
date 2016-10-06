#script_RDP_to_FUNGuild.R

library(tidyr)

# example df for FUNGuild input
#df.example <- read.table("otu_table_example.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# current df from RDP output
df.rdpout <- read.table("data/RDPUnite.txt", sep = "\t", stringsAsFactors = FALSE)

# current otu table
df.otutab <- read.table("data/otuTable.tab", sep = "\t", header = TRUE, stringsAsFactors = FALSE)


################

# taxonomy cell template
#df.example[1,"taxonomy"]



################

# reformat df.rdpout

#pull out relevant columns and given them column names
df.tmp <- df.rdpout[, c("V1","V3","V6","V9","V12","V15","V18","V21","V23")]
colnames(df.tmp) <- c("OTUId","kingdom","phylum","class","order","family","genus","genus_species_code","speciesPerc")

#split the column with multiple types of info
df.tmp1 <- separate(data = df.tmp, col = genus_species_code, into = c("genus_species", "shcode"), sep = "\\|")
df.tmp2 <- separate(data = df.tmp1, col = genus_species, into = c("Genus","Species"), sep = "_", extra = "merge")
df.tmp3<-data.frame(df.tmp2, genus_species=df.tmp1$genus_species)

#replace columns that include "unidentified with just unidentified"
i<-0
COLS <- c("kingdom","phylum","class","order","family","Genus","Species")
for (i in 1:length(COLS)){
  #pull out the current column
  columnThing <- df.tmp3[,COLS[i]]
  #find "unidentified"s and replace cell with simple "unidentified"
  columnThing[grepl("unidentified", columnThing)] <- "unidentified"
  #update the column in the original dataframe
  df.tmp3[,COLS[i]] <- columnThing
}


################

# pull stuff together and make a 'taxonomy' column
data<-df.tmp3 #call this something generic

perc <- data$speciesPerc
genus_species <- data$genus_species
eucode <- "EU"
shcode <- data$shcode
repinfo <- "reps"
kingdom <- data$kingdom
phylum <- data$phylum
class <- data$class
order <- data$order
family <- data$family
genus <- data$Genus
species <- data$Species

data[,"taxonomy"]<-paste(perc, 
                         genus_species, eucode, shcode, repinfo,
                         paste(paste("k__",kingdom, sep =""),
                               paste("p__",phylum, sep =""),
                               paste("c__",class, sep =""),
                               paste("o__",order, sep =""),
                               paste("f__",family, sep =""),
                               paste("g__",genus, sep =""),
                               paste("s__",species, " ...", sep =""), sep = ";"), 
                         sep = "|")

#check the format
#data[1,"taxonomy"]


################

# attach the 'taxonomy' column to the otu table
ind.tax <- data[,c("OTUId","taxonomy")]
df.otu.tax <- merge(df.otutab, ind.tax)


################
write.table(df.otu.tax, file="output/otuTable_forFUNG.tab", sep = "\t")

df.test<-read.table("output/otuTable_forFUNG.tab")
colnames(df.test)[colnames(df.test) == "taxonomy"]
