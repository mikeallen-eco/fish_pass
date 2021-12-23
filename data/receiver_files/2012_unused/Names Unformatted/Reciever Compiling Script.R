setwd ('/Users/OrionWeldon/Documents/Shad Project/Data/Receiver Data/2013/Receiver Files')

outfile.name <- "/Users/OrionWeldon/Documents/Shad Project/Data/Receiver Data/2013/Combined Data/CombinedFiles" #Creating a name and path just for the outfile

list.files ()  #Listing the directories within this
Files <- list.files ()  #Creating the directory vector named "Files"
Files<- Files[-1] # We want to skip the first two (and the ".")

for (i in 1:length (Files)) {
  temp.files <- read.table (Files[i], header = F, sep = "", quote = "")
  CombinedFiles <- cbind (temp.files)
}

write.table (CombindedFiles)