PurPL<-read.csv("./FieldMethods/Data/PL_complete.csv")
Lythrum17<-read.csv("./FieldMethods/Data/Lythrum2017FieldData.csv")

# Combine columns with the paste() function
paste(PurPL$Block,PurPL$ID,sep="-") # Sep is the separator string

# Rename 'Block' column to 'BLOCK' in PurPL dataset 
# So that they both have the same column names for merging
names(PurPL)[4]<-"BLOCK"
names(PurPL)

# Add a merging column called "BL_ID" to each dataset
# using the paste() function on each dataset
PurPL$BL_ID<-paste(PurPL$BLOCK,PurPL$ID,sep="_")
Lythrum17$BL_ID<-paste(Lythrum17$BLOCK,Lythrum17$ID,sep="_")

# Merge datasets with the merge() function
Ls17comp<-merge(PurPL,Lythrum17,by="BL_ID")

# Notice the # observations in the 'Environment' tab
# shows that some rows are missing

# Check for duplicate codes
PurPL[duplicated(PurPL$BL_ID),]
sum(duplicated(PurPL$BL_ID))

Lythrum17[duplicated(Lythrum17$BL_ID),]

# No duplicates in either dataset
# Check for unique values
BL_ID<-c(PurPL$BL_ID,Lythrum17$BL_ID)
# Sort BL_ID using the order() function
BL_ID<-BL_ID[order(BL_ID)]

Uniq<-BL_ID[!duplicated(BL_ID)]
Uniq[grep("E5",Uniq)]

# Use Uniq vector to examine datasets
Lythrum17Missing<-PurPL[PurPL$BL_ID %in% Uniq,]
PurPLMissing<-Lythrum17[Lythrum17$BL_ID %in% Uniq,]

# Merge while keeping missing data
MergedLsData<-merge(PurPL,Lythrum17,by="BL_ID",all=TRUE)

# Check for missing data using BLOCK.x and BLOCK.y
MergedLsData[is.na(MergedLsData$BLOCK.x),]
MergedLsData[is.na(MergedLsData$BLOCK.y),]

# Merge and remove missing data
MergedLsDataBoth<-merge(PurPL,Lythrum17,by="BL_ID",all=FALSE)

write.csv(MergedLsDataBoth,"./FieldMethods/Data/Lythrum2017-2018.csv",row.names=FALSE)


