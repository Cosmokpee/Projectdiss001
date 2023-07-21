#
##
###Solomon
####
##### Data Analysis part 8
setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

# Loading in the different files
phenotypes <- read.table ("ind.sorted.phe.txt")
map <- read.table ("map.sorted.txt")

map <- cbind(map, PosName = paste(map[, "Chr"], map [, "Position"],sep="_"))

haplo <- read.table("pHaplo.txt",  sep = "\t")
haplo[1:10,1:10]

haplo <- read.table("pHaplo.txt",  sep = "\t", na.strings = "", colClasses ="character")
haplo[1:10,1:10]


for(x in 1:nrow(haplo)){ 
	mname <- rownames(haplo)[x]
	if(x != nrow(haplo)){
		mnext <- rownames(haplo) [x+1]
		map[mname,]
		map[mnext,]
	}
}
map[mname,]
map[mnext,]


# Merging 2 markers into 1 if they are in the same Position
haploNDM <- c()

for(x in  1:nrow(haplo)){ 
	mname <- rownames(haplo)[x]
	if(x != nrow(haplo)){
		mnext <- rownames(haplo)[x+1]
		if(map[mname, "PosName"] != map[mnext, "PosName"]){
			haploNDM <- rbind(haploNDM, haplo[x,])
			}
		}
	cat("Scanning",x,"\n")
}


# Merging 2 markers into 1 if they are in the same Position and  skipping to the next marker after merge
haploNDM <- c()
x <- 1
while(x < nrow(haplo)){ 
	mname <- rownames(haplo)[x]
	if(x != nrow(haplo)){
		mnext <- rownames(haplo)[x+1]
		if(map[mname, "PosName"] != map[mnext, "PosName"]){
			haploNDM <- rbind(haploNDM, haplo[x,])
			x <- x + 1
		}else{
			# we know x and x+1 are the same markers
			newM <- haplo[x,]
			iix <- which (is.na(newM))
			newM [iix] <- haplo[x + 1,iix]
			haploNDM <- rbind(haploNDM, newM)
			x <- x+ 2
		}
	}
	cat("Scanning",x,"\n")
}
 
#Picking the last unique marker 
haploNDM <- c()
x <- 1
while(x <= nrow(haplo)){ 
	mname <- rownames(haplo)[x]
	if(x != nrow(haplo)){
		mnext <- rownames(haplo)[x+1]
		if(map[mname, "PosName"] != map[mnext, "PosName"]){
			haploNDM <- rbind(haploNDM, haplo[x,])
			x <- x + 1
		}else{
			# we know x and x+1 are the same markers
			newM <- haplo[x,]
			iix <- which (is.na(newM))
			newM [iix] <- haplo[x + 1,iix]
			haploNDM <- rbind(haploNDM, newM)
			x <- x+ 2
		}
	}else{
		haploNDM <- rbind(haploNDM, haplo[x,])
			x <- x + 1
		}
	cat("Scanning",x,"\n")
}


write.table(haploNDM, "haplotypes_uniques.txt", sep = "\t", quote=FALSE, na = "")

