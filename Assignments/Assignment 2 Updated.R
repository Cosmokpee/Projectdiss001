# Answer to Assignment 3 at June 17th 01:39 am
# From Dr. Danny Arrends Site 

setwd("C:/Users/Solom/OneDrive/Documents/Project Data/Assignment 3/")

# 1)
# Just a matrix
one <- read.csv("Lecture3.data1.txt", sep = "\t")
head(one)

two <- read.csv("Lecture3.data2.fasta", sep = "\n", header = FALSE, colClasses = "character")
fasta <- two[seq(2, nrow(two), 2),]           
# DNA Sequences
names(fasta) <- two[seq(1, nrow(two), 2),]    
# Names of the DNA Sequences
head(fasta)

# Just a matrix, column names are not loaded because of the #
three <- read.table("Lecture3.data3.vcf", col.names = c("CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", "5073"))

# Matrix with ; as separator
four <- read.csv("Lecture3.data4.txt", sep = ";", colClasses = "numeric")

# Just a matrix
five <- read.table("Lecture3.data5.vcf", na.string = c("NA", "."))

# Skip 17 lines at the beginning of the file
six <- read.csv("Lecture3.data6.csv", skip = 17, row.names = 1)

# Matrix with malformed names, use check.names = FALSE
seven <- read.csv("Lecture3.data7.txt", sep = "\t", row.names = 1, check.names = FALSE)

# Answer to Assignment 3 at June 17th 01:39 am
# From Dr. Danny Arrends Site 

setwd("C:/Users/Solom/OneDrive/Documents/Project Data/Assignment 3/")

# 1)
# Just a matrix
one <- read.csv("Lecture3.data1.txt", sep = "\t")
head(one)

two <- read.csv("Lecture3.data2.fasta", sep = "\n", header = FALSE, colClasses = "character")
fasta <- two[seq(2, nrow(two), 2),]           
# DNA Sequences
names(fasta) <- two[seq(1, nrow(two), 2),]    
# Names of the DNA Sequences
head(fasta)

# Just a matrix, column names are not loaded because of the #
three <- read.table("Lecture3.data3.vcf", col.names = c("CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", "5073"))

# Matrix with ; as separator
four <- read.csv("Lecture3.data4.txt", sep = ";", colClasses = "numeric")

# Just a matrix
five <- read.table("Lecture3.data5.vcf", na.string = c("NA", "."))

# Skip 17 lines at the beginning of the file
six <- read.csv("Lecture3.data6.csv", skip = 17, row.names = 1)

# Matrix with malformed names, use check.names = FALSE
seven <- read.csv("Lecture3.data7.txt", sep = "\t", row.names = 1, check.names = FALSE)

# 2a)
line.n <- 1
Tfile <- file("Lorem Ipsum.txt", "r")
# Get a pointer to the file
while (length((line = readLines(Tfile, n = 1))) > 0) {
  # Read a line, if available
  cat(line.n, "\n")
  # Number of words per line
  line.n <- line.n + 1
}
close(Tfile)

# 2b)
line.n <- 1
Tfile <- file("Lorem Ipsum.txt", "r")
while (length((line = readLines(Tfile, n = 1))) > 0) {
  amount <- lengths(strsplit(line, " ", fixed = TRUE))
  string <- sprintf("line %i contains %i words", line.n, amount)
  cat(string, "\n")
  line.n <- line.n + 1
}
close(Tfile)

# 2b) Reading a text file line by line
line.n <- 1
Tfile <- file("Lorem Ipsum.txt", "r")
# Get a pointer to the file
while (length((line = readLines(Tfile, n = 1))) > 0) {
  # Read a line, if available
  line.length <- length(strsplit(line, " ")[[1]])
  cat("line:", line.n, "length:", line.length, "\n")
  # Number of words per line
  line.n <- line.n + 1
}
close(Tfile)

# 3a)
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("biomaRt")

# 3b)
# library(biomaRt)
# listMarts()
#               biomart                version
# 1 ENSEMBL_MART_ENSEMBL      Ensembl Genes 109
# 2   ENSEMBL_MART_MOUSE      Mouse strains 109
# 3     ENSEMBL_MART_SNP  Ensembl Variation 109
# 4 ENSEMBL_MART_FUNCGEN Ensembl Regulation 109

# 3c)
snp.db <- useMart("ENSEMBL_MART_SNP")

snp.db <- useMart("ENSEMBL_MART_SNP", dataset = "mmusculus_snp")  # For mouse SNPs

# 3d)
snpID <- as.character(read.table("snpIDs.txt", colClasses = "character")[,1])

# 3e)
# Use biomart to retrieve locations and reference alleles
res.biomart <- getBM(c("refsnp_id", "allele", "chr_name", "chrom_start"), 
                     filters = "snp_filter", values = snpID, mart = snp.db)

# 4a)
set.seed(1)
# Big data to compute on, will take a long time
bigdata <- matrix(runif(10000000), 10000, 1000)

# 4b) This is where I want results to be stored:
cat("", file = "tmp.txt")  ## One time analysis, clear the output

# 4c) If the file exists, load it into R
temp <- tryCatch(
  read.table("tmp.txt", header = FALSE, sep = "\t", row.names = 1)   # Load from disk
  , error = function(e) return(matrix(0, 0, 0)))

# 4d)
for (x in max(1, nrow(temp) + 1):ncol(bigdata)) {  # Start where we were
  cors <- cor(bigdata[, x], bigdata, use = "pair")  # Do some correlations
  cat(paste(cors, sep = "\t"), "\n", file = "tmp.txt", append = TRUE)  # Save the correlation to the file
  cat("Done", x, "\n")  # inform the user how many we did
}

# 5a) Reading binary data from a BMP file
image.file <- "image2.bmp"

myimage.info <- file.info(image.file)
myimage.data <- readBin(image.file, n = as.numeric(myimage.info["size"]), what = "raw")

# 5b) Remove the header (54 bytes)
myimage.colordata <- myimage.data[-c(1:54)]

# 5b) Get the 3 different color components in the image
myimage.red <- matrix(as.numeric(myimage.colordata[seq(3, length(myimage.colordata), 3)]), 200, 200)
myimage.green <- matrix(as.numeric(myimage.colordata[seq(2, length(myimage.colordata), 3)]), 200, 200)
myimage.blue <- matrix(as.numeric(myimage.colordata[seq(1, length(myimage.colordata), 3)]), 200, 200)

# 5c) Image the red channel
image(myimage.red, col = topo.colors(255), yaxt = 'n', xaxt = 'n', ann = FALSE); box()


6) Recreate the image using pointillism
# 6a)
plot(c(1, 200), c(1, 200), t = 'n', yaxt = 'n', xaxt = 'n', ann = FALSE)

# 6b)
locations <- cbind(rep(1:200, 200), unlist(lapply(seq(1, 200), rep, 200)))
points(locations, cex = 0.2, pch = 15)

# 6c)
image.colors <- rgb(as.numeric(myimage.red), as.numeric(myimage.green), as.numeric(myimage.blue), maxColorValue = 255)
points(locations, col = image.colors, cex = 0.7, pch = 15)

some <- round(runif(20000, 1, 40000))
plot(c(1, 200), c(1, 200), t = 'n', yaxt = 'n', xaxt = 'n', ann = FALSE)
points(locations[some, ], col = image.colors[some], cex = 0.7, pch = 15)

plot(c(1, 200), c(1, 200), t = 'n', yaxt = 'n', xaxt = 'n', ann = FALSE)
for (x in 1:60000) {
  some <- round(runif(2, 1, 40000))
  points(locations[some, ], col = image.colors[some], cex = 0.7, pch = 15)
  # Sys.sleep(0.01)
}"
