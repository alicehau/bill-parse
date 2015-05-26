options(warn = 1)
startingDir <- "/Users/alicehau/Gifts_Research/gifts/2015"
txtDir <- "/Users/alicehau/Gifts_Research/gifts/2015/modified"
dir.create(txtDir, showWarnings = FALSE, recursive = FALSE)

filesToCopy <- list.files(startingDir, pattern = ".*\\.lob", full.names = TRUE)
file.copy(filesToCopy, txtDir)

filesToModify <- list.files(txtDir, pattern = ".*\\.lob", full.names = TRUE)
sapply(filesToModify,FUN=function(eachPath){
	file.rename(from=eachPath, to=sub(pattern=".lob", replacement=".txt",eachPath))
})


file.names <- dir(txtDir, pattern =".*\\.txt", full.names = TRUE)
eol <- "\n"
for(i in 1:length(file.names)){
	writeFile <- gsub(".txt", "_MODIFIED.txt", file.names[i], fixed = TRUE)
	write(eol, file = writeFile, append=TRUE)
	readFile <- file.names[i]
	readCon <- file(readFile, 'r')
	writeCon <- file(writeFile, 'w')
	line <- readLines(readCon, warn=F, skipNul = T)
	for ( i in 1:length(line)) {
		curr <- line[i]
		modified <- gsub("[^[:graph:]///' ]", "", curr)
	    removed <- gsub("MERGEFIELD  [a-z]*" , " ", modified, fixed = FALSE, ignore.case = FALSE)
	    mergeformatRemoved <- gsub("MERGEFORMAT" , " ", removed, fixed = TRUE)
	    backslashStarRemoved <- gsub("\\\\\\*", "", mergeformatRemoved, fixed=FALSE)
		writeLines(backslashStarRemoved, writeCon)
	}
	close(writeCon)
	close(readCon)
}

file.remove(file.names)
