options(warn = 1)
txtDir <- "/Users/alicehau/Gifts_Research/gifts/2015/modified"
file.names <- dir(txtDir, pattern =".*\\.txt", full.names = TRUE)
bill_id <- character()
date_introduced <- character()
date_hearing <- character()
date_amended <- character()
author <- character()
consultant <- character()
opposition <- character()
support <- character()
chair <- character()
file <- character()
numRows = 1
# file.names <- "/Users/alicehau/Gifts_Research/gifts/2015/modified/BILL_ANALYSIS_TBL_1524_MODIFIED.txt"
for(i in 1:length(file.names)){
  # for(i in 100:200){

    readFile <- file.names[i]
    cat(file.names[i])
    cat("\n")

    dateIntroduced <- ""
    dateAmended <- ""
    dateHearing <- ""

    singleString <- readChar(readFile, file.info(readFile)$size)

  #get the bill id
  if (grepl("Bill No:\\s*(_type)*\\s*[A-Z]*\\s*(_num)*\\s*[0-9]*", singleString, fixed=FALSE)) {
      billID <- sub(".*?(Bill No:\\s*(_type)*\\s*[A-Z]*\\s*(_num)*\\s*[0-9]*).*$", "\\1", singleString, fixed=FALSE)
      billID <- sub("Bill No:", "", billID)
      billID <- sub("_type", "", billID)
      billID <- sub("_num", "", billID)
      billID <- gsub("\\s", "", billID, fixed= FALSE)
  } else if (grepl("Bill Id:\\s*[A-z]*\\s*[0-9]*", singleString, fixed=FALSE)) {
    billID <- sub(".*?(Bill Id:\\s*[A-z]* [0-9]*).*$", "\\1", singleString, fixed=FALSE)
    billID <- sub("Bill Id:", "", billID)
    billID <- gsub("\\s", "", billID, fixed= FALSE)
  } else if (grepl(" [A-z]*\\s*[0-9]* ", singleString, fixed= FALSE)) {
    billID <- sub(".*?( [A-z]{2,3}\\s*[0-9]{2,3}).*$", "\\1", singleString, fixed=FALSE)
    billID <- gsub("\\s", "", billID, fixed= FALSE)
  }


  #get the date the bill was introduced if it's in the bill
  if (grepl(".*?Introduced( Ver)?:?\\s*((January|February|March|April|June|July|August|September|October|November|December) \\d{0,2}, \\d{4})", singleString, fixed=FALSE) ) {
    dateIntroduced <- sub(".*?Introduced( Ver)?:?\\s*((January|February|March|April|June|July|August|September|October|November|December) \\d{0,2}, \\d{4})", "\\2", singleString, fixed=FALSE)
    dateIntroduced <- sub("(\\d{4})(.*)", "\\1", dateIntroduced) 

  } else if (grepl(".*?Introduced( Ver)?:?\\s*(\\d{0,2}/\\d{0,2}/\\d{2,4})", singleString)) {
    dateIntroduced <- sub(".*?Introduced( Ver)?:?\\s*(\\d{0,2}/\\d{0,2}/\\d{2,4}).*$", "\\2", singleString, fixed=FALSE)
  }

  if (grepl("[0-9]+", dateIntroduced, fixed=FALSE)) {
    date_introduced[numRows] = dateIntroduced
    bill_id[numRows] = billID
    chair[numRows] = "";
    author[numRows] = "";
    support[numRows] = "";
    opposition[numRows]= ""
    date_amended[numRows] = "";
    date_hearing[numRows] = ""
    consultant[numRows] = ""

    file[numRows] = file.names[i]
    numRows = numRows + 1
  }

  if (grepl(".*?Hearing.*?(((January|February|March|April|June|July|August|September|October|November|December) \\d{0,2}, \\d{4})|(\\d{0,2}/\\d{0,2}/\\d{2,4}))", singleString, fixed=FALSE)) {
    dateHearing <- gsub(".*?Hearing.*?(((January|February|March|April|June|July|August|September|October|November|December) \\d{0,2}, \\d{4})|(\\d{0,2}/\\d{0,2}/\\d{2,4}))", "\\1", singleString)
    dateHearing <- sub("(\\d{1,2}/\\d{2,4}|\\d{1,2}, \\d{4})(.*)", "\\1", dateHearing)
    if (grepl("[0-9]+", dateHearing, fixed=FALSE)) {
      date_hearing[numRows] = dateHearing
      bill_id[numRows] = billID
      chair[numRows] = "";
      author[numRows] = "";
      support[numRows] = "";
      opposition[numRows]= ""
      date_amended[numRows] = "";
      date_introduced[numRows] = ""
      consultant[numRows] = ""

      file[numRows] = file.names[i]
      numRows = numRows + 1
    }
  }


  if (grepl(".*?Amended.*?((January|February|March|April|June|July|August|September|October|November|December) \\d{0,2}, \\d{4})", singleString, fixed=FALSE) ) {
      dateAmended <- sub(".*?Amended.*?((January|February|March|April|June|July|August|September|October|November|December) \\d{0,2}, \\d{4})", "\\1", singleString, fixed=FALSE)
      dateAmended <- sub("(\\d{4})(.*)", "\\1", dateAmended)
    } else if (grepl(".*?Amended.*?(\\d{0,2}/\\d{0,2}/\\d{2,4})", singleString, fixed= FALSE)) {
      dateAmended <- sub(".*?Amended.*?(\\d{0,2}/\\d{0,2}/\\d{2,4})", "\\1", singleString, fixed=FALSE)
      dateAmended <- sub(" .*", "", dateAmended)
    } else {
      dateAmended = ""
    }
    date_amended[numRows] = dateAmended
    bill_id[numRows] = billID
    chair[numRows] = "";
    author[numRows] = "";
    support[numRows] = "";
    opposition[numRows]= ""
    date_hearing[numRows] = "";
    date_introduced[numRows] = ""
    consultant[numRows] = ""
    file[numRows] = file.names[i]




  #get the name of the chair
  if (grepl("[A-z],.*?Chair", singleString, fixed=FALSE)) {
    chairName <- sub(".*?(([A-Z]'*[A-z]*-*[A-z]* {0,1}[A-z]*(, )*)*,{0,1}\\s*Chair).*$", "\\1", singleString, fixed = FALSE)
    chairName <- sub("Chair", "", chairName)
    chairName <- sub("_full_name", "", chairName)
    chairName <- sub("^[^a-zA-Z]+", "", chairName)
    chairName <- sub("[ \t]+$", "", chairName)
    chairName <- sub("\n", "", chairName)
    chairName <- sub(",", "", chairName)
    chairName <- sub("Senator\\s*", "", chairName, fixed = FALSE)

    if (grepl("[A-z]+", chairName, fixed=FALSE)) {
      chair[numRows] = chairName
      bill_id[numRows] = billID
      date_introduced[numRows] = "";
      date_amended[numRows] = "";
      date_hearing[numRows] = ""
      author[numRows] = "";
      support[numRows] = "";
      opposition[numRows]= ""
      consultant[numRows] = ""

      file[numRows] = file.names[i]
      numRows = numRows + 1
    }
  }


  if (grepl("Author:*\\s*[A-z]*", singleString, fixed=FALSE)) {
    authorName <- sub(".*?(Author:*\\s*\\(*\\s*(\\s*([A-Z]'*[A-z]*-*[A-z]* {0,1}[A-z]*(, )*)*(\\((D|R)\\),)*)*(and [A-Z]'*[A-z]*-*[A-z]* {0,1}[A-z]* \\((D|R)\\))*).*", "\\1", singleString, fixed=FALSE)
    authorName <- sub("Author:\\s*", "", authorName, fixed= FALSE)
    authorName <- sub(":", "", authorName, fixed= FALSE)
    
    if (grepl(",", authorName)) {
      authorName <-  sub("(.*\\((D|R)\\)).*", "\\1", authorName, fixed=FALSE)
      authorName <- gsub("\\((D|R)\\)", "", authorName, fixed=FALSE)
      authorName <- gsub("and", "", authorName)
      splitByCommas <- strsplit(authorName, ",")
      authorName <- unlist(splitByCommas)
      authorName <- sub("^[^a-zA-Z]+", "", authorName)
      authorName <- sub("(([A-Z]'*[A-z]*-*[A-z]* {0,1}[A-z]*)*).*$", "\\1", authorName)
      authorName <- sub("[ \t]+$", "", authorName)
      authorName <- sub("Version", "", authorName)
      for (j in 1:length(authorName)) {
        author[numRows] = authorName[j]
        bill_id[numRows] = billID
        date_introduced[numRows] = "";
        chair[numRows] = "";
        support[numRows] = "";
        opposition[numRows]= ""
        date_amended[numRows] = "";
        date_hearing[numRows] = ""
        consultant[numRows] = ""
        file[numRows] = file.names[i]
        numRows = numRows + 1
      }
    } else {
      authorName <- sub("^[^a-zA-Z]+", "", authorName)
      authorName <- sub("(([A-Z]'*[A-z]*-*[A-z]* {0,1}[A-z]*)*).*$", "\\1", authorName)
      authorName <- sub("[ \t]+$", "", authorName)
      authorName <- sub("Version", "", authorName)
      author[numRows] = authorName
      bill_id[numRows] = billID
      date_introduced[numRows] = "";
      chair[numRows] = "";
      support[numRows] = "";
      opposition[numRows]= ""
      date_amended[numRows] = "";
      date_hearing[numRows] = ""
      consultant[numRows] = ""
      file[numRows] = file.names[i]
      numRows = numRows + 1
    }
  }


  #get consultant name
  if (grepl("(Consultant|CONSULTANT):\\s*(_by_name)*\\s*[A-z]*\\s[A-z]*", singleString, fixed=FALSE)) {
    consultantName <- sub(".*?((Consultant|CONSULTANT):\\s*(_by_name)*\\s*([A-Z][a-z]*( {0,1}[A-Z][a-z]*)*)).*$", "\\1", singleString, fixed=FALSE)
    consultantName <- sub("(Consultant|CONSULTANT):\\s*(_by_name)*\\s*", "", consultantName, fixed= FALSE)
    consultantName <- sub("([A-Z][a-z]* ( {0,1}[A-Z][a-z]*)*).*$", "\\1", consultantName)
    consultantName <- sub("[ \t]+$", "", consultantName)
    if (grepl("[A-z]+", consultantName, fixed=FALSE)) {
      consultant[numRows] = consultantName
      bill_id[numRows] = billID
      date_introduced[numRows] = ""
      author[numRows] = ""
      chair[numRows] = ""
      support[numRows] = ""
      opposition[numRows]= ""
      date_amended[numRows] = ""
      date_hearing[numRows] = ""
      file[numRows] = file.names[i]
      numRows = numRows + 1
    }
  }




  if (grepl("(\n(Support|Registered support):*\n(.*\n)*.*(Opposition|Registered opposition))", singleString, ignore.case = TRUE)) {
    unparsedSupport <- gsub(".*?(\n(Support|Registered support):*\n(.*\n)*.*(Opposition|Registered opposition)).*","\\1",singleString, ignore.case = TRUE)
    unparsedSupport <- gsub("(Support|Opposition|Registered support|Registered opposition)", "", unparsedSupport, ignore.case = TRUE)
    # cat(unparsedSupport)
    splitByLineSupport <- strsplit(unparsedSupport, "\n")
    splitByLineSupport <- unlist(splitByLineSupport)


    numSupporters <- length(splitByLineSupport)

    for (j in 1:numSupporters) {
      if (grepl("^[A-Z][-'A-z]+,?((\\s\\(?&?\\s?[A-z][-'A-z]{0,19},?)*.?,?)*$", splitByLineSupport[j], fixed=FALSE)) {
        nextSupport <- splitByLineSupport[j]
        nextSupport <- sub("^[^a-zA-Z]+", "", nextSupport)
        nextSupport <- sub("[ \t]+$", "", nextSupport)
        support[numRows] <- nextSupport
        bill_id[numRows] = billID
        date_introduced[numRows] = "";
        chair[numRows] = ""
        author[numRows] = "";
            date_amended[numRows] = "";
    date_hearing[numRows] = ""
        opposition[numRows]= ""
            consultant[numRows] = "";
          file[numRows] = file.names[i]
        numRows = numRows + 1
      }

    }

  }




  # print(singleString)
  if (grepl("Opposition:*\n*([A-z].*\n)*(Analysis|-- END --)*", singleString, ignore.case = TRUE)) {
    unparsedOpposition <- sub(".*?(\nOpposition:*\n*([A-z].*\n)*(Analysis)*).*","\\1",singleString, ignore.case =TRUE)
    unparsedOpposition <- gsub("(Support|Opposition|Registered support|Registered opposition|analysis)", "", unparsedOpposition, ignore.case = TRUE)
    splitByLineOpposition <- strsplit(unparsedOpposition, "\n")
    splitByLineOpposition <- unlist(splitByLineOpposition)
  # print(splitByLineOpposition)
  # # cat(opposition)



  # numOpposers <- length(splitByLineOpposition)
  #   for (i in 1:numOpposers) {
  #     if (grepl("^[A-Z][-'A-z]+,?(\\s[A-z][-'A-z]{0,19},?)*.?", splitByLineOpposition[i], fixed=FALSE)) {

  #       nextOpposition <- splitByLineOpposition[i]
  #       # opposition[length(billOpposition)+1] <-  nextOpposition
  #       # support[length(billSupport)+1] <- ""
  #           opposition[numRows] <-  nextOpposition
  #       bill_id[numRows] = billID
  #         date_introduced[numRows] = ""
  #             date_amended[numRows] = ""
  #     date_hearing[numRows] = ""
  #         chair[numRows] = ""
  #         author[numRows] = ""
  #         support[numRows]= ""
  #         consultant[numRows] = ""
  #           file[numRows] = file.names[i]
  #         numRows = numRows + 1
  #     }  
  #   }
    # print(opposition)



  }
}

frame <- data.frame(file, bill_id, date_introduced, date_hearing, date_amended, chair, author, consultant, support, opposition, stringsAsFactors=FALSE)
write.csv(frame, "/Users/alicehau/bill-parse/support.csv")

