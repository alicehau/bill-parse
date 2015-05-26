# bill-parse
convertExtension.R: given a starting directory of .lob files, and another new directory to contain the converted
lob to .txt files, this program will convert .lob files to .txt files and store them in a given directory

cleanTxt.R: given a starting directory of the converted .lob to .txt files, will iterate over the files, parsing out key information such as the Bill ID/NO, chair, author(s), consultant, date of introduction, date of hearing, and date
of being amended (as they apply), and the supporters of and opposition to the bill
