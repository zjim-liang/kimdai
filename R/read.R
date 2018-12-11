#' @import readxl
library(readxl)

#' XLSX2Lines() reads the given xlsx file and returns a vector of lines.
XLSX2Lines <- function(file = NULL, delimiter = "\t") {
    XLSX.EXTS <- casefold(c("xlsx"))
    placeholders <- "X__\\d+"
    if (casefold(tools::file_ext(file)) %in% XLSX.EXTS) {
        m <- as.matrix(read_excel(file))  # read_excel() returns a tibble object
        m[is.na(m)] <- character(1)
        col <- colnames(m)
        col[grepl(placeholders, col)] <- character(1)  # Replace "X__1", "X__2", ... to ""
        Lines <- paste0(col, collapse = delimiter)
        for (i in 1:nrow(m)) {
            Lines[i + 1] <- paste0(m[i, ], collapse = delimiter)
        }
        Lines
    } else {
        stop("The input must be a string of the file path ",
             "with the extensions: ", paste0(XLSX.EXTS, collapse = ", "), "!")
    }
}

#' ReadLines() reads the input file or text and returns a vector of lines.
#' The param "delimiter" is for XLSX2Lines().
ReadLines <- function(file = NULL, text = NULL, delimiter = "\t",
                      func.name = "ReadLines()") {
    if (length(file) > 1) {
        stop("More than one file was given for '", func.name, "'!")
    } else if (length(file)) {
        if (casefold(tools::file_ext(file)) %in% XLSX.EXTS) {
            XLSX2Lines(file, delimiter)
        } else {
            readLines(file)
        }
    } else if (length(text)) {
        unlist(strsplit(text, "\n", fixed = TRUE))
    } else {
        stop("No input file or text were found for '", func.name, "'!")
    }
}

#' ReadText() reads the input file or text and returns a string of text.
#' The function wraps ReadLines() around and
#' add a "\n" as the EOF for the output text.
#' The param "delimiter" is for XLSX2Lines().
ReadText <- function(file = NULL, text = NULL, delimiter = "\t",
                     func.name = "ReadText()") {
    Lines <- ReadLines(file, text, delimiter, func.name)
    paste0(paste0(Lines, collapse = "\n"), "\n")
}

#' ReadTable() reads the input file or text and returns a data frame.
ReadTable <- function(file = NULL, text = NULL, header = FALSE, delimiter = "\t",
        check.names = FALSE, comment.char = "", func.name = "ReadTable()") {
    text <- ReadText(file, text, delimiter, func.name)
    read.table(text = text, stringsAsFactors = FALSE, header = header, sep = delimiter,
               check.names = check.names, comment.char = comment.char, quote = "\"")
}

