#' @import stringr

library(stringr)

#' Add colors and styles for printing a vector of objects.
#' The function returns a vector of character strings,
#' cyan-colored normal-style strings by default.
AddColor <- function(x, color = "cyan", style = "default")
{
    codes <- c(
        RESET_ALL   = '\x1b[0m',
        BOLD        = '\x1b[1m',
        DIM         = '\x1b[2m',
        ITALIC      = '\x1b[3m',
        UNDERSCORED = '\x1b[4m',
        BACKGROUND  = '\x1b[7m',
        NORMAL      = '\x1b[22m',
        BLACK       = '\x1b[30m',
        RED         = '\x1b[31m',
        GREEN       = '\x1b[32m',
        YELLOW      = '\x1b[33m',
        BLUE        = '\x1b[34m',
        MAGENTA     = '\x1b[35m',
        CYAN        = '\x1b[36m',
        WHITE       = '\x1b[37m',
        RESET_COLOR = '\x1b[39m',
        DEFAULT     = character(1)
        )
    color.code <- codes[toupper(color)]
    if (any(is.na(color.code))) {
        WriteLog("Invalid spellings in the given colors!",
                 func.name = match.call()[[1]], level = "warn")
        color.code[is.na(color.code)] <- character(1)
    }
    if (length(color.code) > length(x)) {
        color.code <- color.code[1:length(x)]
    }
    style.code <- codes[toupper(style)]
    if (any(is.na(style.code))) {
        WriteLog("Invalid spellings in the given styles!",
                 func.name = match.call()[[1]], level = "warn")
        style.code[is.na(style.code)] <- character(1)
    }
    if (length(style.code) != length(x)) {
        style.code <- paste0(style.code, collapse = character(1))
    }
    paste0(color.code, style.code, x, codes["RESET_ALL"])
}

#' In an annotation file, the value in each column are often
#' delimited by symbols like ';', ',', or '|'. This function
#' is for finding the proper delimiter in the given strings.
#' TODO: The problem on whether the reg expr is fixed or not.
CheckSep <- function(x, priority = c(";", ",", "\\|"), default = "\t") {
    v <- sapply(x, function(x) priority[str_detect(x, priority)][1])
    v[is.na(v)] <- default
    v
}

#' Print messages, warnings, or errors in a unified format.
#' The messages will be written to STDOUT.
#' And the warnings and the errors will be written to STDERR.
WriteLog <- function(text, func.name = "Default",
                     level = c("info", "warn", "error")[1],
                     color = NULL, style = NULL, saveWhenQuit = "default")
{
    level <- rep(level, length.out = length(text))
    ChooseColor <- c(INFO = "yellow", WARN = "magenta", ERROR = "red")
    ChooseStyle <- c(INFO = "default", WARN = "default", ERROR = "bold")
    if (!length(color)) color <- ChooseColor[toupper(level)]
    if (!length(style)) style <- ChooseStyle[toupper(level)]
    if (any(is.na(c(color, style)))) {
        WriteLog("Invalid level given! A level must be 'info', 'warn', or 'error'.",
                 func.name = match.call()[[1]], level = "error",
                 saveWhenQuit = saveWhenQuit)
    }
    string <- paste0(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ",
        AddColor(x = paste0("[", toupper(level), "] [", func.name, "]"),
                 color = color, style = style),
        " -- ", trimws(text)
        )
    for (i in 1:length(text)) {
        switch(toupper(level[i]),
               INFO  = write(string[i], stdout()),
               WARN  = write(string[i], stderr()),
               ERROR = write(string[i], stderr()))
    }
    if ("ERROR" %in% toupper(level)) quit(save = saveWhenQuit, status = 1)
}


