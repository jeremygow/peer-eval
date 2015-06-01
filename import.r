# peer.dir <- "SET TO DIRECTORY CONTAINING QUESTIONNAIRE RESPONSES"

# Import raw response data
responsePath <- paste0(peer.dir, "/", "responses.tsv")
rawResponses <- read.csv(responsePath, header=T, sep="\t", stringsAsFactors=FALSE)

names(rawResponses) <- c("Response", "Submitted.on.", "Institution", "Department", "Course", "NAGroup",
                         "ID", "Evaluator", "Username", "Group", "Student", "Participated",
                         "Contribution.4", "Contribution.3", "Contribution.2", "Contribution.1", "Contribution.0",
                         "Interacting.4", "Interacting.3", "Interacting.2", "Interacting.1", "Interacting.0", 
                         "Track.4", "Track.3", "Track.2", "Track.1", "Track.0", 
                         "Quality.4", "Quality.3", "Quality.2", "Quality.1", "Quality.0",
                         "Skills.4", "Skills.3", "Skills.2", "Skills.1", "Skills.0", 
                         "ShoutOut", "ShoutOutComment", "Other")

# Clean up response data
responses <- rawResponses[,c(8:12)]

responses$Contribution <- NA
responses$Contribution[rawResponses$Contribution.4 == 1] <- 4
responses$Contribution[rawResponses$Contribution.3 == 1] <- 3
responses$Contribution[rawResponses$Contribution.2 == 1] <- 2
responses$Contribution[rawResponses$Contribution.1 == 1] <- 1
responses$Contribution[rawResponses$Contribution.0 == 1] <- 0

responses$Interacting <- NA
responses$Interacting[rawResponses$Interacting.4 == 1] <- 4
responses$Interacting[rawResponses$Interacting.3 == 1] <- 3
responses$Interacting[rawResponses$Interacting.2 == 1] <- 2
responses$Interacting[rawResponses$Interacting.1 == 1] <- 1
responses$Interacting[rawResponses$Interacting.0 == 1] <- 0

responses$Track <- NA
responses$Track[rawResponses$Track.4 == 1] <- 4
responses$Track[rawResponses$Track.3 == 1] <- 3
responses$Track[rawResponses$Track.2 == 1] <- 2
responses$Track[rawResponses$Track.1 == 1] <- 1
responses$Track[rawResponses$Track.0 == 1] <- 0

responses$Quality <- NA
responses$Quality[rawResponses$Quality.4 == 1] <- 4
responses$Quality[rawResponses$Quality.3 == 1] <- 3
responses$Quality[rawResponses$Quality.2 == 1] <- 2
responses$Quality[rawResponses$Quality.1 == 1] <- 1
responses$Quality[rawResponses$Quality.0 == 1] <- 0

responses$Skills <- NA
responses$Skills[rawResponses$Skills.4 == 1] <- 4
responses$Skills[rawResponses$Skills.3 == 1] <- 3
responses$Skills[rawResponses$Skills.2 == 1] <- 2
responses$Skills[rawResponses$Skills.1 == 1] <- 1
responses$Skills[rawResponses$Skills.0 == 1] <- 0

responses$Mean <- rowMeans(responses[,c("Contribution", "Interacting", "Track", "Quality", "Skills")])
responses$ShoutOut <- rawResponses$ShoutOut
responses$ShoutOut[is.na(responses$ShoutOut)] <- 0
responses$ShoutOutComment <- rawResponses$ShoutOutComment
responses$Other <- rawResponses$Other
responses <- responses[order(responses$Group,responses$Student),]

# Normalise whitespace and case
library(stringr)
titleCase <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}
tidyName <- function(x) {
  x <- str_trim(x)
  x <- str_replace_all(x, "  ", " ")
  titleCase(x) 
}
responses$Evaluator <- unlist(lapply(responses$Evaluator, tidyName))
responses$Student <- unlist(lapply(responses$Student, tidyName))

# Apply corrections to names, based on CSV file of pairs "incorrectName, correctName"
correctionsPath <- paste0(peer.dir, "/", "corrections.csv")
corrections <- read.csv(correctionsPath, strip.white=TRUE, stringsAsFactors=FALSE, header=FALSE)
names(corrections) <- c("old", "new")
matches <- match(responses$Student, corrections$old, nomatch=0)
responses$Student <- replace(responses$Student, matches > 0, corrections$new[matches])

outPath <- paste0(peer.dir, "/", "summary.csv")
write.csv(responses, file=outPath, row.names=FALSE)



getGroup <- function(x) {
  responses[responses$Group == x,]
}





