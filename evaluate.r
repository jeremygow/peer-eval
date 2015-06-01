# Import list of students
studentPath <- paste0(peer.dir, "/", "students.csv")
students <- read.csv(studentPath,header=T,strip.white=TRUE,stringsAsFactors=FALSE)
students$Student <- unlist(lapply(students$Student, tidyName))

# Students participation / approval rating
peerEval <- responses[responses$Evaluator != responses$Student,]
students <- merge(students, aggregate(Participated ~ Student, peerEval, length), all.x=TRUE)
names(students)[3] <- "Evaluations"
students <- merge(students, aggregate(Participated ~ Student, peerEval, sum), all.x=TRUE)
students[is.na(students)] <- 0
students$Approval <- round(students$Participated / students$Evaluations, 2)

# Mean self rating
selfEval <- responses[responses$Evaluator == responses$Student,]
students <- merge(students, subset(selfEval, select=c(Student, Mean)), all.x=TRUE)
names(students)[6] <- "Self"

# Mean ratings
students <- merge(students, aggregate(Mean ~ Student, peerEval, mean), all.x=TRUE)
names(students)[7] <- "Peer"
students$Peer <- round(students$Peer,1)

students <- merge(students, aggregate(Contribution ~ Student, peerEval, mean), all.x=TRUE)
names(students)[8] <- "Contrib"
students$Contrib <- round(students$Contrib,1)

students <- merge(students, aggregate(Interacting ~ Student, peerEval, mean), all.x=TRUE)
names(students)[9] <- "Interact"
students$Interact <- round(students$Interact,1)

students <- merge(students, aggregate(Track ~ Student, peerEval, mean), all.x=TRUE)
names(students)[10] <- "Track"
students$Track <- round(students$Track,1)

students <- merge(students, aggregate(Quality ~ Student, peerEval, mean), all.x=TRUE)
names(students)[11] <- "Quality"
students$Quality <- round(students$Quality,1)

students <- merge(students, aggregate(Skills ~ Student, peerEval, mean), all.x=TRUE)
names(students)[12] <- "Skill"
students$Skill <- round(students$Skill,1)

# Shout outs
students <- merge(students, aggregate(ShoutOut ~ Student, peerEval, sum), all.x=TRUE)
names(students)[13] <- "ShoutOuts"
students$ShoutOuts[is.na(students$ShoutOuts)] <- 0


pastec <- function(x) {paste(x, collapse='')}

# Peer comments
students <- merge(students, aggregate(ShoutOutComment ~ Student, peerEval, pastec), all.x=TRUE)
students$ShoutOutComment[is.na(students$ShoutOutComment)] <- ""

students <- merge(students, aggregate(Other ~ Student, peerEval, pastec), all.x=TRUE)
students$Other[is.na(students$Other)] <- ""

selfEval$SelfComments <- paste(selfEval$ShoutOutComment, selfEval$Other)
students <- merge(students, subset(selfEval, select=c(Student, SelfComments)), all.x=TRUE)


students <- students[order(students$Group,students$Student),]
evalPath <- paste0(peer.dir, "/", "evaluation.csv")
write.csv(students, file=evalPath, row.names=FALSE)





