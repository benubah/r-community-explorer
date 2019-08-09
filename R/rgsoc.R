trim.strings <- function(x, side = "both") { 
  if (is.na(match(side, c("both", "leading", "trailing")))) { 
    side <- "both" 
  } 
  if (side == "leading") { 
    sub("^\\s+", "", x)
  } else {
    if (side == "trailing") {
      sub("\\s+$", "", x)
    } else gsub("^\\s+|\\s+$", "", x)
  } 
} 

projects <- read.csv("docs/data/gsoc.csv", sep = "\t", encoding = "utf-8")
colnames(projects) <- c("Year", "Project", "WorkProduct", "Student", "Mentors")
number_of_projects <- dim(projects)[[1]]

# Mentors
mentors <- projects$Mentors
mentorlist <- paste(mentors, collapse = ",")
mentorvector <- strsplit(mentorlist, ",")[[1]]
mentortable <- table(trim.strings(mentorvector))
mentordf <- as.data.frame(mentortable)
colnames(mentordf) <- c("mentor_name", "Freq")
number_of_mentors <- length(mentordf$mentor_name)
mentordf <- mentordf[order(mentordf$Freq, decreasing = TRUE), ]
most_active_mentors <- head(mentordf,15)


# Students
students <- projects$Student
student_table <- table(trim.strings(students))
studentdf <- as.data.frame(student_table)
colnames(studentdf) <- c("student_name", "Freq")
number_of_students <- length(studentdf$student_name)

# Student Turned Mentors
student_turned_mentor <- (subset(studentdf, (student_name %in% mentordf$mentor_name)))$student_name
student_turned_mentor_project_count <- subset(mentordf, (  mentor_name %in% student_turned_mentor))
number_of_students_turned_mentors <- length(student_turned_mentor)
number_projects_mentored_by_paststudents <- sum(student_turned_mentor_project_count$Freq)
student_turned_mentor_project_count <- student_turned_mentor_project_count[order(student_turned_mentor_project_count$Freq, decreasing = TRUE), ]
top5_student_mentors <- head(student_turned_mentor_project_count,5)

# Students Returning to R org for a second GSoC
returning_students <- studentdf[(studentdf$Freq > 1), ]
number_returning_students <- dim(returning_students)[[1]]

gsocsummary <- data.frame(total_projects = number_of_projects , total_mentors = number_of_mentors, total_students = number_of_students, student_mentors = number_of_students_turned_mentors, numberOfMentoredProjectsByPastStudents = number_projects_mentored_by_paststudents, numberOfReturningStudents = number_returning_students)

# write to json
gsoc_json <- jsonlite::toJSON(list(gsocsummary, student_turned_mentor_project_count, top5_student_mentors, returning_students, most_active_mentors), auto_unbox = FALSE, pretty = TRUE)
writeLines(gsoc_json, "docs/data/gsoc_summary.json")
