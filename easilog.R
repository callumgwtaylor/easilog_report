library(tidyverse)
library(lubridate)
admissions <- read_csv("ignore/Admissions.csv")
cpd  <- read_csv("ignore/CPD Activities.csv")
procedures <- read_csv("ignore/Procedures.csv")

admissions$Hospital <- as.factor(admissions$Hospital)
procedures$Procedure <- as.factor(procedures$Procedure)
cpd$`Type of CPD Activity` <- as.factor(cpd$`Type of CPD Activity`)
procedures[] <- lapply(procedures, gsub, pattern = "\n", replacement = "", fixed = TRUE)
cpd[] <- lapply(cpd, gsub, pattern = "\n", replacement = " ", fixed = TRUE)
cpd$`Type of CPD Activity` <- as.factor(cpd$`Type of CPD Activity`)
admissions[] <- lapply(admissions, gsub, pattern = "\n", replacement = "", fixed = TRUE
colnames(cpd)[1] <- "date"
cpd$date <- dmy(cpd$date)
colnames(admissions)[1] <- "date"
admissions$date <- dmy(admissions$date)

# This section below returns a table that counts all procedures done, sorted alphabetically.
procedure_count <- procedures %>%
  group_by(Procedure) %>%
  mutate(count = n()) %>%
  select(Procedure, count) %>%
  distinct(.keep_all = TRUE)
procedure_count <- procedure_count[order(procedure_count$Procedure),]
procedure_count <- as.data.frame(procedure_count)

# This section looks at notes for arrests recorded
arrest_notes <- procedures %>%
  filter(Procedure == "Advanced Life Support (CPR)")
colnames(arrest_notes)[1] <- "date"
arrest_notes <- arrest_notes %>%
  select(date, Procedure, Supervision, Setting, Gender, Age, Notes)
arrest_notes$date <- dmy(arrest_notes$date)
arrest_notes <- arrest_notes[order(arrest_notes$date),]

# This section looks at teaching sessions attended
teaching_notes <- cpd %>%
  filter(`Type of CPD Activity` == "Course" | `Type of CPD Activity` == "Seminar" | `Type of CPD Activity` == "Training Day" | `Type of CPD Activity` == "Conference attended" | `Type of CPD Activity` == "Training event attended")
colnames(teaching_notes)[1] <- "date"
colnames(teaching_notes)[2] <- "type"
colnames(teaching_notes)[3] <- "title"
colnames(teaching_notes)[5] <- "notes"
teaching_notes$date <- dmy(teaching_notes$date)
teaching_notes <- teaching_notes[order(teaching_notes$date),]

# This section looks at teaching sessions provided, plus other CPD
cpd_notes <- cpd %>%
  filter(`Type of CPD Activity` == "Undergraduate teaching delivered" | `Type of CPD Activity` == "Audit complteted" | `Type of CPD Activity` == "Presentation")
colnames(cpd_notes)[1] <- "date"
colnames(cpd_notes)[2] <- "type"
colnames(cpd_notes)[3] <- "title"
colnames(cpd_notes)[5] <- "notes"
cpd_notes$date <- dmy(cpd_notes$date)
cpd_notes <- cpd_notes[order(cpd_notes$date),]

# This section looks at all clerk ins to ICU
icu_admissions <- admissions %>%
  filter(Role == 'Clerked') %>%
  select(date, Problem, `Referral source`, Notes)
icu_admissions <- icu_admissions[order(icu_admissions$date),]



