# HEADER Cleaning Data
library(ggplot2)

# Opening File
data <- read.csv("Data.csv")

# We factorize these variables here so that we can use the amounts of them 
data$Offense <- factor(data$Offense)
data$Offense.Parent.Group <- factor(data$Offense.Parent.Group)
data$Offense.Code <- factor(data$Offense.Code) # Probably useless (clerical)
data$Crime.Against.Category = factor(data$Crime.Against.Category)

# NOT_A_CRIME is self defense AKA Justifiable Homicide
not.a.crime.indices <- which(data$Crime.Against.Category == "NOT_A_CRIME")
data$Offense[not.a.crime.indices]
self.defense.indices <- which(data$Offense == "Justifiable Homicide")

# Factorizing the geographical locations
data$Precinct <- factor(data$Precinct)
data$Sector <- factor(data$Sector)
data$Beat <- factor(data$Beat)
data$MCPP  <- factor(data$MCPP)

# Amount of blank end time, start time, and report time
end.time.blank <- which(data$Offense.End.DateTime == "") #412178 reports without a end time
start.time.blank <- which(data$Offense.Start.DateTime == "") #721 reports without a start time
report.time.blank <- which(data$Offense.Report.DateTime == "") #0 reports without a report time
same.time <- which(data$Offense.End.DateTime == data$Offense.Start.DateTime) #44987 reports with the same start and end time


# Convert DateTime into 5 variables, day, month, year, time, AM/PM (watch out for midnight)
data$Offense.Start.DateTime.Month <- substr(data$Offense.Start.DateTime, 1, 2)
data$Offense.Start.DateTime.Day <- substr(data$Offense.Start.DateTime, 4, 5)
data$Offense.Start.DateTime.Year <- substr(data$Offense.Start.DateTime, 7, 10)
data$Offense.Start.DateTime.Hour <- substr(data$Offense.Start.DateTime, 12, 13)
data$Offense.Start.DateTime.Minute <- substr(data$Offense.Start.DateTime, 15, 16)
data$Offense.Start.DateTime.Seconds <- substr(data$Offense.Start.DateTime, 18, 19)
data$Offense.Start.DateTime.AM <- substr(data$Offense.Start.DateTime, 21, 22)
PM_Indices <- which(data$Offense.Start.DateTime.AM == "PM")

data$Offense.Start.DateTime.Month <- as.numeric(data$Offense.Start.DateTime.Month)
data$Offense.Start.DateTime.Day <- as.numeric(data$Offense.Start.DateTime.Day)
data$Offense.Start.DateTime.Year <- as.numeric(data$Offense.Start.DateTime.Year)
data$Offense.Start.DateTime.Hour <- as.numeric(data$Offense.Start.DateTime.Hour)
data$Offense.Start.DateTime.Minute <- as.numeric(data$Offense.Start.DateTime.Minute)
data$Offense.Start.DateTime.Seconds <- as.numeric(data$Offense.Start.DateTime.Seconds)

# We wanted to change the time from AM PM to military time
data$Offense.Start.DateTime.Hour[PM_Indices] <- data$Offense.Start.DateTime.Hour[PM_Indices] + 12
Midnight_Indices <- which(data$Offense.Start.DateTime.Hour == 24)
data$Offense.Start.DateTime.Hour[Midnight_Indices] <- data$Offense.Start.DateTime.Hour[Midnight_Indices] - 24

#Same as before but for the Report time
data$Report.DateTime.Month <- substr(data$Report.DateTime, 1, 2)
data$Report.DateTime.Day <- substr(data$Report.DateTime, 4, 5)
data$Report.DateTime.Year <- substr(data$Report.DateTime, 7, 10)
data$Report.DateTime.Hour <- substr(data$Report.DateTime, 12, 13)
data$Report.DateTime.Minute <- substr(data$Report.DateTime, 15, 16)
data$Report.DateTime.Seconds <- substr(data$Report.DateTime, 18, 19)
data$Report.DateTime.AM <- substr(data$Report.DateTime, 21, 22)
PM_Indices_Report <- which(data$Report.DateTime.AM == "PM")

data$Report.DateTime.Month <- as.numeric(data$Report.DateTime.Month)
data$Report.DateTime.Day <- as.numeric(data$Report.DateTime.Day)
data$Report.DateTime.Year <- as.numeric(data$Report.DateTime.Year)
data$Report.DateTime.Hour <- as.numeric(data$Report.DateTime.Hour)
data$Report.DateTime.Minute <- as.numeric(data$Report.DateTime.Minute)
data$Report.DateTime.Seconds <- as.numeric(data$Report.DateTime.Seconds)

data$Report.DateTime.Hour[PM_Indices_Report] <- data$Report.DateTime.Hour[PM_Indices_Report] + 12
Midnight_Indices_Report <- which(data$Report.DateTime.Hour == 24)
data$Report.DateTime.Hour[Midnight_Indices_Report] <- data$Report.DateTime.Hour[Midnight_Indices_Report] - 24

# We want these as a factor for our graphs, so ggplot can use them for bar plots
data$Report.DateTime.Year <- as.factor(data$Report.DateTime.Year)
data$Offense.Start.DateTime.Month <- factor(data$Offense.Start.DateTime.Month, 
                                            levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                                            labels = c("Jan", "Feb", "Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"))
data$Offense.Start.DateTime.Day <- factor(data$Offense.Start.DateTime.Day)
data$Offense.Start.DateTime.Year <- factor(data$Offense.Start.DateTime.Year)
data$Offense.Start.DateTime.Hour <- factor(data$Offense.Start.DateTime.Hour)
data$Offense.Start.DateTime.Minute <- factor(data$Offense.Start.DateTime.Minute)
data$Offense.Start.DateTime.Seconds <- factor(data$Offense.Start.DateTime.Seconds)


# Make own types of criminal offenses column for less types.
data$type.of.criminal.offense <- ""

data$type.of.criminal.offense[which(data$Offense.Parent.Group == "HOMICIDE OFFENSES" |
                                    data$Offense.Parent.Group == "SEX OFFENSES, CONSENSUAL" |
                                    data$Offense.Parent.Group == "SEX OFFENSES" |
                                    data$Offense.Parent.Group == "ASSAULT OFFENSES" |
                                    data$Offense.Parent.Group == "HUMAN TRAFFICKING" |
                                    data$Offense.Parent.Group == "KIDNAPPING/ABDUCTION" |
                                    data$Offense.Parent.Group == "PEEPING TOM" | 
                                    data$Offense.Parent.Group == "FAMILY OFFENSES, NONVIOLENT")
                              ] <- "Personal Crimes"

data$type.of.criminal.offense[which(data$Offense.Parent.Group == "ARSON" |
                                    data$Offense.Parent.Group == "BURGLARY/BREAKING&ENTERING" |
                                    data$Offense.Parent.Group == "DESTRUCTION/DAMAGE/VANDALISM OF PROPERTY" |
                                    data$Offense.Parent.Group == "LARCENY-THEFT" |
                                    data$Offense.Parent.Group == "MOTOR VEHICLE THEFT" |
                                    data$Offense.Parent.Group == "STOLEN PROPERTY OFFENSES" |
                                    data$Offense.Parent.Group == "ROBBERY" |
                                    data$Offense.Parent.Group == "TRESPASS OF REAL PROPERTY" )
                              ] <- "Property Crimes"

data$type.of.criminal.offense[which(data$Offense.Parent.Group == "ANIMAL CRUELTY" |
                                    data$Offense.Parent.Group == "CURFEW/LOITERING/VAGRANCY VIOLATIONS" |
                                    data$Offense.Parent.Group == "PROSTITUTION OFFENSES" |
                                    data$Offense.Parent.Group == "PORNOGRAPHY/OBSCENE MATERIAL" |
                                    data$Offense.Parent.Group == "DRUNKENNESS" |
                                    data$Offense.Parent.Group == "DRIVING UNDER THE INFLUENCE" |
                                    data$Offense.Parent.Group == "DRUG/NARCOTIC OFFENSES" |
                                    data$Offense.Parent.Group == "LIQUOR LAW VIOLATIONS" |
                                    data$Offense.Parent.Group == "GAMBLING OFFENSES" |  
                                    data$Offense.Parent.Group == "WEAPON LAW VIOLATIONS"
)] <- "Statutory Crimes"

data$type.of.criminal.offense[which(data$Offense.Parent.Group == "BAD CHECKS" |
                                    data$Offense.Parent.Group == "BRIBERY" |
                                    data$Offense.Parent.Group == "COUNTERFEITING/FORGERY" |
                                    data$Offense.Parent.Group == "EMBEZZLEMENT" |
                                    data$Offense.Parent.Group == "FRAUD OFFENSES" |
                                    data$Offense.Parent.Group == "EXTORTION/BLACKMAIL" |
                                    data$Offense.Parent.Group == "" # For any missing criminal offense
)] <- "Financial and Other Crimes"

#Now we want them as factors
data$type.of.criminal.offense <- factor(data$type.of.criminal.offense)

#HEADER Presenting the Data

# Figure 1. Talking about types of criminal offenses across the years
ggplot(data = data, mapping = aes(x = Report.DateTime.Year, fill = type.of.criminal.offense)) + geom_bar(position = "dodge") + 
  labs(x = "Year Reported", y = "Count", title = "Amount of Crimes Across the Years", fill = "Type of Criminal Offense")

#Now we want a subset for 2019-2021 for our next graph
data2019.to.2021 <- subset(data, Report.DateTime.Year == 2019 | Report.DateTime.Year == 2020 | Report.DateTime.Year == 2021 )
data2019.to.2021$Report.DateTime.Month <- factor(data2019.to.2021$Report.DateTime.Month,
                                                    levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                                    labels = c("Jan", "Feb", "Mar","Apr","May","June",
                                                               "July","Aug","Sept","Oct","Nov","Dec"))

# Figure 2. Types of crimes from 2019-2021 by the months
ggplot(data = data2019.to.2021, mapping = aes(x = Report.DateTime.Month, fill = type.of.criminal.offense)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Month Reported", y = "Count", title = "Amount of Crimes Across the Months of 2019-2021", fill = "Type of Criminal Offense") + 
  facet_grid(data2019.to.2021$Report.DateTime.Year)

# The code for our further research into the spike in financial crimes in 2020

financial.data.2020 <- subset(data, Report.DateTime.Month == 5 & Report.DateTime.Year == 2020 & type.of.criminal.offense == "Financial and Other Crimes")

# This is how we found out that the majority of crimes here identity theft
table(financial.data.2020$Offense)
# Identity theft 7799 Identity Theft

financial.data.2020.identity.theft <- subset(data, Report.DateTime.Month == 5 & Report.DateTime.Year == 2020 & type.of.criminal.offense == "Financial and Other Crimes" & Offense == "Identity Theft")

#This is how we found out most of the identity thefts started in March, April, and May
table(financial.data.2020.identity.theft$Offense.Start.DateTime.Month)

# Figure 3. Offense start time by hours
ggplot(data = subset(data, !is.na(Offense.Start.DateTime.Hour)), mapping = aes(x = as.factor(Offense.Start.DateTime.Hour))) + 
  geom_bar(fill = "#000066") +
  labs(x = "Start time of the Offense", y = "Count", title = "Offense Start Times by the Hour") + ylim(0,80000)

# Figure 4. Report start time by hours
ggplot(data = data, mapping = aes(x = as.factor(Report.DateTime.Hour))) + geom_bar(fill = "#B20000") +
  labs(x = "Report time of the Offense", y = "Count", title = "Report Times by the Hour") + ylim(0,80000)

# In order to answer our questions about how many reports per unit, we need to be able to get the mode of that unit
getmode <- function(v) {
  x <- unique(v)
  x[which.max(tabulate(match(v, x)))]
}

#mode of report.number for largest amount of offenses on one report
getmode(data$Report.Number)
#[1] "2011-409287"
subset(data, Report.Number == "2011-409287") #9 Offenses on this report

# We have time of report as either Date and time or individual parts, but we don't have
# Just the Date, so we create the Data here by concatenating the strings of month, day, and year 
data$Report.Date <- "";
data$Report.Date <- paste(data$Report.DateTime.Month, data$Report.DateTime.Day, data$Report.DateTime.Year, sep ="/")

#Getting the date the most reports on it
getmode(data$Report.Date)
#5/26/2020


