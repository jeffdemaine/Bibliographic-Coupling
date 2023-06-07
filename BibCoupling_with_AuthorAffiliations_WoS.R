install.packages("tidyverse")
install.packages("stringr")
install.packages("sqldf")
library(tidyverse)
library(stringr)
library(sqldf)

# PURPOSE: To match authors from two universities based on their Bibliographic Coupling strength.
# This pulls out the authors' affiliation info, calculates the Jaccard Similarity of 
# every pair of authors, and then re-connects the paired names with their affiliation data.
# However, it may be that the author pairs are already co-authors on other papers.

# REQUIRES: Web of Science records for two universities (presumably on the same topic) in two formats:
# - as an Excel file. Then convert to csv format.
# - as Plain text. The original linear ISI format. THIS MUST INCLUDE CITED REFERENCES!


# Step 1 - SETUP
# 1.1 Set Working Directory -> To Project Directory
# 1.2 Load utility functions:
source("Utility_parseAuthorAffiliations_WoS.R")
source("Utility_BibCoupling_Authors_Jaccard.R")

#Replace with the real file names:
inputFile_UnivA_csv <- "SomeFileName.csv"
inputFile_UnivB_csv <- "SomeFileName.csv"
inputFile_UnivA_ISI <- "SomeFileName.txt"
inputFile_UnivB_ISI <- "SomeFileName.txt"
outputFilename <- "SomeFileName.csv"

# Step 2 - Load files from WoS (Export from WoS in Excel format (then converted to csv format):
inputFile_A.df = read.csv(inputFile_UnivA_csv, sep = ",", header = TRUE)
inputFile_B.df = read.csv(inputFile_UnivB_csv, sep = ",", header = TRUE)
inputFile.df <- rbind(inputFile_A.df, inputFile_B.df)

# Step 3 - Parse names and affiliations inta a dataframe:
namesByInstitution <- parseAuthorsWithInstitution(inputFile.df) # parseAuthorsByInstitution(inputFile.df)

# Step 4 - Get the Bibliographic Coupling for authors with affiliations:
# Files must be in WoS "Plain text" (field-list) format. 
# These MUST BE the same records as in Step 2, just in a different format.
resultsBC <- bibCoupling_Authors_2institutions(inputFile_UnivA_ISI, inputFile_UnivB_ISI, "wos", "plaintext")

# Filter the resulting dataframe:
dt.temp1 <- resultsBC[order(resultsBC$Jaccard_Similarity, decreasing = TRUE), ]  # Sort by "weight" column
dt.temp2 <- dt.temp1[dt.temp1$Jaccard_Similarity < 1, ]          # Weight of 1 implies identical references, which is simply because they are co-authors.
dt.temp3 <- by(dt.temp2, dt.temp2["From_author"], head, n=5)     # Select the top 5 per citing author ("from")
resultsBC_tidy <- Reduce(rbind, dt.temp3)                        # Convert to a dataframe

#Step 5 - Match names+affiliations from Step #3 with Bibliographically-coupled author pairs from Step 4:
merged <- sqldf("SELECT DISTINCT * FROM (
              SELECT DISTINCT J.Row_num, J.From_author AS Author_A, J.To_author AS Author_B, J.Jaccard_Similarity, N.DOI AS DOI_A, N.author AS A_FullName, N.university AS A_University, N.country AS A_Country, 
                    REPLACE(upper(
                      substr(N.author, 0, 3 + instr(N.author, \",\"))), \",\", \"\") AS A_author_affiliated
              FROM resultsBC_tidy AS J, namesByInstitution AS N
              WHERE substr(J.From_author, 0, 2 + instr(J.From_author, \" \")) LIKE A_author_affiliated) AS T1
              INNER JOIN (
              SELECT DISTINCT J2.Row_num, J2.To_author AS Author_B, N2.DOI AS DOI_B, N2.author AS B_FullName, N2.university AS B_University, N2.country AS B_Country, 
                    REPLACE(upper(
                      substr(N2.author, 0, 3 + instr(N2.author, \",\"))), \",\", \"\") AS B_author_affiliated
              FROM resultsBC_tidy AS J2, namesByInstitution AS N2
              WHERE substr(J2.To_author, 0, 2 + instr(J2.To_author, \" \")) LIKE B_author_affiliated) AS T2
              ON T1.Row_num = T2.Row_num
              WHERE T1.DOI_A <> T2.DOI_B;")

# Save this as a csv file. Analyze further as required.
write.csv(merged, outputFilename, row.names=FALSE)