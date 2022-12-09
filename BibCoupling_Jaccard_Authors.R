install.packages("bibliometrix")
install.packages("chinese.misc")
install.packages("tidyverse")
install.packages("stringr")
install.packages("sqldf")
library(bibliometrix)
library(biblionetwork)
library(chinese.misc)
library(tidyverse)
library(igraph)
library(sqldf)


# SETUP: Set Working Directory -> To Project Directory

# Import bibliographic metadata for University #1:
metadata.file1 <- "DATA\\BC_ETH_McM\\Dimensions-McM_metadata.csv"
M1 <- convert2df(file = metadata.file1, dbsource = "dimensions", format = "csv")

# Import bibliographic metadata for University #2:
metadata.file2 <- "DATA\\BC_ETH_McM\\Dimensions-ETH_metadata.csv"
M2 <- convert2df(file = metadata.file2, dbsource = "dimensions", format = "csv")

# Join these together:
M <- rbind(M1, M2)


# Bibliometric Coupling grouped at AUTHOR level:
NetMatrix_Authors <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")

Sim <- normalizeSimilarity(NetMatrix_Authors, type = "jaccard")

# Convert matrix to long format:
# From the igraph package, the “graph_from_adjacency_matrix” function takes a square 
# adjacency matrix as input. This can be a sparse matrix created with the Matrix package.
# Then the "as_data_frame" function concatenates every row into a column with cbind:
ajs <- as_data_frame(    # "ajs" = Authors Jaccard Similarity
        graph_from_adjacency_matrix(
          Sim, 
          weighted = TRUE, 
          mode = 'undirected',
          diag = FALSE))         # Remove the diagonal values

# Better column names:
colnames(ajs) <- c("From_author", "To_author", "Jaccard_Similarity")

# Filter the resulting dataframe:
dt.temp1 <- ajs[order(ajs$Jaccard_Similarity, decreasing = TRUE), ]  # Sort by "weight" column
dt.temp2 <- dt.temp1[dt.temp1$Jaccard_Similarity < 1, ]          # Weight of 1 implies identical references, which is simply because they are co-authors.
dt.temp3 <- by(dt.temp2, dt.temp2["From_author"], head, n=3)  # Select the top 5 per citing author ("from")

ajs_clean <- Reduce(rbind, dt.temp3)                          # Convert to dataframe

write.csv(ajs_clean, "DATA\\McM_ETH_AuthorsByJaccardSimilarity.csv", row.names=FALSE)


View(ajs_clean)


# Match Bibliographically-coupled names with names by university:
merged1 <- sqldf("SELECT 'ETH', From_author, To_author, Weight
                 FROM dt, Authors_ETH, Authors_McM
                 WHERE dt.From_author == UPPER(Authors_ETH.Author)
                 AND dt.To_author == UPPER(Authors_McM.Author)
                 UNION
                 SELECT 'McM', From_author, To_author, Weight
                 FROM dt, Authors_ETH, Authors_McM
                 WHERE dt.From_author == UPPER(Authors_McM.Author)
                 AND dt.To_author == UPPER(Authors_ETH.Author)")

View(merged1)
