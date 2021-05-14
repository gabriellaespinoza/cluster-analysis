# ----------------------------------------------------------------------------
# K MEANS CLUSTERING SCRIPT
# This script reads a CSV file performs a k-means clustering analysis

# ------------------ USER DEFINED VARIABLES ----------------------------------

# the folder of the CSV files to read and write
DATA_FOLDER_PATH = "~/FolderName/"

# the name of the csv file including extension (i.e. '....csv')
SOURCE_FILE_NAME = "filename"

# cluster based on all columns (TRUE) or a selection (FALSE)
ALL_COLUMNS = FALSE

# if ALL_COLUMNS = FALSE, select which columns to perform clustering
COLUMNS = c(3:26)

# the number of clusters to be created by K means algorithm
NUM_CLUSTERS = 7

# ------------------ SCRPT START ---------------------------------------------

# create read/write file names
read_file = paste0(DATA_FOLDER_PATH, SOURCE_FILE_NAME)
file_base_name = substr(read_file, 1, nchar(read_file)-4)
write_file_clusters = paste0(file_base_name,"_with_clusters.csv")
write_file_cluster_summary = paste0(file_base_name,"_cluster_summary.csv")

# read the csv file
dt <- read.csv(read_file)

# set random seed to allow for replication
set.seed(20)

# generate clusters
if (ALL_COLUMNS) {
  cluster <- kmeans(dt, NUM_CLUSTERS, nstart = 20)
} else {
  cluster <- kmeans(dt[, COLUMNS], NUM_CLUSTERS, nstart = 20)
}

# Save the cluster to the data frame
dt$cluster <- as.factor(cluster$cluster)

# write new data frame (with clusters) to CSV
write.csv(dt, write_file_clusters, row.names = FALSE)

# make cluster summary data frame to be written to fille
cluster_summary <- cbind(cluster$size, cluster$withinss, cluster$centers)
colnames(cluster_summary)[1] <- "cluster_size"
colnames(cluster_summary)[2] <- "withinss"
cluster_summary <- cluster_summary[order(-cluster_summary[,1]),]

# add additional sum of squares information
cluster_summary <- rbind(cluster_summary, c(""), c(""), c(""), c(""))
cluster_summary[nrow(cluster_summary)-2, 1] <- "Total sum of squares:"
cluster_summary[nrow(cluster_summary)-2, 2] <- cluster$totss
cluster_summary[nrow(cluster_summary)-1, 1] <- "Between sum of squares:"
cluster_summary[nrow(cluster_summary)-1, 2] <- cluster$betweenss
cluster_summary[nrow(cluster_summary), 1] <- "between_SS / total_SS:"
cluster_summary[nrow(cluster_summary), 2] <- cluster$betweenss / cluster$totss

# write cluster summary to CSV
write.csv(cluster_summary, write_file_cluster_summary, row.names = FALSE)
