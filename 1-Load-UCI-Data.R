# Load Human Activity Recognition Using Smartphones Dataset
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# efg, 2014-09-04

################################################################################
### Setup

setwd("C:/Users/efg/Desktop/UCI/")          ##### Modify as appropriate
sink("1-Load-UCI-Data.txt", split=TRUE)

SAMSUNG.DIR <- "Samsung-Human-Activity"

Activity.Names <- c("Walk", "WalkUp", "WalkDown", "Sit", "Stand", "Lying")

################################################################################
### Helper function
###
### Read data GROUP:  "test" or "train"

fetchGroup <- function(GROUP)
{
  # Read subject_GROUP.txt file
  filename <- paste0(SAMSUNG.DIR, "/", GROUP, "/subject_", GROUP, ".txt")
  s <- readLines(filename)
  subject.id <- as.numeric(s)  # Convert to number

  # Human activity index:  y_GROUP.txt file
  filename <- paste0(SAMSUNG.DIR, "/", GROUP, "/y_", GROUP, ".txt")
  s <- readLines(filename)
  activity.index <- as.numeric(s)    # Convert to number

  # Sensor Signals:  X_GROUP.txt file
  filename <- paste0(SAMSUNG.DIR, "/", GROUP, "/X_", GROUP, ".txt")
  s <- readLines(filename)

  # All records are 8976 bytes.  Observation of first few columns suggests 16 bytes/column.
  # Therefore, must be 8976 / 16 = 561 columns = expected number of features in features.txt.
  x <- scan(filename)           # fast way to parse very regular file
  dim(x) <- c(561, length(s))   # Want a matrix
  tx <- t(x)                    # Really want transpose in a data.frame
  group.data <- data.frame(source=GROUP,
                           subject.id=subject.id,
                           activity=Activity.Names[activity.index],
                           tx,  # deal with column names below
                           stringsAsFactors=FALSE)
  invisible( group.data )
}

################################################################################
### Load test and train data

test  <- fetchGroup("test")
dim(test)

train <- fetchGroup("train")
dim(train)

################################################################################
### Combine into a single data.frame

combined.data <- rbind(test, train)
dim(combined.data)

################################################################################
### Let's cleanup feature names to be more R "friendly"

filename <- paste0(SAMSUNG.DIR, "/features.txt")
feature.names <- read.table(filename, sep=" ", header=FALSE, as.is=TRUE)
feature.names <- feature.names$V2   # only want 2nd column

# Change "()" to "E" for "estimate" as in "estimated from these signals"
feature.names <- gsub("\\()", "E", feature.names)

# Remove dashes and commas
feature.names <- gsub("-|,", ".", feature.names)

# Fix angle data with paretheses. "(" -> ".". ")" -> ""
feature.names <- gsub("\\(", ".", feature.names)
feature.names <- gsub(")",   "",  feature.names)

# Prefix all names with vNNN. to maintain link to original documentation.
# (These can always be removed later if not wanted.)
feature.names <- sprintf("v%3.3d.%s", 1:length(feature.names),
                           feature.names)
# Add "friendly" feature names to combined.data

names(combined.data)[4:ncol(combined.data)] <- feature.names

################################################################################
### Look at crosstab of subjects and activities

print( table(combined.data$subject.id, combined.data$activity) )

################################################################################
### Quick summary of variables

head(names(combined.data))
tail(names(combined.data))

################################################################################
### Single case

head(combined.data,1)

################################################################################
### Write to file for future use
write.csv(combined.data, "Samsung-Human-Activity.csv", row.names=FALSE)

sink()


