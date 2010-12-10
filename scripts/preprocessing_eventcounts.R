#library(Rserve)
#Rserve(args="--no-save")

######## event_counts.txt
## This data contains into on metrics for which we only have aggregate counts

### READ DATA
dat.raw.eventcounts = read.csv("data/raw/event_counts.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

## Look at it
dim(dat.raw.eventcounts)
names(dat.raw.eventcounts)
summary(dat.raw.eventcounts)

## A bit of data cleaning
dat.eventcounts = dat.raw.eventcounts

# get rid of rows that have dates that are NA
library(date)

# Create a column that contains the date as a date object
years = as.integer(substr(dat.raw.eventcounts$pubDate, 1, 4))
months = as.integer(substr(dat.raw.eventcounts$pubDate, 6, 7))
days = as.integer(substr(dat.raw.eventcounts$pubDate, 9, 10))
has.good.date = years > 0

# Now create a date type variable
dat.eventcounts$pubDateValue[has.good.date]  = mdy.date(months[has.good.date], days[has.good.date], years[has.good.date])
dat.eventcounts$pubDateValue[years <= 0]  = NA

# Create a column that has days since published
dat.eventcounts$daysSincePublished = max(dat.eventcounts$pubDateValue, na.rm=T) - dat.eventcounts$pubDateValue


## Adjust some fields to they are the right datatype.  

# Change journal strings to factors
dat.eventcounts$journal = factor(dat.raw.eventcounts$journal)

# Change f1000Factor strings to integer counts.  "false" means count of 0.
dat.eventcounts$f1000Factor = as.integer(dat.raw.eventcounts$f1000Factor)
dat.eventcounts$f1000Factor[is.na(dat.eventcounts$f1000Factor)] = 0

# delicious count looks strange for now
dat.eventcounts$deliciousCount = NULL


# rename PMC column
dat.eventcounts$almPubMedCentralCount = dat.eventcounts$almPubMedCount
dat.eventcounts$almPubMedCount = NULL

# There are a few Facebook results from Facebook API with negative numbers
# Not clear what this means (not in Facebook API docs), so setting to NA
facebookColumns = c("facebookShareCount", "facebookLikeCount", "facebookCommentCount", "facebookClickCount")
for (col in facebookColumns) {
	dat.eventcounts[which(dat.eventcounts[, col] < 0), col] = NA	
}



## Look again
summary(dat.eventcounts)


