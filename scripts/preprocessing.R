library(Rserve)
Rserve(args="--no-save")

######## event_counts.txt
## This data contains into on metrics for which we only have aggregate counts

### READ DATA
dat.raw.eventcounts = read.csv("data/raw/event_counts.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

## Look at it
dim(dat.raw.eventcounts)
names(dat.raw.eventcounts)
summary(dat.eventcounts)

## Adjust some fields to they are the right datatype.  
dat.eventcounts = dat.raw.eventcounts

# Change journal strings to factors
dat.eventcounts$journal = factor(dat.raw.eventcounts$journal)

# Change f1000Factor strings to integer counts.  "false" means count of 0.
dat.eventcounts$f1000Factor = as.integer(dat.raw.eventcounts$f1000Factor)
dat.eventcounts$f1000Factor[is.na(dat.eventcounts$f1000Factor)] = 0

library(date)
# Create a column that contains the date as a date object
days = as.integer(substr(dat.raw.eventcounts$pubDate, 9, 10))
months = as.integer(substr(dat.raw.eventcounts$pubDate, 6, 7))
years = as.integer(substr(dat.raw.eventcounts$pubDate, 1, 4))
dat.eventcounts$pubDateValue  = mdy.date(months, days, years)

# Create a column that has days since published
dat.eventcounts$daysSincePublished = max(dat.eventcounts$pubDateValue) - dat.eventcounts$pubDateValue

## Look again
summary(dat.eventcounts)


###### events.txt
# file has one row for each event.
# in some cases, an event contains a count of occurances that day in the "values" column

## READ DATA
dat.raw.events = read.csv("data/raw/events.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

## Look at it
dim(dat.raw.events)
names(dat.raw.events)
summary(dat.raw.events)

## Now adjust because some events include multiple occurances
## add a new column called number.events that is ususally 1
## but is set to the number of occurances in the "value" column 
### for datatypes with more than one occurance per row
dat.events = dat.raw.events
number.rows = table(dat.events$eventType)
eventTypes = names(number.rows)
# Assign the contents of value as the number of occurances
dat.events$number.events = as.integer(dat.events$value)		
# But for many datatypes the value content is actually a text string.  
# for these, overwrite with the number of occurances with 1.
events.with.individual.rows = names(number.rows[number.rows < max(number.rows)])
for (myEventType in events.with.individual.rows) {
	dat.events$number.events[dat.events$eventType == myEventType] = 1
}

## now consolidate these events into a single table of dois, eventsType, and total count
dat.events.perDoi = as.data.frame(tapply(dat.events$number.events, list(dat.events$doi, dat.events$eventType), sum))
dat.events.perDoi$doi = rownames(dat.events.perDoi)
dat.events.perDoi[is.na(dat.events.perDoi)] = 0

# look at it
colnames(dat.events.perDoi)
dat.events.perDoi["10.1371/journal.pone.0008280",]
summary(dat.events.perDoi)


########  Now merge it all together

dim(merge(dat.events.perDoi, dat.eventcounts, by=c("doi")))
dat.all = merge(dat.events.perDoi, dat.eventcounts, by=c("doi"))

## Look at it
dim(dat.all)
names(dat.all)
summary(dat.all)

########  Save this data
# then can examine it easily in other programs

write.csv(dat.all, "data/derived/all_event_counts.csv")
