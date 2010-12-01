library(Rserve)
Rserve(args="--no-save")


###### events.txt
# file has one row for each event.
# in some cases, an event contains a count of occurances that day in the "values" column

## READ DATA
dat.raw.events = read.csv("data/events.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

## Look at it
dim(dat.raw.events)
names(dat.raw.events)
summary(dat.raw.events)

## add a new column called number.events that is ususally 1
## but is set to the number of occurances in the "value" column 
### for datatypes with more than one occurance per row
dat.events = dat.raw.events
number.rows = table(dat.raw.events$eventType)
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
counts = tapply(dat.events$number.events, list(dat.events$doi, dat.events$eventType), sum)
counts[is.na(counts)] = 0

# look at it
colnames(counts)
counts["10.1371/journal.pone.0008280",]
summary(counts)
describe(counts)

## get the data into a data frame so easier to merge
counts.df = as.data.frame(counts.df)
counts.df$doi = rownames(counts.df)


######## OK NOW event_counts.txt
## This data contains into on metrics for which we only have aggregate counts

### READ DATA
dat.raw.eventcounts = read.csv("data/event_counts.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

## Look at it
dim(dat.raw.eventcounts)
names(dat.raw.eventcounts)

## Adjust some fields to they are the right datatype
dat.eventcounts$journal = factor(dat.raw.eventcounts$journal)
dat.eventcounts$f1000Factor = as.integer(dat.raw.eventcounts$f1000Factor)
dat.eventcounts$f1000Factor[is.na(dat.eventcounts$f1000Factor)] = 0
summary(dat.eventcounts)


########  Now merge it all together

dim(merge(counts.df, dat.raw.eventcounts, by=c("doi")))
dat.all = merge(counts.df, dat.raw.eventcounts, by=c("doi"))

## Look at it

summary(dat.all)

