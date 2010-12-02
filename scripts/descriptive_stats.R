
# Placeholder content.  Will make more browseable soon.

metadataColumns = c("doi", "pubDate", "pubDateValue", "daysSincePublished", "journal")
altmetricsColumns = names(dat.eventcounts)[names(dat.eventcounts) %nin% metadataColumns]

dat.metricIsUsed = dat.eventcounts

dat.metricIsUsed[,altmetricsColumns][dat.metricIsUsed[,altmetricsColumns] > 1] = 1
summary(dat.metricIsUsed)

options(scipen=100)
options(digits=2)

# Metrics by Average number of articles with at least one event
apply(dat.metricIsUsed[,altmetricsColumns], 2, mean)

# Number of articles by how many metrics they have with at least one event
table(apply(dat.metricIsUsed[,altmetricsColumns], 1, sum))

# Look at the distributions
for (col in altmetricsColumns) {
	quartz()
	par(mfrow = c(2, 1))
	hist(dat.eventcounts[,col], main=col)
	hist(log(dat.eventcounts[,col]), main=paste("log(", col, ")", sep=""))
}

### Write out to examine in other programs
write.csv(dat.metricIsUsed, "data/derived/all_metrics_used.csv")

library(Hmisc)
Hmisc::describe(dat.eventcounts[,altmetricsColumns])
latex(Hmisc::describe(dat.eventcounts[,altmetricsColumns]))

Hmisc::describe(dat.metricIsUsed[,altmetricsColumns])

