
# Placeholder content.  Will make more browseable soon.

metadataColumns = c("doi", "pubDate", "pubDateValue", "daysSincePublished", "journal")
altmetricsColumns = names(dat.all)[names(dat.all) %nin% metadataColumns]

dat.all.metricIsUsed = dat.all

dat.all.metricIsUsed[,altmetricsColumns][dat.all.metricIsUsed[,altmetricsColumns] > 1] = 1
summary(dat.all.metricIsUsed)

options(scipen=100)
options(digits=2)

# Metrics by Average number of articles with at least one event
apply(dat.all.metricIsUsed[,altmetricsColumns], 2, mean)

# Number of articles by how many metrics they have with at least one event
table(apply(dat.all.metricIsUsed[,altmetricsColumns], 1, sum))
