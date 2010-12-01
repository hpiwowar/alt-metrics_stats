
# Placeholder content.  Will make more browseable soon.

count.if.gt1 = counts
count.if.gt1[count.if.gt1 > 1] = 1
summary(count.if.gt1)
apply(count.if.gt1, 2, mean)
table(apply(count.if.gt1, 1, sum))

metadataColumns = c("doi", "pubDate", "journal")
altmetricsColumns = names(dat.all)[names(dat.all) %nin% metadataColumns]

dat.all.if.gt1 = dat.all[,altmetricsColumns]
dat.all.if.gt1[dat.all > 1] = 1
summary(dat.all.if.gt1)

apply(dat.all.if.gt1, 2, mean)
table(apply(dat.all.if.gt1, 1, sum))