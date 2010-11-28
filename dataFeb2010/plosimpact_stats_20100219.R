### To do

# to get latex to work, first in the R GUI go to 
# Misc, start X11 server
# And execute the following line
Sys.setenv( PATH=paste(Sys.getenv("PATH"),"/usr/texbin",sep=":") ) 


library(Rserve)
Rserve(args="--no-save")

library(plyr)

setwd("/Users/hpiwowar/Documents/Projects/PLoS ArticleImpact")
source("aim3_functions_20100215.R")


#### READ DATA
dat.raw = read.csv("PLoS_Metrics.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)
#save(dat.raw, file="dat_raw.Rdata")
#load("dat_raw.Rdata")
#dim(dat.raw)
#summary(dat.raw)
#names(dat.raw)
#names(dat.raw)[0:20]


#### START TO BUILD STATS DATASET
dat = data.frame(doi = dat.raw$DOI)

# skip publication date
dat$days.since.aug.2009         = dat.raw$Days.Since.Aug.2009
dat$pmid                        = dat.raw$PMID
# skip publication year
# skip journal, transform to dummy variables
#dat$journal                     = factor(dat.raw$Journal)
dat$is.plos.biology             = ifelse(dat.raw$Journal == "PLoS Biology", 1, 0)
dat$is.plos.clinical.trials     = ifelse(dat.raw$Journal == "PLoS Clinical Trials", 1, 0)
dat$is.plos.comp.bio            = ifelse(dat.raw$Journal == "PLoS Computational Biology", 1, 0)
dat$is.plos.genetics            = ifelse(dat.raw$Journal == "PLoS Genetics", 1, 0)
dat$is.plos.medicine            = ifelse(dat.raw$Journal == "PLoS Medicine", 1, 0)
dat$is.plos.neglect.tropical    = ifelse(dat.raw$Journal == "PLoS Neglected Tropical Diseases", 1, 0)
dat$is.plos.one                 = ifelse(dat.raw$Journal == "PLoS ONE", 1, 0)
dat$is.plos.pathogens           = ifelse(dat.raw$Journal == "PLoS Pathogens", 1, 0)
dat$is.research                 = factor(dat.raw$X.Research.Article..or..non.Research.Article..)
# skip title
dat$num.cites.crossref          = dat.raw$Citations...CrossRef
dat$num.cites.pmc               = dat.raw$Citations...PubMed.Central
dat$num.cites.scopus            = dat.raw$Citations...Scopus
dat$num.page.views.html         = dat.raw$Total.HTML.Page.Views
dat$num.downloads.pdf           = dat.raw$Total.PDF.Downloads
dat$num.downloads.xml           = dat.raw$Total.XML.Downloads
# skip combined usage
dat$num.blogs.postgenomic       = dat.raw$Blog.Coverage...Postgenomic
dat$num.blogs.natureblogs       = dat.raw$Blog.Coverage...Nature.Blogs
dat$num.blogs.bloglines         = dat.raw$Blog.Coverage...Bloglines
dat$num.trackbacks              = dat.raw$Number.of.Trackbacks
dat$num.bookmarks.citeulike     = dat.raw$Social.Bookmarking...CiteULike
dat$num.bookmarks.connotea      = dat.raw$Social.Bookmarking...Connotea
dat$num.ratings                 = dat.raw$Number.of.Ratings
dat$avg.rating                  = dat.raw$Average.Rating
dat$num.note.threads            = dat.raw$Number.of.Note.threads
dat$num.note.replies            = dat.raw$Number.of.replies.to.Notes
dat$num.comment.threads         = dat.raw$Number.of.Comment.threads
# skip num comment replies because all zero
#dat$num.comment.replies         = dat.raw$Number.of.replies.to.Comments
dat$num.ratings.with.comment    = dat.raw$Number.of..Star.Ratings..that.also.include.a.text.comment

dat.transformed = dat
vars.no.transform = c("doi", "pmid", "days.since.aug.2009")

vars.factor = c("is.research", "is.plos.clinical.trials", "is.plos.biology",
"is.plos.comp.bio", "is.plos.genetics", "is.plos.medicine", "is.plos.neglect.tropical", "is.plos.one", "is.plos.pathogens")
dat.transformed[,vars.factor] = colwise(factor)(dat[,vars.factor])

vars.to.transform = !(names(dat.transformed) %in% c(vars.no.transform, vars.factor))
dat.transformed[,vars.to.transform] = tr(dat[,vars.to.transform])


summary(dat.transformed)


############ Look at the data

library(Hmisc)
library(psych)
# both Hmisc and psych have a describe
Hmisc::describe(dat, listunique=0)
#latex(Hmisc::describe(dat, listunique=0))
#psych::describe(dat, interp=TRUE)


### HEATHER come back and do this part once I know

#dat.dots = cbind(dat, pubmed.journal=dat.raw$pubmed.journal, country.clean=dat.raw$country.clean)
#s = summary(dataset.in.geo.or.ae.int ~ pubmed.journal + country.clean, dat=dat.dots)
#s
#plot(s, cex.labels=0.1, cex=0.7)
#title=("Proportion of studies with datasets in GEO or ArrayExpress")
# This is a GREAT plot!  need to split the categories a bit better

# Like this one, but need fewer variables so it is more readable
#s = summary(dataset.in.geo.or.ae.int ~ ., dat=dat)
#tiff("heat-attributes.tiff", bg="white", width=880, height=1200)
#plot(s, cex.labels=0.1, cex=0.7)
#title=("Proportion of studies with datasets in GEO or ArrayExpress")
#dev.off()

########### Prep for statistics
# Heather, try something

#vars.exclude = c("doi")
#dat.indep.stats = dat.transformed[,!(names(dat.transformed) %in% vars.exclude)]
dat.indep.stats = dat.transformed[,vars.to.transform]


############## Get correlation matrix

# Need to do special sorts of correlations because have binary data, not just this:
# mycor = rcorr(as.matrix(dat.indep.stats))$r

library(polycor)
#myhetcorr = hetcor.modified(dat.indep.stats, use="complete.obs", std.err=FALSE, pd=FALSE)
myhetcorr = hetcor.modified(dat.indep.stats, use="pairwise.complete.obs", std.err=FALSE, pd=FALSE)
mycor.unadjusted = myhetcorr$correlations
#write.table(mycor.unadjusted,"/Users/hpiwowar/stats link/mycor.unadjusted.txt",append=F,quote=F,sep="\t",row.names=T)


# Are some correlations NA?
which(is.na(mycor.unadjusted))

#mycor.unadjusted[which(is.na(mycor.unadjusted))] = 1

# Now fix the correlation matrix if it is not positive-definite

mycor = adjust.to.positive.definite(mycor.unadjusted)

#display
library(gplots)
heatmap.2(mycor, col=bluered(32)[17:32], cexRow=0.9, cexCol = .9, symm = TRUE, dend = "row", trace = "none", main = "Thesis Data", margins=c(10,10), key=FALSE, keysize=0.1)

showpanel <- function(col) {
  image(z=matrix(1:100, ncol=1), col=col, xaxt="n", yaxt="n" )
}
#showpanel(bluered(32)[17:32])

#cm.colors(16)

# Now, for interest, display it with data sharing variables also
#mycor.all = hetcor.modified(dat, use="pairwise.complete.obs", std.err=FALSE, pd=FALSE)
#mycor.data.sharing = mycor.all$correlations["dataset.in.geo.or.ae",]
#mycor.data.sharing.relevant = mycor.data.sharing[!names(mycor.data.sharing) %in% c("pmid", "dataset.in.geo", "dataset.in.geo.or.ae", "dataset.in.geo.or.ae.int")]
#data.sharing.colours = colorpanel(20,low="red",high="green")[10 * (1 + round(mycor.data.sharing.relevant, 1))]
#heatmap.3(mycor, ColSideColors=data.sharing.colours, col=cm.colors, cexRow=0.5, cexCol = .8, symm = TRUE, dend = "row", trace = "none", main = "Thesis Data", margins=c(15,15), key=FALSE, keysize=0.1)

#pdf("heatmap.pdf", height=10, width=10)
#heatmap.2(mycor, col=bluered(16), cexRow=0.5, cexCol = .8, symm = TRUE, dend = "row", trace = "none", main = "Thesis Data", margins=c(15,15), key=FALSE, keysize=0.1)
#dev.off

############## FIRST ORDER ANALYSIS

##############  Determine number of First-Order factors

# Determine Number of Factors to Extract
library(nFactors)
eigenvectors.1st <- eigen(mycor) # get eigenvalues
# this line takes a long time
aparallel.1st <- parallel(subject=nrow(dat.indep.stats), var=ncol(dat.indep.stats), rep=100, cent=.05)
scree.results.1st <- nScree(eigenvectors.1st$values, aparallel.1st$eigen$qevpea)
summary(scree.results.1st)
plotnScree(scree.results.1st) 

# Pull out the "Optimal Coordinate"
# defined in nScree help page as 
# The optimal coordinates (OC) corresponds to an extrapolation of the preceding eigenvalue by a regression line between the eignvalue coordinates and the last eigenvalue coordinate
#number.factors.1st = scree.results.1st$Components$noc
number.factors.1st = 5


##############  Do First-Order Factor Analysis

# Maximum liklihood doesn't converge because too 
fit.ml = factanal(dat, number.factors.1st, rotation="promax", covmat=mycor)
print(fit.ml, sort=TRUE)


# Use princip axis when maximum liklihood fails to converge:
library(psych)
fit.fa.1st = fa(mycor, number.factors.1st, fm="minres", rotate="promax", 
                scores=FALSE, residuals=TRUE, n.obs=max(dim(dat.indep.stats)))

#to show the loadings sorted by absolute value
print(fit.fa.1st, sort=TRUE)

#fa.diagram(fit.fa.1st)
#cluster.plot(fit.fa.1st)

# look at residuals
#fit.fa.1st.residuals = fit.fa.1st$residual
#heatmap.2(fit.fa.1st.residuals, col=bluered(16), cexRow=0.5, cexCol = .8, symm = TRUE, dend = "row", trace = "none", main = "Thesis Data", margins=c(15,15), key=FALSE, keysize=0.1)


factor.names.1st = c(
"MR1"="Ratings",
"MR2"="Citations",
"MR3"="Views",
"MR4"="Bookmarks",
"MR5"="Notes")

for (afactor in names(factor.names.1st)) {
    print(paste(afactor, ": ", factor.names.1st[afactor], sep=""))
    print.thresh(fit.fa.1st$loadings[, afactor], .4, TRUE)
    print.thresh(fit.fa.1st$loadings[, afactor], -0.4, FALSE)
}



############## SECOND ORDER ANALYSIS

##############  Determine number of Second-Order factors
# Equations as per Factor Analysis, Second Edition, by Gorsuch

# Correlation of my first order results
fit.fa.1st.cor = fit.fa.1st$r.scores
colnames(fit.fa.1st.cor) = factor.names.1st[colnames(fit.fa.1st$loadings)]
rownames(fit.fa.1st.cor) = factor.names.1st[colnames(fit.fa.1st$loadings)]

eigenvectors.2nd <- eigen(fit.fa.1st.cor) # get eigenvalues
aparallel.2nd <- parallel(subject=nrow(fit.fa.1st.cor), var=ncol(fit.fa.1st.cor), rep=100, cent=.05)
scree.results.2nd <- nScree(eigenvectors.2nd$values, aparallel.2nd$eigen$qevpea)
scree.results.2nd
plotnScree(scree.results.2nd) 

#number.factors.2nd = scree.results.2nd$Components$noc
number.factors.2nd = 2


##############  Do Second-Order Factor Analysis

# Ideally uncorrelated, but want it to be a good fit
#fit.fa.2nd = fa(fit.fa.1st.cor, number.factors.2nd, fa="minres", rotate="varimax")
fit.fa.2nd = fa(fit.fa.1st.cor, number.factors.2nd, fa="minres", rotate="varimax")
print(fit.fa.2nd, sort=TRUE)

#fa.diagram(fit.fa.2nd)
#cluster.plot(fit.fa.2nd)

##############  Map variables directly to second order analysis
# Equations as per Factor Analysis, Second Edition, by Gorsuch

U = fit.fa.2nd$uniquenesses * diag(number.factors.1st)
P = fit.fa.2nd$loadings
A = cbind(P, U)
Pvf = fit.fa.1st$loadings

Pvo = Pvf %*% A

############# HAVE A LOOK AT THESE RESULTS

#Pvo[1:24, 1:4]
# Interesting:  last.author.num.prev.geoae.sharing.tr          0.134139157 -0.066308705  0.138307260 -0.04026715
# what about how it would correlate with our main endpoint?


factor.names.2nd = c(
"MR1"="Views, citations, bookmarks",
"MR2"="Notes and ratings")

# On original variables
for (afactor in names(factor.names.2nd)) {
    print(paste(afactor, ": ", factor.names.2nd[afactor], sep=""))    
    print.thresh(Pvo[, afactor], .3, TRUE)
    print.thresh(Pvo[, afactor], -0.3, FALSE)
}

# On first-order factors
for (afactor in names(factor.names.2nd)) {
    print(paste(afactor, ": ", factor.names.2nd[afactor], sep=""))
    print.thresh(fit.fa.2nd$loadings[, afactor], .3, TRUE)
    print.thresh(fit.fa.2nd$loadings[, afactor], -0.3, FALSE)
}
########   IMPUTE VARIABLES SO CAN CALCULATE SCORES


# Just want one output variables
#dat$dataset.in.geo.or.ae.int = dat.nums$in.ae.or.geo
#dat.impute.input = dat[,!names(dat) %in% c("dataset.in.geo", "dataset.in.geo.or.ae")]
dat.impute.input = dat.indep.stats

# Show the pattern of NAs
library(mice)
#md.pattern(dat.impute.input)

### Impute variables
mice.output = mice(dat.impute.input, m=1)  # singular

 
# Now flush out the rest of the scores 
dat.imputed = complete(mice.output, 1)
#dat.for.scores = dat.imputed[,!names(dat.imputed) %in% c("dataset.in.geo.or.ae.int")]
dat.for.scores = dat.imputed

###### COMPUTE FIRST ORDER SCORES

scores.1st = factor.scores.bartlett(dat.for.scores, fit.fa.1st)



####### MERGE WITH AIM3
load("/Users/hpiwowar/Documents/Code/hpiwowar/pypub/trunk/src/aim3/stats/dat_aim3.RData")

dat.plos = cbind(dat, scores.1st)

dat.merge = merge(dat.plos, dat.aim3, by="pmid")

dat.regress = data.frame(dataset.in.geo.or.ae.int = dat.merge$dataset.in.geo.or.ae.int)
score.names = dimnames(scores.1st)[[2]]
dat.regress[,score.names] = dat.merge[,names(dat.merge) %in% score.names]

vars.indep = c("days.since.aug.2009", 
"num.authors.tr",
"first.author.num.prev.pubs.tr", 
"first.author.num.prev.pmc.cites.tr",
"last.author.num.prev.pubs.tr",
"last.author.num.prev.pmc.cites.tr",
"num.grant.numbers.tr",
"pubmed.is.humans",
"pubmed.is.cancer",
"country.usa",
"institution.rank",
"nih.sum.sum.dollars.tr")

dat.regress[,vars.indep] = dat.merge[,vars.indep]



plot(MR2 ~ dataset.in.geo.or.ae.int, dat=dat.regress)
boxplot(MR2 ~ dataset.in.geo.or.ae.int, dat=dat.regress, main="MR2")
boxplot(log(MR1) ~ dataset.in.geo.or.ae.int, dat=dat.regress)
boxplot(log(MR2) ~ dataset.in.geo.or.ae.int, dat=dat.regress)
boxplot(log(MR3) ~ dataset.in.geo.or.ae.int, dat=dat.regress)
boxplot(log(MR4) ~ dataset.in.geo.or.ae.int, dat=dat.regress)
boxplot(log(MR5) ~ dataset.in.geo.or.ae.int, dat=dat.regress)


####### FIRST ORDER REGRESSION
# Not sure if this will be a primary result


# If I name them here, then setting up the regressions is more awkward due to long names
#names(dat.regress) = c("dataset.in.geo.or.ae.int", factor.names.1st[names(dat.regress)[-1]])

library(rms)
dd.regress = datadist(dat.regress)
options(datadist='dd.regress')
options(digits=2)

f.1st.nonlinear.interactions.reduced = ols(formula = MR2 ~ (dataset.in.geo.or.ae.int +
    (rcs(days.since.aug.2009, 10) +
    rcs(num.authors.tr, 3) ) +
    rcs(first.author.num.prev.pubs.tr, 3) + 
    rcs(first.author.num.prev.pmc.cites.tr, 3) +
    rcs(last.author.num.prev.pubs.tr, 3) + 
    rcs(last.author.num.prev.pmc.cites.tr, 3) + 
    as.numeric(pubmed.is.humans) + 
    as.numeric(pubmed.is.cancer) + 
    as.numeric(country.usa) )
#    rcs(institution.rank, 3) )
    ,
    dat=dat.regress, x=T, y=T)
anova(f.1st.nonlinear.interactions.reduced)

f.1st.nonlinear.interactions.reduced = ols(formula = MR2 ~ (dataset.in.geo.or.ae.int +
    rcs(days.since.aug.2009, 10) + 
    (rcs(num.authors.tr, 3) + 
    rcs(last.author.num.prev.pmc.cites.tr, 3) + 
    as.numeric(pubmed.is.humans) + 
    as.numeric(pubmed.is.cancer) + 
    as.numeric(country.usa) ) )
    #rcs(institution.rank, 3) )
    # which plos journal?
    # watch degrees of freedom!
    # try num.cites.scopus
    # read up about ols.  Am I requiring it to be linear?
    # is MR3 normal?  does it matter?  sqrt it or log it?
    ,
    dat=dat.regress, x=T, y=T)
anova(f.1st.nonlinear.interactions.reduced)

s = summary(f.1st.nonlinear.interactions.reduced)
s
plot(s)




summ.1st.nonlinear.interactions.reduced = summary(f.1st.nonlinear.interactions.reduced)
summ.1st.nonlinear.interactions.reduced.dimnames = dimnames(summ.1st.nonlinear.interactions.reduced)[[1]][seq(1,length(dimnames(summ.1st.nonlinear.interactions.reduced)[[1]]),2)]
dimnames(summ.1st.nonlinear.interactions.reduced)[[1]][seq(1,length(dimnames(summ.1st.nonlinear.interactions.reduced)[[1]]),2)] = factor.names.1st[summ.1st.nonlinear.interactions.reduced.dimnames]
plot(summ.1st.nonlinear.interactions.reduced, q = c(0.95), col=gray(0.5), log=T, cex=.8)
title("Multivariate nonlinear regressions with interactions")

### Dots of first-order factors
dat.regress.named = dat.regress
names(dat.regress.named) = c("dataset.in.geo.or.ae.int", factor.names.1st[names(dat.regress)[-1]])

dots.1st.nonlinear.interactions.reduced = summary(dataset.in.geo.or.ae.int ~ .,
    dat=dat.regress.named)
plot(dots.1st.nonlinear.interactions.reduced, cex.labels=0.5, cex=0.7, 
    xlab="Percentage of studies with datasets in GEO or ArrayExpress", 
    main="Univariate data sharing behaviour on first order factors")
#plot.summary.formula.response
#?summary.formula
			
######### SECOND ORDER REGRESSION


loadings.2nd = Pvo

#fit.pa.2nd.tovars = list(loadings=loadings.2nd[,(1+length(colnames(fit.pa.2nd$weights))):length(colnames(loadings.2nd))])
fit.pa.2nd.tovars = list(loadings=loadings.2nd[,colnames(fit.fa.2nd$loadings)])
fit.pa.2nd.tovars$uniquenesses = apply(fit.pa.2nd.tovars$loadings^2, 1, sum)

scores.to.dat.2nd = factor.scores.bartlett(dat.for.scores, fit.pa.2nd.tovars)

### HERE

dat.regress.2nd = data.frame(dataset.in.geo.or.ae.int = dat.merge$dataset.in.geo.or.ae.int) 
dat.regress.2nd[,dimnames(scores.to.dat.2nd)[[2]]] = scores.to.dat.2nd

library(rms)

dd.regress.2nd = datadist(dat.regress.2nd)
options(datadist='dd.regress.2nd')
options(digits=2)


f.2nd.nonlinear.interactions.reduced = lrm(formula = dataset.in.geo.or.ae.int ~ 
    rcs(MR3, 4) + (rcs(MR1, 4) + rcs(MR4, 4) + rcs(MR5, 4) + rcs(MR2, 4))^2,
    dat=dat.regress.2nd, x=T, y=T)
anova(f.2nd.nonlinear.interactions.reduced)

summ.2nd.nonlinear.interactions.reduced = summary(f.2nd.nonlinear.interactions.reduced)
summ.2nd.nonlinear.interactions.reduced.dimnames = dimnames(summ.2nd.nonlinear.interactions.reduced)[[1]][seq(1,length(dimnames(summ.2nd.nonlinear.interactions.reduced)[[1]]),2)]
dimnames(summ.2nd.nonlinear.interactions.reduced)[[1]][seq(1,length(dimnames(summ.2nd.nonlinear.interactions.reduced)[[1]]),2)] = factor.names.2nd[summ.2nd.nonlinear.interactions.reduced.dimnames]
plot(summ.2nd.nonlinear.interactions.reduced, q = c(0.95), col=gray(0.5), log=T, cex=.8)
title("Multivariate nonlinear regression with interactions")

### Dots of second-order factors
dat.regress.2nd.named = dat.regress.2nd
names(dat.regress.2nd.named) = c("dataset.in.geo.or.ae.int", factor.names.2nd[names(dat.regress.2nd)[-1]])

dots.2nd.nonlinear.interactions.reduced = summary(dataset.in.geo.or.ae.int ~ .,
    dat=dat.regress.2nd.named)
plot(dots.2nd.nonlinear.interactions.reduced, cex.labels=0.5, cex=0.7, 
    xlab="Percentage of studies with datasets in GEO or ArrayExpress", 
    main="Univariate data sharing behaviour\non second order factors")



#########  CLUSTERING

if (FALSE) {
    n <- nrow(dat.regress.2nd[,2:6])
    wss <- rep(0, 20)
    wss[1] <- (n - 1) * sum(apply(dat.regress.2nd[,2:6], 2, var))
    for (i in 2:20) {
    	wss[i] <- sum(kmeans(dat.regress.2nd[,2:6], centers = i)$withinss)
    }
    plot(1:20, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")

    k = 8
    km = kmeans(dat.regress.2nd[,2:6], centers=k)
    dat.explore.clusters.2nd = cbind(dat, dat.regress.2nd, km.cluster.2nd = km$cluster)
    cbind(aggregate(dat.explore.clusters.2nd[,125:130], by=list(kmeans(dat.regress.2nd[,2:6], centers=k)$cluster), mean), table(km$cluster))


    library(clValid)
    dat.cluster.2nd = dat.regress.2nd[,2:6]
    dat.cluster.2nd.tmp = dat.regress.2nd[1:100,2:6]
    stab = clValid(dat.cluster.2nd.tmp, 2:3, clMethods = c("hierarchical", "kmeans", "pam"), validation = "stability")
    optimalScores(stab)

    op <- par(no.readonly = TRUE)
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
    plot(stab, measure = c("APN", "AD", "ADM"), legend = FALSE)
    plot(nClusters(stab), measures(stab, "APN")[, , 1], type = "n", axes = F, xlab = "", ylab = "")
    legend("center", clusterMethods(stab), col = 1:9, lty = 1:9, pch = paste(1:9))
    par(op)

    stab.internal = clValid(dat.cluster.2nd.tmp, 2:3, clMethods = c("hierarchical", "kmeans", "pam"), validation = "internal")
    summary(stab.internal)
    op <- par(no.readonly = TRUE)
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
    plot(stab.internal, legend = FALSE)
    plot(nClusters(stab.internal), measures(stab.internal, "Dunn")[, , 1], type = "n", axes = F, xlab = "", ylab = "")
    legend("center", clusterMethods(stab.internal), col = 1:9, lty = 1:9, pch = paste(1:9))
    par(op)
}

library(tree)
g.smallest = tree(dataset.in.geo.or.ae.int ~ 
    rcs(MR1, 4) + rcs(MR2, 4) + rcs(MR3, 4) + rcs(MR4, 4) + rcs(MR5, 4),
    dat = dat.regress.2nd,
    control=tree.control(nobs=max(dim(dat.regress.2nd)), minsize=5))

g.bigger = tree(dataset.in.geo.or.ae.int ~ 
    rcs(MR1, 4) + rcs(MR2, 4) + rcs(MR3, 4) + rcs(MR4, 4) + rcs(MR5, 4),
    dat = dat.regress.2nd,
    control=tree.control(nobs=max(dim(dat.regress.2nd)), mindev=0.003))
    
g = g.smallest    
g
tree.screens()
plot(g, type="u")
text(g, digits=2)
pred.tree = predict(g)
pred.full.grouped = cut2(dat.regress.2nd$dataset.in.geo.or.ae.int, seq(0, 1, by=0.01)) 
tile.tree(g, pred.full.grouped)   
close.screen(all = TRUE)
    
# Just split on sharing and cancer, at medians:
# I think this is my take-away:
#aggregate(dat.regress.2nd$dataset.in.geo.or.ae.int, by=list(cut2(dat.regress.2nd$MR3, g=2), cut2(dat.regress.2nd$MR2, g=2)), mean)

#counts
numerator = aggregate(dat.regress.2nd$dataset.in.geo.or.ae.int, by=list(cut2(dat.regress.2nd$MR3, g=2), cut2(dat.regress.2nd$MR2, g=2)), function(x) sum(x==1))
denomerator = aggregate(dat.regress.2nd$dataset.in.geo.or.ae.int, by=list(cut2(dat.regress.2nd$MR3, g=2), cut2(dat.regress.2nd$MR2, g=2)), function(x) sum(x<2))

output.binconf = round(binconf(numerator$x, denomerator$x, method="wilson"), 2)

for (jj in 1:dim(output.binconf)[1]) {
    print(paste(output.binconf[jj,1], " [", output.binconf[jj,2], ", ", output.binconf[jj,3], "]", sep=""))
}

#write.table(dat.explore.clusters.2nd,"/Users/hpiwowar/stats link/at_explore_clusters_2nd.txt",append=F,quote=F,sep="\t",row.names=T)

#dim(dat.explore.clusters.2nd)
#names(dat.explore.clusters.2nd)



########

#save.image("image.RData")
#setwd("/Users/hpiwowar/Documents/Code/hpiwowar/pypub/trunk/src/aim3/stats")
#load("image.RData")

#setwd("/Users/hpiwowar/Documents/Code/hpiwowar/pypub/trunk/src/aim3/stats")
#for (iii in 1:4) source("aim3_stats_20100217b.R", echo=TRUE)
