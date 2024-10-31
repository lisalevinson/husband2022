#
# Delong Maze Analysis
#

library(lme4)
library(ggplot2)
library(lmerTest)
library(tidyverse)
library(emmeans)
library(logistf)
library(cowplot)

### Read in the raw data and pull apart into demographics, comprehension response data, and maze reading data

#directory <- "C:\\Users\\Matt\\Dropbox\\Research\\delong maze\\"
directory <- "C:\\Users\\cpgl0052\\Dropbox\\Research\\delong maze\\"

d <- read.csv(paste(directory,"data\\delong maze 40Ss.csv",sep=""), 
              header = 0, sep = ",", comment.char = "#", strip.white = T,
              col.names = c("Index","Time","Counter","Hash","Owner","Controller","Item","Element","Type","Group","FieldName","Value","WordNum","Word","Alt","WordOn","CorrWord","RT","Sent","TotalTime","Question","Resp","Acc","RespRT"));

demo <- d[d$Controller == "Form",1:12]
names(demo) <- c("Subject","MD5","TrialType","Number","Element","Experiment","Item","Field","Response","X","field","resp")
demo <- as.data.frame(lapply(demo, function (x) if (is.factor(x) | is.character(x)) factor(x) else x)) 

resp <- d[d$Controller == "Question" & substr(d$Type,1,4) != "prac", c(1:10,21:24)]
resp <- separate(data = resp, col = Type, into = c("exp", "item", "expect", "position", "pos", "cloze", "art.cloze", "n.cloze"), sep = "\\.", convert = TRUE, fill = "right")
resp <- as.data.frame(lapply(resp, function (x) if (is.factor(x) | is.character(x)) factor(x) else x))
resp$Acc <- as.numeric(as.character(resp$Acc))
resp$RespRT <- as.numeric(as.character(resp$RespRT))

rt <- d[d$Controller == "Maze" & substr(d$Type,1,4) != "prac", c(1:10,13:20)]
rt <- separate(data = rt, col = Type, into = c("exp", "item", "expect", "position", "pos", "cloze", "art.cloze", "n.cloze"), sep = "\\.", convert = TRUE, fill = "right")
rt <- as.data.frame(lapply(rt, function (x) if (is.factor(x) | is.character(x)) factor(x) else x))
rt$WordNum <- as.numeric(as.character(rt$WordNum))
rt$RT <- as.numeric(as.character(rt$RT))
rt$TotalTime <- as.numeric(as.character(rt$TotalTime))
rt$Acc <- as.numeric(as.character(recode(rt$CorrWord, yes = "1", no = "0")))
rt$n.cloze.scale <- scale(rt$n.cloze)
rt$art.cloze.scale <- scale(rt$art.cloze)

# Removing item 29 due to incorrect noun pairing
resp <- resp[resp$item != 29,]
rt <- rt[rt$item != 29,]

#Change directory for analysis
#directory <- "C:\\Users\\Matt\\Dropbox\\Research\\delong maze\\manuscript\\Revision 2\\"
directory <- "C:\\Users\\cpgl0052\\Dropbox\\Research\\delong maze\\manuscript\\Revision 2\\"

###

# Item cloze distributions

item.cloze <- rt %>% group_by(expect) %>% distinct(item, .keep_all = T) %>% arrange(item)

item.cloze %>% summarize(n=n(), min.art.cloze = min(art.cloze), max.art.cloze = max(art.cloze), mean.art.cloze = mean(art.cloze), med.art.cloze = median(art.cloze),
                         min.n.cloze = min(n.cloze), max.n.cloze = max(n.cloze), mean.n.cloze = mean(n.cloze), med.n.cloze = median(n.cloze))

item.cloze %>% group_by(expect) %>% summarize(n=n(), cor = cor(art.cloze, n.cloze))



# Demo checks

demo %>% filter(field == "age") %>% summarize(m.age = mean(as.numeric(as.character(resp))), 
                                              min.age = min(as.numeric(as.character(resp))), 
                                              max.age = max(as.numeric(as.character(resp))),
                                              sd.age = sd(as.numeric(as.character(resp))))

ggplot(demo[demo$field == "age",], aes(x = as.numeric(as.character(resp)))) + geom_histogram()

table(factor(demo[demo$field == "gender",]$resp))

###

### Comprehension question response analysis

resp %>% summarize(n=n(), acc=mean(Acc), acc.sd=sd(Acc), rt=mean(RespRT), rt.sd=sd(RespRT)) %>% as.data.frame()

resp %>% group_by(Hash) %>% summarize(n=n(), acc=mean(Acc), acc.sd=sd(Acc), rt=mean(RespRT), rt.sd=sd(RespRT)) %>% mutate(keep = acc > mean(acc)-2*sd(acc)) %>% arrange(acc) %>% as.data.frame()
#remove 1 subject at 52% accuracy - all others >70%

resp.s <- resp[resp$Hash != '9dAvrH0+R6a0U5adPzZSyA',]
resp.s %>% summarize(n=n(), acc=mean(Acc), rt=mean(RespRT)) %>% as.data.frame()

###

### Maze reading analysis

#Note: Rgn0 is article, Rgn1 is noun

rt.s <- rt[rt$Hash != '9dAvrH0+R6a0U5adPzZSyA',]

rt.s$rgn.fix <- rt.s$WordNum - rt.s$pos + 1
rt.s$word.num.z <- scale(rt.s$WordNum)
rt.s$word.len <- nchar(as.character(rt.s$Word))
rt.s$Altword.len <- nchar(as.character(rt.s$Alt))
contrasts(rt.s$expect) <- c(-.5,.5)

rt.s$item.expect <- paste(rt.s$item, rt.s$expect, sep=".")
delong.items <- rt.s %>% filter(rgn.fix == 0) %>% distinct(item.expect, .keep_all = TRUE)

ggplot(delong.items, aes(x = n.cloze, fill = expect)) + geom_histogram()
ggplot(delong.items, aes(x = art.cloze, fill = expect)) + geom_histogram()

ggplot(delong.items, aes(x = n.cloze, fill = Word)) + geom_histogram()
ggplot(delong.items, aes(x = art.cloze, fill = Word)) + geom_histogram()

ggplot(delong.items, aes(x = pos, fill = expect)) + geom_histogram()

#Response accuracy
rt.s %>% filter(rgn.fix > -4 & rgn.fix < 5) %>% summarize(n=n(), acc=mean(Acc), sd=sd(Acc), error=1-acc)
rt.s %>% filter(rgn.fix == 0) %>% summarize(n=n(), acc=mean(Acc), sd=sd(Acc), error=1-acc)
rt.s %>% filter(rgn.fix == 1) %>% summarize(n=n(), acc=mean(Acc), sd=sd(Acc), error=1-acc)
rt.s %>% filter(rgn.fix > -4 & rgn.fix < 4) %>% group_by(Hash) %>% summarize(n=n(), acc=mean(Acc), sd=sd(Acc), error=1-acc) %>% mutate(keep = acc > mean(acc)-2*sd(acc)) %>% arrange(acc) %>% as.data.frame()
#remove 2 (73.5% and 81.9%) - all others >90%

rt.s.filt <- rt.s[rt.s$Hash != "gyxidIf0fqXBM7nxg2K7SQ" & rt.s$Hash != "f8dC3CkleTBP9lUufzUOyQ",]

rt.s.filt %>% filter(rgn.fix > -4 & rgn.fix < 5) %>% summarize(n=n(), acc=mean(Acc), sd=sd(Acc), error=1-acc)
rt.s.filt %>% filter(rgn.fix == 0) %>% summarize(n=n(), acc=mean(Acc), sd=sd(Acc), error=1-acc)
rt.s.filt %>% filter(rgn.fix == 1) %>% summarize(n=n(), acc=mean(Acc), sd=sd(Acc), error=1-acc)

#Graph the raw Reading Error data
rgn.acc.raw <- rt.s.filt %>% filter(rgn.fix > -4 & rgn.fix < 5) %>% group_by(rgn.fix, expect) %>% summarize(n=n(), subj=length(unique(Hash)), err=1-mean(Acc), stderr=sd(Acc)/sqrt(subj)) %>% as.data.frame()
print(rgn.acc.raw, digits=2)
rgn.acc.raw$rgn <- as.factor(recode(rgn.acc.raw$rgn.fix, "-3"="CW-3", "-2"="CW-2", "-1"="CW-1", "0"="art", "1"="n","2"="CW+1", "3"="CW+2", "4"="CW+3"))
rgn.acc.raw$rgn <- ordered(rgn.acc.raw$rgn, levels = c("CW-3", "CW-2", "CW-1", "art", "n", "CW+1", "CW+2", "CW+3"))
ggplot(rgn.acc.raw, aes(x=rgn, y=err, group=expect, shape=expect)) +
  geom_line(stat = "identity", position=position_dodge(width=.3)) +
  geom_point(stat = "identity", position=position_dodge(width=.3)) +
  geom_errorbar(aes(ymin = err-stderr, ymax = err+stderr), width=.15, position=position_dodge(width=.3)) +
  ylim(0,1) +
  scale_shape_manual(name = "", labels = c("Expected", "Unexpected"), values = c(21,19)) + 
  xlab("Word") + ylab("Error Rate") +
  theme_bw(base_size=12)+ theme(legend.text=element_text(size=10))
ggsave(paste(directory,"delong maze Error by word.png",sep=""), dpi=300, height=70, width=190, units="mm")

#Analyze Reading Error Rates by region
acc.rgn3n.fm <- logistf(Acc ~ expect, data=rt.s.filt[rt.s.filt$rgn.fix == -3,])
summary(acc.rgn3n.fm)
acc.rgn2n.fm <- logistf(Acc ~ expect, data=rt.s.filt[rt.s.filt$rgn.fix == -2,])
summary(acc.rgn2n.fm)
acc.rgn1n.fm <- logistf(Acc ~ expect, data=rt.s.filt[rt.s.filt$rgn.fix == -1,])
summary(acc.rgn1n.fm)
acc.rgn0.fm <- logistf(Acc ~ expect, data=rt.s.filt[rt.s.filt$rgn.fix == 0,])
summary(acc.rgn0.fm)
acc.rgn1.fm <- logistf(Acc ~ expect, data=rt.s.filt[rt.s.filt$rgn.fix == 1,])
summary(acc.rgn1.fm)
acc.rgn2.fm <- logistf(Acc ~ expect, data=rt.s.filt[rt.s.filt$rgn.fix == 2,])
summary(acc.rgn2.fm)
acc.rgn3.fm <- logistf(Acc ~ expect, data=rt.s.filt[rt.s.filt$rgn.fix == 3,])
summary(acc.rgn3.fm)
acc.rgn4.fm <- logistf(Acc ~ expect, data=rt.s.filt[rt.s.filt$rgn.fix == 4,])
summary(acc.rgn4.fm)

#Analyze Response Times
rt.s.filt %>% filter(rgn.fix > -4 & rgn.fix < 5) %>% filter(Acc == 1) %>% summarize(n=n(), rt=mean(RT), rt.sd=sd(RT), med=median(RT), rt.min=min(RT), rt.max=max(RT))
rt.s.filt %>% filter(rgn.fix > -4 & rgn.fix < 5) %>% filter(Acc == 1) %>% group_by(Hash) %>% summarize(n=n(), rt=mean(RT), rt.sd=sd(RT), med=median(RT), rt.min=min(RT), rt.max=max(RT)) %>% mutate(keep = rt > mean(rt)-2*sd(rt) | rt < mean(rt)+2*sd(rt)) %>% as.data.frame()
#all Ss kept

#Filter out reading errors
rt.s.rgn <- rt.s.filt %>% filter(rgn.fix > -4 & rgn.fix < 5) %>% filter(Acc == 1) %>% as.data.frame()
hist(rt.s.rgn$RT, breaks=100)
hist(log(rt.s.rgn$RT), breaks=100)

#Graph raw (error free) RTs
rgn.rt.raw <- rt.s.filt %>% filter(rgn.fix > -4 & rgn.fix < 5) %>% filter(Acc == 1) %>% group_by(rgn.fix, expect) %>% summarize(n=n(), subj=length(unique(Hash)), rt=mean(RT), sd=sd(RT), stderr=sd/sqrt(subj)) %>% as.data.frame()
rgn.rt.raw$rgn <- as.factor(recode(rgn.rt.raw$rgn.fix, "-3"="CW-3", "-2"="CW-2", "-1"="CW-1", "0"="art", "1"="n","2"="CW+1", "3"="CW+2", "4"="CW+3"))
rgn.rt.raw$rgn <- ordered(rgn.rt.raw$rgn, levels = c("CW-3", "CW-2", "CW-1", "art", "n", "CW+1", "CW+2", "CW+3"))
ggplot(rgn.rt.raw, aes(x=rgn, y=rt, group=expect, shape=expect)) +
  geom_line(stat = "identity", position=position_dodge(width=.3)) +
  geom_point(stat = "identity", position=position_dodge(width=.3), size=3) +
  geom_errorbar(aes(ymin = rt-stderr, ymax = rt+stderr), width=.15, position=position_dodge(width=.3)) +
  scale_shape_manual(name="", labels=c("Expected", "Unexpected"), values = c(21,19)) + 
  xlab("Word") + ylab("Reading Time (msec)") + 
  theme_bw()

#Analyze untransformed RTs by region
rt.untrans.rgn3n.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == -3,])
summary(rt.untrans.rgn3n.m)
rt.untrans.rgn2n.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == -2,])
summary(rt.untrans.rgn2n.m)
rt.untrans.rgn1n.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == -1,])
summary(rt.untrans.rgn1n.m)
rt.untrans.rgn0.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 0,])
summary(rt.untrans.rgn0.m)
rt.untrans.rgn1.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 1,])
summary(rt.untrans.rgn1.m)
rt.untrans.rgn2.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 2,])
summary(rt.untrans.rgn2.m)
rt.untrans.rgn3.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 3,])
summary(rt.untrans.rgn3.m)
rt.untrans.rgn4.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 4,])
summary(rt.untrans.rgn4.m)

#Examine the singular fits
rePCA(rt.untrans.rgn0.m)
rt.untrans.rgn0.m.v2 <- lmer(RT ~ expect + (1|Hash) + (1|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 0,])
summary(rt.untrans.rgn0.m.v2) # full model Est: 46.20, intercept only Est: 46.258

rePCA(rt.untrans.rgn1.m)
rt.untrans.rgn1.m.v2 <- lmer(RT ~ expect + (1|Hash) + (1|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 1,])
summary(rt.untrans.rgn1.m.v2) # full model Est: 357.53, stderr 41.18; intercept only Est: 356.88, stderr: 14.46

#Pull out condition means
rt.untrans.rgn.emm <- rbind(as.data.frame(regrid(emmeans(rt.untrans.rgn3n.m, specs=c("expect")))),
                    as.data.frame(regrid(emmeans(rt.untrans.rgn2n.m, specs=c("expect")))),
                    as.data.frame(regrid(emmeans(rt.untrans.rgn1n.m, specs=c("expect")))),
                    as.data.frame(regrid(emmeans(rt.untrans.rgn0.m, specs=c("expect")))),
                    as.data.frame(regrid(emmeans(rt.untrans.rgn1.m, specs=c("expect")))),
                    as.data.frame(regrid(emmeans(rt.untrans.rgn2.m, specs=c("expect")))),
                    as.data.frame(regrid(emmeans(rt.untrans.rgn3.m, specs=c("expect")))),
                    as.data.frame(regrid(emmeans(rt.untrans.rgn4.m, specs=c("expect")))))
rgn.label <- c("CW-3","CW-3","CW-2","CW-2","CW-1","CW-1","art","art","n","n","CW+1","CW+1","CW+2","CW+2","CW+3","CW+3")
rt.untrans.rgn.emm <- cbind(rgn.label, rt.untrans.rgn.emm)
rt.untrans.rgn.emm$rgn.label <- ordered(rt.untrans.rgn.emm$rgn.label, levels = c("CW-3", "CW-2", "CW-1", "art", "n", "CW+1", "CW+2", "CW+3"))

#Calculate difference adjusted mixed effect model CIs
LMEMinterval <- function(
  formula,
  data,
  boot.type="percentile",
  conf=.95,
  nsim=NULL){

  # This convenience function calculates LMEM-based "confidence" intervals for
  #	a given design and dataset.
  # Parameters:
  #	formula: a usual model formula, with one DV and one or more IVs. Currently
  #		this function is able to handle functions in DVs (e.g., using log(RT)
  #		rather than RT as a DV), but not in IVs. And I haven't done much testing
  #		of DVs with functions so there may be bugs; I prefer just creating a
  #		new column in the data frame (e.g., creating a logRT column).
  #		Also note that this is currently only implemented for single-factor
  #		designs. If you have a factorial (e.g. 2x2) design, this function will
  #		collapse it into a single-factor (e.g. 1x4) design.
  #	data: a data frame with repeated measures data
  #	conf: The confidence level (between 0 and 1) for the CI. Defaults .95.
  #	boot.type: which type of bootstrap to use. Defaults to "percentile". If set
  #		to anything else, it will instead use normal bootstrap.
  #		Percentile bootstrap is more accurate but slower, as it requires more
  #     iterations to get accurate.
  #	nsim: Number of bootstrap replicates to use. By default this will be 2000 if
  #		boot.type=="percentile" and 200 otherwise, but you can set `nsim` to
  #		override that.

  # Load the lme4 and boot packages
  require( lme4 )
  require( boot )

  # Figure out the DV and the IVs.
  # This doesn't use all.var() because that strips away functions,
  #	whereas sometimes your formula might be like log(DV) ~ rather than just DV.
  vars <- rownames(attr(terms(formula),"factors"))

  # Figure out the DV
  DV <- vars[1]

  # Figure out what the random effects are. The first line finds which
  #	IVs look like random effects terms (which ones have pipes), and
  #	the next line grabs the stuff after the pipe
  ranef_idx <- which( unlist( lapply( vars, function(x){ length( grep("|", x, fixed=T ) ) } ) )>0 )
  grouping.vars <- unlist( lapply( vars[ranef_idx],
                                   function(x){ strsplit( x, " | ", fixed=T )[[1]][2] } ) )

  # Figure out the fixed IVs
  IVs <- vars[-c(1,ranef_idx)]

  # handles cases where the DV has a function around it (e.g. when the DV
  #	is `log(RT)` rather than just `RT`
  realDV <- all.vars(formula)[1]
  if( DV != realDV ){
    func <- gsub( paste0("(",realDV,")"), "", DV, fixed=T )
    DV <- realDV
    data[,DV] <- unlist(lapply( data[,DV], func ) )
  }

  ### A function to do the scaling. It first fits an intercept-only model to the
  ### data, then subtracts the residuals and adds the intercept (the grand mean)
  LMEscale <- function( formula, data ){
    model <- lmer( formula, data )
    data$LMEscale <- as.numeric( resid(model) + fixef(model)["(Intercept)"] )
    return(data)
  }

  # Scale the data, using a model with only a fixed intercept
  # and random intercepts
  lmerformula <- paste( DV, " ~ 1 + ", paste( "(1|", grouping.vars, ")", collapse=" + " ) )
  data <- LMEscale( lmerformula, data )

  ### The rest of the code handles making bootstrap CIs of the scaled data. The
  ###	general procedure is as follows: to get the bootstrap CIs we have to fit
  ###	an lmer	model to the scaled data. To make the models more likely to
  ###	converge, we want to fit the models without random correlation
  ###	parameters; to do this use a hack from https://rpubs.com/Reinhold/22193,
  ###	which requires first calculating a temporary model [which may not
  ###	converge] and then extracting dummy coefficients directly from
  ###	its model matrix to construct the good model.
  ###	Finally, we bootstrap the good model a lot of times and use the bootstrap
  ###	coefficients to get "confidence" intervals.

  # Collapse design into one factor (just treating each condition as its
  #	own condition, without considering main effects, interactions, etc.)
  data$Condition <- factor( do.call( paste0, lapply( IVs, function(IV){ data[,IV] } ) ) )

  # Create the temporary model, which may not converge, it doesn't matter
  lmerformula <- paste( "LMEscale ~ 0 + Condition + ", paste( "(1|", grouping.vars, ")", collapse=" + " ) )
  junkmodel <- lmer( lmerformula, data )

  # Pull out dummy variables from model matrix https://rpubs.com/Reinhold/22193
  mydummies <- list()
  for ( i in 1:dim( model.matrix(junkmodel) )[2] ) {
    data[,paste0("c",i)] <- model.matrix(junkmodel)[,i]
  }

  # Make random effect terms using the dummy variables rather than the big
  #	'Condition' variable. Per https://rpubs.com/Reinhold/22193, this ensures
  #	that random correlations between the random effects will not
  #  be used.
  #	We also specify no random intercepts; because the data are scaled, every
  #	subject/item/whatever should already have a mean of 0, so the random
  #	intercept is meaningless and in fact often cannot be fit anyway.
  ran <- paste( "0 +", paste( "c", 1:dim( model.matrix(junkmodel) )[2],
                              collapse="+", sep="" ) )

  # Now fit the good model. Because there is no fixed-effect intercept, it will
  #	estimate a coefficient for each condition, rather than estimating
  #	comparisons between conditions.
  lmerformula <- paste( "LMEscale ~ 0 + Condition + ", paste( "(", ran, "||", grouping.vars, ")", collapse=" + " ) )
  model <- lmer( lmerformula, data )

  # A function that gets the fixed-effect estimates from a model; this will
  #	be used for bootstrapping
  getcoef <- function(.){ getME(., "beta" ) }

  # Print a message so we know the function is going
  message( "Bootstrapping LME-scaled values, may be very slow..." )

  # Figures out the number of bootstrap replicates to use (unless the user
  #	already specified how many)
  if( is.null(nsim) ) {
    nsim <- ifelse( boot.type=="percentile", 2000, 200 )
  }

  # Sets some variables that will be needed depending on whether we do a
  #	percentile or normal bootstrap
  if (boot.type=="percentile" ) {
    ci_idx <- 4:5
    ci.type <- "percent"
  } else {
    ci_idx <- 2:3
    ci.type <- "normal"
  }

  # Bootstrap the model. This line is what takes time
  bigboo <- bootMer( model, getcoef, nsim=nsim )

  # Extracts the requested CIs from the bootstrap samples
  CIs <- do.call(rbind, lapply( 1:length(fixef(model)), function(x){ boot.ci( bigboo, index=x, type=substr(ci.type, 1, 4), conf=conf )[[ci.type]][ci_idx] } ) )

  # Gives human-friendly row names and column names
  rownames(CIs) <- substr( names( fixef(model) ), nchar("Condition")+1, nchar(names(fixef(model))) )
  colnames(CIs) <- c("Lower", "Upper")

  # Returns the CIs
  return(CIs)
}

rt.untrans.rgn3n.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == -3,])
rt.untrans.rgn2n.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == -2,])
rt.untrans.rgn1n.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == -1,])
rt.untrans.rgn0.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 0,])
rt.untrans.rgn1.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 1,])
rt.untrans.rgn2.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 2,])
rt.untrans.rgn3.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 3,])
rt.untrans.rgn4.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 4,])

rt.untrans.rgn.ci <- rbind(rt.untrans.rgn3n.ci, rt.untrans.rgn2n.ci, rt.untrans.rgn1n.ci, rt.untrans.rgn0.ci, rt.untrans.rgn1.ci, rt.untrans.rgn2.ci, rt.untrans.rgn3.ci, rt.untrans.rgn4.ci)

rt.untrans.rgn.emm.fix <- cbind(rt.untrans.rgn.emm, rt.untrans.rgn.ci)

rt.untrans.rgn.emm.fix$Lower.adj <- rt.untrans.rgn.emm.fix$emmean - ((rt.untrans.rgn.emm.fix$emmean - rt.untrans.rgn.emm.fix$Lower) * sqrt(2))
rt.untrans.rgn.emm.fix$Upper.adj <- rt.untrans.rgn.emm.fix$emmean + ((rt.untrans.rgn.emm.fix$Upper - rt.untrans.rgn.emm.fix$emmean) * sqrt(2))

#Plot RTs by region with CIs
rt.untrans.g <- ggplot(rt.untrans.rgn.emm.fix, aes(x=rgn.label, y=emmean, group=expect, shape=expect)) +
  geom_line(stat = "identity", position=position_dodge(width=.3)) +
  geom_point(stat = "identity", position=position_dodge(width=.3), size=2) +
  geom_errorbar(aes(ymin = Lower.adj, ymax = Upper.adj), width=.15, position=position_dodge(width=.3)) +
  scale_shape_manual(name = "", labels = c("Expected", "Unexpected"), values = c(21,19)) + 
  xlab("Word") + ylab("Response Time (msec)") +
  theme_bw(base_size=12)+ theme(legend.text=element_text(size=10))
rt.untrans.g
ggsave(paste(directory,"delong maze RT by word.png",sep=""), dpi=300, height=70, width=190, units="mm")

#Analyze by noun cloze
rt.untrans.rgn0.ncloze.m <- lmer(RT ~ n.cloze.scale + (1+n.cloze.scale|Hash) + (1+n.cloze.scale|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 0,])
summary(rt.untrans.rgn0.ncloze.m)
rt.untrans.rgn1.ncloze.m <- lmer(RT ~ n.cloze.scale + (1+n.cloze.scale|Hash) + (1+n.cloze.scale|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 1,])
summary(rt.untrans.rgn1.ncloze.m)

#Plot subject means and model means for article and noun
rt.raw.rgn0.subj <- rt.s.filt %>% filter(rgn.fix == 0) %>% filter(Acc == 1) %>% 
  group_by(Hash,expect) %>% summarise(rt=mean(RT))

art.rt.untrans.g <- ggplot() +
  geom_line(data=rt.raw.rgn0.subj, aes(x=expect, y=rt, group=Hash), stat = "identity", position=position_dodge(width=.1), alpha=.3) +
  geom_point(data=rt.raw.rgn0.subj, aes(x=expect, y=rt, group=Hash), stat = "identity", position=position_dodge(width=.1), size=.75, alpha=.3) +
  geom_point(data=rt.untrans.rgn.emm.fix[rt.untrans.rgn.emm.fix$rgn.label == "art",], aes(x=expect, y=emmean), stat="identity", size=2, position=position_dodge(), alpha=1) +
  geom_errorbar(data=rt.untrans.rgn.emm.fix[rt.untrans.rgn.emm.fix$rgn.label == "art",], aes(x=expect, ymin = Lower.adj, ymax = Upper.adj), width=.15, size=1, position=position_dodge(width=.3)) +
  ylab('Response Time (msec)') + scale_x_discrete(name = "Article", labels = c('Expected', 'Unexpected')) + theme_bw(base_size=12) + theme(legend.text=element_text(size=10))

rt.raw.rgn1.subj <- rt.s.filt %>% filter(rgn.fix == 1) %>% filter(Acc == 1) %>% 
  group_by(Hash,expect) %>% summarise(rt=mean(RT))

noun.rt.untrans.g <- ggplot() +
  geom_line(data=rt.raw.rgn1.subj, aes(x=expect, y=rt, group=Hash), stat = "identity", position=position_dodge(width=.1), alpha=.3) +
  geom_point(data=rt.raw.rgn1.subj, aes(x=expect, y=rt, group=Hash), stat = "identity", position=position_dodge(width=.1), size=.75, alpha=.3) +
  geom_point(data=rt.untrans.rgn.emm.fix[rt.untrans.rgn.emm.fix$rgn.label == "n",], aes(x=expect, y=emmean), stat="identity", size=2, position=position_dodge(), alpha=1) +
  geom_errorbar(data=rt.untrans.rgn.emm.fix[rt.untrans.rgn.emm.fix$rgn.label == "n",], aes(x=expect, ymin = Lower.adj, ymax = Upper.adj), width=.15, size=1, position=position_dodge(width=.3)) +
  ylab('Response Time (msec)') + scale_x_discrete(name = "Noun", labels = c('Expected', 'Unexpected')) + theme_bw(base_size=12) + theme(legend.text=element_text(size=10))

plot_grid(art.rt.untrans.g, noun.rt.untrans.g, labels="AUTO")
ggsave(paste(directory,"delong maze RT art-noun.png",sep=""), dpi=300, height=90, width=190, units="mm")


### Analysis RTs by subject mean reading time

subj.rts <- rt.s.filt %>% filter(rgn.fix > -4 & rgn.fix < 0) %>% filter(Acc == 1) %>% group_by(Hash) %>% summarise(subj.rt.mean = mean(RT)) %>% arrange(subj.rt.mean) %>% as.data.frame()
mean(subj.rts$subj.rt.mean)
subj.rts$subj.rt.mean.c <- subj.rts$subj.rt.mean - mean(subj.rts$subj.rt.mean)

rt.s.filt <- left_join(rt.s.filt, subj.rts, by="Hash")
rt.raw.rgn0.subj <- left_join(rt.raw.rgn0.subj, subj.rts, by="Hash")
rt.raw.rgn1.subj <- left_join(rt.raw.rgn1.subj, subj.rts, by="Hash")

#Analyze Article Rgn0 Untransformed with subject mean reading time
rt.untrans.rgn0.subj.rts.m <- lmer(RT ~ expect*subj.rt.mean.c + (1+expect|Hash) + (1+expect*subj.rt.mean.c|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 0,])
summary(rt.untrans.rgn0.subj.rts.m)

art.subj.rts.untrans.emmip <- emmip(rt.untrans.rgn0.subj.rts.m, expect ~ subj.rt.mean.c, cov.reduce = range)

art.part.rt.untrans.g <- ggplot() +
  geom_point(data=rt.raw.rgn0.subj, aes(x=subj.rt.mean, y=rt, group=expect, shape=expect), stat = "identity", position=position_dodge(width=.1), alpha=.3) +
  geom_line(data=rt.raw.rgn0.subj, aes(x=subj.rt.mean, y=rt, group=Hash), stat = "identity", alpha=.3) +
  geom_line(data=art.subj.rts.untrans.emmip$data, aes(x=subj.rt.mean.c + mean(subj.rts$subj.rt.mean), y=yvar, group=expect, linetype=expect), stat="identity", size=1) +
  scale_linetype_discrete(name="", labels=c("Expected", "Unexpected")) + 
  scale_shape_manual(name="", labels=c("Expected", "Unexpected"), values=c(21, 19)) +
  xlab("Participant Average Response Time (msec)") + ylab("RT on Article (msec)") + theme_bw(base_size=12) + theme(legend.position="bottom", legend.text=element_text(size=10))

#Analyze Noun Rgn1 Untransformed with subject mean reading time
rt.untrans.rgn1.subj.rts.m <- lmer(RT ~ expect*subj.rt.mean.c + (1+expect|Hash) + (1+expect*subj.rt.mean.c|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 1,])
summary(rt.untrans.rgn1.subj.rts.m)

noun.subj.rts.untrans.emmip <- emmip(rt.untrans.rgn1.subj.rts.m, expect ~ subj.rt.mean.c, cov.reduce = range)

noun.part.rt.untrans.g <- ggplot() +
  geom_point(data=rt.raw.rgn1.subj, aes(x=subj.rt.mean, y=rt, group=expect, shape=expect), stat = "identity", position=position_dodge(width=.1), alpha=.3) +
  geom_line(data=rt.raw.rgn1.subj, aes(x=subj.rt.mean, y=rt, group=Hash), stat = "identity", alpha=.3) +
  geom_line(data=noun.subj.rts.untrans.emmip$data, aes(x=subj.rt.mean.c + mean(subj.rts$subj.rt.mean), y=yvar, group=expect, linetype=expect), stat="identity", size=1) +
  scale_linetype_discrete(name="", labels=c("Expected", "Unexpected")) + 
  scale_shape_manual(name="", labels=c("Expected", "Unexpected"), values=c(21, 19)) +
  xlab("Participant Average Response Time (msec)") + ylab("RT on Noun (msec)") + theme_bw(base_size=12) + theme(legend.position="bottom", legend.text=element_text(size=10))

plot_grid(art.part.rt.untrans.g, noun.part.rt.untrans.g, labels="AUTO")
ggsave(paste(directory,"delong maze RT art and noun expect by subj avg rt.png",sep=""), dpi=300, height=90, width=190, units="mm")

#Analyze with noun cloze
rt.untrans.rgn0.subj.rts.ncloze.m <- lmer(RT ~ n.cloze.scale*subj.rt.mean.c + (1+n.cloze.scale|Hash) + (1+n.cloze.scale*subj.rt.mean.c|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 0,])
summary(rt.untrans.rgn0.subj.rts.ncloze.m)
rt.untrans.rgn1.subj.rts.ncloze.m <- lmer(RT ~ n.cloze.scale*subj.rt.mean.c + (1+n.cloze.scale|Hash) + (1+n.cloze.scale*subj.rt.mean.c|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 1,])
summary(rt.untrans.rgn1.subj.rts.ncloze.m)


#Investigate effects if top 5 slowest subjects for Article RT are removed (Article RT > 900 msec [~ +1.36sd])
rt.s.filt %>% filter(Acc == 1 & rgn.fix == 0) %>% group_by(Hash) %>% summarize(n=n(), art.avg = mean(RT), art.sd = sd(RT)) %>% arrange(-art.avg) %>% as.data.frame()

rt.s.filt %>% filter(Acc == 1 & rgn.fix == 0) %>% group_by(Hash) %>% summarize(n=n(), art.avg = mean(RT), art.sd = sd(RT)) %>% arrange(-art.avg) %>%
  summarize(art.avg.avg = mean(art.avg), art.avg.sd = sd(art.avg), ms900 = (900-art.avg.avg)/art.avg.sd) %>% as.data.frame()

rt.s.filt.subjartRT.filt <- rt.s.filt[rt.s.filt$Hash != "mZWaXixbbdck3XK51B4K6Q" & rt.s.filt$Hash != "q2XrvdNoCHp1YZCq7dN8HA" & rt.s.filt$Hash != "nCo9yIhDFSY1i66lqYSGaw" & rt.s.filt$Hash != "wDoGt7DLcwC57E8wIVcfoQ" & rt.s.filt$Hash != "7fXus/pCAdSJoGyMElmM5A",]

rt.untrans.subjartRT.filt.rgn0.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.subjartRT.filt[rt.s.filt.subjartRT.filt$Acc == 1 & rt.s.filt.subjartRT.filt$rgn.fix == 0,])
summary(rt.untrans.subjartRT.filt.rgn0.m)

subj.rts.subjartRT.filt <- rt.s.filt.subjartRT.filt %>% filter(rgn.fix > -4 & rgn.fix < 0) %>% filter(Acc == 1) %>% group_by(Hash) %>% summarise(subj.rt.subjartRT.mean = mean(RT)) %>% arrange(subj.rt.subjartRT.mean) %>% as.data.frame()
subj.rts.subjartRT.filt$subj.rt.subjartRT.mean.c <- subj.rts.subjartRT.filt$subj.rt.subjartRT.mean - mean(subj.rts.subjartRT.filt$subj.rt.subjartRT.mean)

rt.s.filt.subjartRT.filt <- left_join(rt.s.filt.subjartRT.filt, subj.rts.subjartRT.filt, by="Hash")

rt.untrans.subjartRT.filt.subj.rts.rgn0.m <- lmer(RT ~ expect*subj.rt.subjartRT.mean.c + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.subjartRT.filt[rt.s.filt.subjartRT.filt$Acc == 1 & rt.s.filt.subjartRT.filt$rgn.fix == 0,])
summary(rt.untrans.subjartRT.filt.subj.rts.rgn0.m)


#Investigate effects if top 3 slowest subjects are removed (subj.rt.mean > 900msec [~ +1.39sd])
subj.rts %>% arrange(-subj.rt.mean) %>% as.data.frame()

subj.rts %>% summarize(n=n(), avg=mean(subj.rt.mean), sd=sd(subj.rt.mean), ms900 = (900-avg)/sd)

rt.s.filt.subjRT.filt <- rt.s.filt[rt.s.filt$Hash != "q2XrvdNoCHp1YZCq7dN8HA" & rt.s.filt$Hash != "mZWaXixbbdck3XK51B4K6Q" & rt.s.filt$Hash != "nCo9yIhDFSY1i66lqYSGaw",]

rt.untrans.subjRT.filt.rgn0.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.subjRT.filt[rt.s.filt.subjRT.filt$Acc == 1 & rt.s.filt.subjRT.filt$rgn.fix == 0,])
summary(rt.untrans.subjRT.filt.rgn0.m)

subj.rts.subjRT.filt <- rt.s.filt.subjRT.filt %>% filter(rgn.fix > -4 & rgn.fix < 0) %>% filter(Acc == 1) %>% group_by(Hash) %>% summarise(subj.rt.subjRT.mean = mean(RT)) %>% arrange(subj.rt.subjRT.mean) %>% as.data.frame()
subj.rts.subjRT.filt$subj.rt.subjRT.mean.c <- subj.rts.subjRT.filt$subj.rt.subjRT.mean - mean(subj.rts.subjRT.filt$subj.rt.subjRT.mean)

rt.s.filt.subjRT.filt <- left_join(rt.s.filt.subjRT.filt, subj.rts.subjRT.filt, by="Hash")

rt.untrans.subjRT.filt.subj.rts.rgn0.m <- lmer(RT ~ expect*subj.rt.subjRT.mean.c + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.subjRT.filt[rt.s.filt.subjRT.filt$Acc == 1 & rt.s.filt.subjRT.filt$rgn.fix == 0,])
summary(rt.untrans.subjRT.filt.subj.rts.rgn0.m)



### Additional analyses

#Analysis including both error and correct responses
#Graph raw RTs (both with and without errors)
rgn.rt.all.raw <- rt.s.filt %>% filter(rgn.fix > -4 & rgn.fix < 5) %>% group_by(rgn.fix, expect) %>% summarize(n=n(), subj=length(unique(Hash)), rt=mean(RT), sd=sd(RT), stderr=sd/sqrt(subj)) %>% as.data.frame()
rgn.rt.all.raw$rgn <- as.factor(recode(rgn.rt.all.raw$rgn.fix, "-3"="CW-3", "-2"="CW-2", "-1"="CW-1", "0"="art", "1"="n","2"="CW+1", "3"="CW+2", "4"="CW+3"))
rgn.rt.all.raw$rgn <- ordered(rgn.rt.all.raw$rgn, levels = c("CW-3", "CW-2", "CW-1", "art", "n", "CW+1", "CW+2", "CW+3"))
ggplot(rgn.rt.all.raw, aes(x=rgn, y=rt, group=expect, shape=expect)) +
  geom_line(stat = "identity", position=position_dodge(width=.3)) +
  geom_point(stat = "identity", position=position_dodge(width=.3), size=3) +
  geom_errorbar(aes(ymin = rt-stderr, ymax = rt+stderr), width=.15, position=position_dodge(width=.3)) +
  scale_shape_manual(name="", labels=c("Expected", "Unexpected"), values = c(21,19)) + 
  xlab("Word") + ylab("Reading Time (msec)") + 
  theme_bw()

#Analyze untransformed RTs by region (both with and without errors)
rt.s.filt.all <- rt.s.filt %>% filter(rgn.fix > -4 & rgn.fix < 5) %>% as.data.frame()

rt.all.untrans.rgn3n.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == -3,])
summary(rt.all.untrans.rgn3n.m)
rt.all.untrans.rgn2n.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == -2,])
summary(rt.all.untrans.rgn2n.m)
rt.all.untrans.rgn1n.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == -1,])
summary(rt.all.untrans.rgn1n.m)
rt.all.untrans.rgn0.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == 0,])
summary(rt.all.untrans.rgn0.m)
rt.all.untrans.rgn1.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == 1,])
summary(rt.all.untrans.rgn1.m)
rt.all.untrans.rgn2.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == 2,])
summary(rt.all.untrans.rgn2.m)
rt.all.untrans.rgn3.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == 3,])
summary(rt.all.untrans.rgn3.m)
rt.all.untrans.rgn4.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == 4,])
summary(rt.all.untrans.rgn4.m)

#Pull out condition means (both with and without errors)
rt.all.untrans.rgn.emm <- rbind(as.data.frame(regrid(emmeans(rt.all.untrans.rgn3n.m, specs=c("expect")))),
                            as.data.frame(regrid(emmeans(rt.all.untrans.rgn2n.m, specs=c("expect")))),
                            as.data.frame(regrid(emmeans(rt.all.untrans.rgn1n.m, specs=c("expect")))),
                            as.data.frame(regrid(emmeans(rt.all.untrans.rgn0.m, specs=c("expect")))),
                            as.data.frame(regrid(emmeans(rt.all.untrans.rgn1.m, specs=c("expect")))),
                            as.data.frame(regrid(emmeans(rt.all.untrans.rgn2.m, specs=c("expect")))),
                            as.data.frame(regrid(emmeans(rt.all.untrans.rgn3.m, specs=c("expect")))),
                            as.data.frame(regrid(emmeans(rt.all.untrans.rgn4.m, specs=c("expect")))))
rgn.label <- c("CW-3","CW-3","CW-2","CW-2","CW-1","CW-1","art","art","n","n","CW+1","CW+1","CW+2","CW+2","CW+3","CW+3")
rt.all.untrans.rgn.emm <- cbind(rgn.label, rt.all.untrans.rgn.emm)
rt.all.untrans.rgn.emm$rgn.label <- ordered(rt.all.untrans.rgn.emm$rgn.label, levels = c("CW-3", "CW-2", "CW-1", "art", "n", "CW+1", "CW+2", "CW+3"))

rt.all.untrans.rgn3n.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == -3,])
rt.all.untrans.rgn2n.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == -2,])
rt.all.untrans.rgn1n.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == -1,])
rt.all.untrans.rgn0.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == 0,])
rt.all.untrans.rgn1.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == 1,])
rt.all.untrans.rgn2.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == 2,])
rt.all.untrans.rgn3.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == 3,])
rt.all.untrans.rgn4.ci <- LMEMinterval(RT ~ expect + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.all[rt.s.filt.all$rgn.fix == 4,])

rt.all.untrans.rgn.ci <- rbind(rt.all.untrans.rgn3n.ci, rt.all.untrans.rgn2n.ci, rt.all.untrans.rgn1n.ci, rt.all.untrans.rgn0.ci, rt.all.untrans.rgn1.ci, rt.all.untrans.rgn2.ci, rt.all.untrans.rgn3.ci, rt.all.untrans.rgn4.ci)

rt.all.untrans.rgn.emm.fix <- cbind(rt.all.untrans.rgn.emm, rt.all.untrans.rgn.ci)

rt.all.untrans.rgn.emm.fix$Lower.adj <- rt.all.untrans.rgn.emm.fix$emmean - ((rt.all.untrans.rgn.emm.fix$emmean - rt.all.untrans.rgn.emm.fix$Lower) * sqrt(2))
rt.all.untrans.rgn.emm.fix$Upper.adj <- rt.all.untrans.rgn.emm.fix$emmean + ((rt.all.untrans.rgn.emm.fix$Upper - rt.all.untrans.rgn.emm.fix$emmean) * sqrt(2))

#Plot RTs by region with CIs (both with and without errors)
rt.all.g <- ggplot(rt.all.untrans.rgn.emm.fix, aes(x=rgn.label, y=emmean, group=expect, shape=expect)) +
  geom_line(stat = "identity", position=position_dodge(width=.3)) +
  geom_point(stat = "identity", position=position_dodge(width=.3)) +
  geom_errorbar(aes(ymin = Lower.adj, ymax = Upper.adj), width=.15, position=position_dodge(width=.3)) +
  scale_shape_manual(name = "", labels = c("Expected", "Unexpected"), values = c(21,19)) + 
  xlab("Word") + ylab("Response Time (msec)") +
  theme_bw(base_size=12)+ theme(legend.text=element_text(size=10))
rt.all.g
ggsave(paste(directory,"delong maze RT error and correct by word.png",sep=""), dpi=300, height=70, width=190, units="mm")


#Analysis of noun RT given article RT
rt.s.filt.rgn0.rgn1 <- rt.s.filt %>% filter(rgn.fix > -1 & rgn.fix < 2) %>% 
  pivot_wider(id_col=c("Hash","item","expect"), names_from = c("rgn.fix"), values_from = c("Acc","RT")) %>% 
  filter(Acc_0 == 1 & Acc_1 == 1) %>% as.data.frame()

rt.s.filt.rgn0.rgn1$RT_0.c <- rt.s.filt.rgn0.rgn1$RT_0 - mean(rt.s.filt.rgn0.rgn1$RT_0)

rt.untrans.rgn0.rgn1.expect.m <- lmer(RT_1 ~ expect*RT_0.c + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.rgn0.rgn1)
summary(rt.untrans.rgn0.rgn1.expect.m)

emtrends(rt.untrans.rgn0.rgn1.expect.m, ~ expect, var = "RT_0.c")

rt.s.filt.rgn0.rgn1.subj.avg <- rt.s.filt.rgn0.rgn1 %>% group_by(Hash, expect) %>% summarize(RT_0.avg = mean(RT_0), RT_1.avg = mean(RT_1)) %>% as.data.frame()
min(rt.s.filt.rgn0.rgn1.subj.avg$RT_0.avg)- mean(rt.s.filt.rgn0.rgn1$RT_0)
max(rt.s.filt.rgn0.rgn1.subj.avg$RT_0.avg)- mean(rt.s.filt.rgn0.rgn1$RT_0)

rt.untrans.rgn0.rgn1.expect.emmip <- emmip(rt.untrans.rgn0.rgn1.expect.m, expect ~ RT_0.c, cov.keep = 2, at = list(RT_0.c = c(min(rt.s.filt.rgn0.rgn1.subj.avg$RT_0.avg)- mean(rt.s.filt.rgn0.rgn1$RT_0), max(rt.s.filt.rgn0.rgn1.subj.avg$RT_0.avg)- mean(rt.s.filt.rgn0.rgn1$RT_0))))

rt.untrans.rgn0.rgn1.expect.g <- ggplot() +
  geom_point(data=rt.s.filt.rgn0.rgn1.subj.avg, aes(x=RT_0.avg, y=RT_1.avg, group=expect, shape=expect), stat = "identity", position=position_dodge(width=.1), alpha=.3) +
  #geom_line(data=rt.s.filt.rgn0.rgn1.subj.avg, aes(x=RT_0.avg, y=RT_1.avg, group=Hash), stat = "identity", alpha=.3) +
  geom_line(data=rt.untrans.rgn0.rgn1.expect.emmip$data, aes(x=RT_0.c + mean(rt.s.filt.rgn0.rgn1$RT_0), y=yvar, group=expect, linetype=expect), stat="identity", size=1) +
  scale_linetype_discrete(name="", labels=c("Expected", "Unexpected")) + 
  scale_shape_manual(name="", labels=c("Expected", "Unexpected"), values=c(21, 19)) +
  xlab("RT on Article (msec)") + ylab("RT on Noun (msec)") + theme_bw(base_size=12) + theme(legend.position="bottom", legend.text=element_text(size=10))
rt.untrans.rgn0.rgn1.expect.g
ggsave(paste(directory,"delong maze noun RT given article RT.png",sep=""), dpi=300, height=70, width=190, units="mm")


#Analysis including WordForm a/an
rt.s.filt.rgn0 <- rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 0,]
rt.s.filt.rgn0 <- as.data.frame(lapply(rt.s.filt.rgn0, function (x) if (is.factor(x) | is.character(x)) factor(x) else x))
contrasts(rt.s.filt.rgn0$Word) <- c(-.5,.5)
contrasts(rt.s.filt.rgn0$expect) <- c(-.5,.5)

rt.untrans.word.form.rgn0.m <- lmer(RT ~ expect*Word + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.rgn0)
summary(rt.untrans.word.form.rgn0.m)

rt.untrans.word.form.rgn0.emm <- emmeans(rt.untrans.word.form.rgn0.m, specs = c("Word", "expect"))
pairs(rt.untrans.word.form.rgn0.emm, simple = "expect")

#rt.untrans.word.form.rgn0.ci <- LMEMinterval(RT ~ expect*Word + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.rgn0)
rt.untrans.word.form.rgn0.emm.df <- as.data.frame(rt.untrans.word.form.rgn0.emm)
#rt.untrans.word.form.rgn0.emm.df <- cbind(rt.untrans.word.form.rgn0.emm.df, rt.untrans.word.form.rgn0.ci)

rt.untrans.word.form.rng0.g <- ggplot(rt.untrans.word.form.rgn0.emm.df, aes(x=Word, y=emmean, fill=expect)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  #geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.15, position=position_dodge(width=.3)) +
  xlab("Article Form") + ylab("RT on Article (msec)") + 
  scale_fill_grey(name="", labels=c("Expected", "Unexpected"), start = 0, end = .75) +
  theme_bw(base_size=12)+ theme(legend.text=element_text(size=10))
rt.untrans.word.form.rng0.g
ggsave(paste(directory,"delong maze article RT given article form.png",sep=""), dpi=300, height=70, width=190, units="mm")

rt.s.filt.rgn1 <- rt.s.filt %>% filter(rgn.fix > -1 & rgn.fix < 2) %>% 
  pivot_wider(id_col=c("Hash","item","Word","expect"), names_from = c("rgn.fix"), values_from = c("Word","Acc","RT")) %>% 
  filter(Acc_0 == 1 & Acc_1 == 1) %>% as.data.frame()

rt.s.filt.rgn1$Word_0 <- factor(rt.s.filt.rgn1$Word_0)
contrasts(rt.s.filt.rgn1$Word_0) <- c(-.5,.5)

rt.untrans.word.form.rgn1.m <- lmer(RT_1 ~ expect*Word_0 + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.rgn1)
summary(rt.untrans.word.form.rgn1.m)

rt.untrans.word.form.rgn1.emm <- emmeans(rt.untrans.word.form.rgn1.m, specs = c("Word_0", "expect"))
pairs(rt.untrans.word.form.rgn1.emm, simple = "expect")


#Analysis including WordPosition

#Analyze Article Rgn0 Untransformed with word pos
rt.s.filt$pos.c <- rt.s.filt$pos - mean(rt.s.filt$pos)

rt.untrans.rgn0.word.pos.m <- lmer(RT ~ expect*pos.c + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 0,])
summary(rt.untrans.rgn0.word.pos.m)

emtrends(rt.untrans.rgn0.word.pos.m, ~ expect, var = "pos.c")

art.word.pos.untrans.emmip <- emmip(rt.untrans.rgn0.word.pos.m, expect ~ pos.c, cov.reduce = range)

word.pos.rts.rgn0 <- rt.s.filt %>% filter(rgn.fix == 0) %>% filter(Acc == 1) %>% group_by(pos, expect) %>% summarise(word.pos.mean = mean(RT), word.pos.med = median(RT)) %>% as.data.frame()

art.part.word.pos.untrans.g <- ggplot() +
  geom_point(data=word.pos.rts.rgn0, aes(x=pos, y=word.pos.mean, group=expect, shape=expect), stat = "identity", position=position_dodge(width=.1), alpha=.3) +
  geom_line(data=word.pos.rts.rgn0, aes(x=pos, y=word.pos.mean, group=pos), stat = "identity", alpha=.3) +
  geom_line(data=art.word.pos.untrans.emmip$data, aes(x=pos.c + mean(rt.s.filt$pos), y=yvar, group=expect, linetype=expect), stat="identity", size=1) +
  scale_linetype_discrete(name="", labels=c("Expected", "Unexpected")) + 
  scale_shape_manual(name="", labels=c("Expected", "Unexpected"), values=c(21, 19)) +
  xlab("Article Position") + ylab("RT on Article (msec)") + theme_bw(base_size=12) + theme(legend.position="right", legend.text=element_text(size=10))
art.part.word.pos.untrans.g
ggsave(paste(directory,"delong maze article RT given article position.png",sep=""), dpi=300, height=70, width=190, units="mm")

rt.untrans.rgn1.word.pos.m <- lmer(RT ~ expect*pos.c + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 1,])
summary(rt.untrans.rgn1.word.pos.m)


#Analysis including Alternative Word properties
#Alternative Word Identity
delong.items.rgn0 <- delong.items[delong.items$rgn.fix == 0,]
delong.items.rgn0$Alt <- factor(delong.items.rgn0$Alt)
table(delong.items.rgn0$Alt)
delong.items.rgn0 %>% group_by(Alt) %>% summarize(n=n()) %>% arrange(-n)
delong.items.rgn0 %>% group_by(Alt) %>% summarize(n=n()) %>% arrange(-n) %>% group_by(n) %>% summarize(count = n())

delong.items.rgn0$Alt <- relevel(delong.items.rgn0$Alt, "etc")
contrasts(delong.items.rgn0$Alt)

#rt.untrans.altword.id.rgn0.m <- lmer(RT ~ expect + (1+expect|Hash) + (1+expect|item) + (1+expect|Alt), data=rt.s.filt.rgn0)
#summary(rt.untrans.altword.id.rgn0.m)

rt.untrans.altword.fac.rgn0.m <- lmer(RT ~ expect*Alt + (1+expect|Hash) + (1+expect|item), data=rt.s.filt.rgn0)
anova(rt.untrans.altword.fac.rgn0.m)

rt.untrans.altword.fac.rgn0.emm <- emmeans(rt.untrans.altword.fac.rgn0.m, specs = c("expect", "Alt"))
rt.untrans.altword.fac.rgn0.emm.pairs <- pairs(rt.untrans.altword.fac.rgn0.emm, simple = "expect")

rt.untrans.altword.fac.rgn0.g <- plot(rt.untrans.altword.fac.rgn0.emm.pairs[c(11,18,10,5,7,26,31,34,39)]) + 
  geom_vline(xintercept = 0, linetype="dashed") +
  theme_bw(base_size=12) +
  #theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  #theme(strip.text.y = element_text(angle=0)) +
  scale_y_discrete(name = "Alternative Word", labels = c("etc (14)","ha (10)","el (8)","bet (6)","cent (6)","lake (6)","met (6)","oh (6)","sir (6)")) +
  xlab("Estimated Mean Difference (Expected - Unexpected)")
rt.untrans.altword.fac.rgn0.g
ggsave(paste(directory,"delong maze article RT given alt word.png",sep=""), dpi=300, height=70, width=190, units="mm")


#Alternative Word Length
rt.s.filt.rgn0$alt.word.len.c <- rt.s.filt.rgn0$Altword.len - mean(rt.s.filt.rgn0$Altword.len)

altword.len.rts.rgn0 <- rt.s.filt.rgn0 %>% group_by(Altword.len, expect) %>% summarise(altword.len.mean = mean(RT), altword.len.med = median(RT)) %>% as.data.frame()

rt.untrans.altword.len.rgn0.m <- lmer(RT ~ expect*alt.word.len.c + (1+expect|Hash) + (1|item), data=rt.s.filt.rgn0)
summary(rt.untrans.altword.len.rgn0.m)

art.altword.len.untrans.emmip <- emmip(rt.untrans.altword.len.rgn0.m, expect ~ alt.word.len.c, cov.reduce = range)

art.part.altword.len.untrans.g <- ggplot() +
  geom_point(data=altword.len.rts.rgn0, aes(x=Altword.len, y=altword.len.mean, group=expect, shape=expect), stat = "identity", alpha=.3) +
  geom_line(data=altword.len.rts.rgn0, aes(x=Altword.len, y=altword.len.mean, group=Altword.len), stat = "identity", alpha=.3) +
  geom_line(data=art.altword.len.untrans.emmip$data, aes(x=alt.word.len.c + mean(rt.s.filt.rgn0$Altword.len), y=yvar, group=expect, linetype=expect), stat="identity", size=1) +
  scale_linetype_discrete(name="", labels=c("Expected", "Unexpected")) + 
  scale_shape_manual(name="", labels=c("Expected", "Unexpected"), values=c(21, 19)) +
  scale_x_continuous(breaks = c(2,3,4)) +
  xlab("Alternative Word Length") + ylab("RT on Article (msec)") + theme_bw(base_size=12) + theme(legend.position="right", legend.text=element_text(size=10))
art.part.altword.len.untrans.g
ggsave(paste(directory,"delong maze article RT given alt word length.png",sep=""), dpi=300, height=70, width=190, units="mm")


#Analysis including Age
subj.age <- demo %>% filter(field == "age") %>% group_by(Number) %>% summarize(age = as.numeric(as.character(resp))) %>% as.data.frame()
names(subj.age) <- c("Hash", "age")
subj.age$age.c <- subj.age$age - mean(subj.age$age)

rt.s.filt <- left_join(rt.s.filt, subj.age, by="Hash")

subj.exp.rts <- rt.s.filt %>% filter(rgn.fix == 0) %>% filter(Acc == 1) %>% group_by(Hash, expect) %>% summarize(rt.m = mean(RT)) %>% pivot_wider(names_from = expect, values_from = rt.m) %>% mutate(diff = unexpected - expected)
left_join(subj.exp.rts, rt.s.filt[,c("Hash","age","subj.rt.mean")], by = "Hash") %>% distinct(Hash, .keep_all = T) %>% select(Hash, age, subj.rt.mean, expected, unexpected, diff) %>% arrange(-diff) %>% as.data.frame()

rt.untrans.rgn0.age.m <- lmer(RT ~ expect*age.c + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 0,])
summary(rt.untrans.rgn0.age.m)

age.rts.rgn0 <- rt.s.filt %>% filter(rgn.fix == 0) %>% filter(Acc == 1) %>% group_by(age, expect) %>% summarize(age.mean = mean(RT)) %>% as.data.frame()

art.age.untrans.emmip <- emmip(rt.untrans.rgn0.age.m, expect ~ age.c, cov.reduce = range)

art.part.age.untrans.g <- ggplot() +
  geom_point(data=age.rts.rgn0, aes(x=age, y=age.mean, group=expect, shape=expect), stat = "identity", position=position_dodge(width=.1), alpha=.3) +
  geom_line(data=age.rts.rgn0, aes(x=age, y=age.mean, group=age), stat = "identity", alpha=.3) +
  geom_line(data=art.age.untrans.emmip$data, aes(x=age.c + mean(subj.age$age), y=yvar, group=expect, linetype=expect), stat="identity", size=1) +
  scale_linetype_discrete(name="", labels=c("Expected", "Unexpected")) + 
  scale_shape_manual(name="", labels=c("Expected", "Unexpected"), values=c(21, 19)) +
  xlab("Participant Age") + ylab("RT on Article (msec)") + theme_bw(base_size=12) + theme(legend.position="bottom", legend.text=element_text(size=10))
art.part.age.untrans.g
ggsave(paste(directory,"delong maze article RT given age.png",sep=""), dpi=300, height=70, width=190, units="mm")

rt.untrans.rgn1.age.m <- lmer(RT ~ expect*age.c + (1+expect|Hash) + (1+expect|item), data=rt.s.filt[rt.s.filt$Acc == 1 & rt.s.filt$rgn.fix == 1,])
summary(rt.untrans.rgn1.age.m)

age.rts.rgn1 <- rt.s.filt %>% filter(rgn.fix == 1) %>% filter(Acc == 1) %>% group_by(age, expect) %>% summarise(age.mean = mean(RT), age.med = median(RT)) %>% as.data.frame()

noun.age.untrans.emmip <- emmip(rt.untrans.rgn1.age.m, expect ~ age.c, cov.reduce = range)

noun.part.age.untrans.g <- ggplot() +
  geom_point(data=age.rts.rgn1, aes(x=age, y=age.mean, group=expect, shape=expect), stat = "identity", position=position_dodge(width=.1), alpha=.3) +
  geom_line(data=age.rts.rgn1, aes(x=age, y=age.mean, group=age), stat = "identity", alpha=.3) +
  geom_line(data=noun.age.untrans.emmip$data, aes(x=age.c + mean(subj.age$age), y=yvar, group=expect, linetype=expect), stat="identity", size=1) +
  scale_linetype_discrete(name="", labels=c("Expected", "Unexpected")) + 
  scale_shape_manual(name="", labels=c("Expected", "Unexpected"), values=c(21, 19)) +
  xlab("Participant Age") + ylab("RT on Noun (msec)") + theme_bw(base_size=12) + theme(legend.position="bottom", legend.text=element_text(size=10))
noun.part.age.untrans.g
ggsave(paste(directory,"delong maze noun RT given age.png",sep=""), dpi=300, height=70, width=190, units="mm")


### Other calculations

# Correlation between Noun and CW+1 RTs
rt.s.filt.rgn1.rgn2 <- rt.s.filt %>% filter(rgn.fix > 0 & rgn.fix < 3) %>% 
  pivot_wider(id_col=c("Hash","item","expect"), names_from = c("rgn.fix"), values_from = c("Acc","RT")) %>% 
  filter(Acc_1 == 1 & Acc_2 == 1) %>% as.data.frame()

cor(rt.s.filt.rgn1.rgn2$RT_1, rt.s.filt.rgn1.rgn2$RT_2)

rt.s.filt.rgn1.rgn2$RT_1.c <- rt.s.filt.rgn1.rgn2$RT_1 - mean(rt.s.filt.rgn1.rgn2$RT_1)

rt.untrans.rgn1.rgn2.expect.m <- lmer(RT_2 ~ expect*RT_1.c + (1+expect*RT_1.c|Hash) + (1+expect*RT_1.c|item), data=rt.s.filt.rgn1.rgn2)
summary(rt.untrans.rgn1.rgn2.expect.m)

emtrends(rt.untrans.rgn1.rgn2.expect.m, ~ expect, var = "RT_1.c")
