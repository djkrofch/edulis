library(ggplot2)
library(reshape)
library(qpcR)
library(glmulti)

modelData <- read.table('LMdataComplete_newv2.csv',head = TRUE, sep = ',')

modelData <- modelData[modelData$SITE == 'Control', ]
modelData <- modelData[abs(modelData$ANGLE) <= 7, ]
#modelData <- modelData[modelData$YEAR < '2011', ]

NDVI_LS0  <- lm(modelData$GPP ~ modelData$RG * modelData$TA * modelData$NDVILS)
NDVILS <- glmulti(NDVI_LS0)
NDVI_LS <- lm(paste(summary(NDVILS)$bestmodel, ' +modelData$TA + modelData$RG + modelData$NDVILS'))
NDVI_LSw0 <- lm(modelData$GPP ~ modelData$RG * modelData$TA * modelData$NDVILS * modelData$NDWI)
NDVILSw <- glmulti(NDVI_LSw0)
NDVI_LSw <- lm(paste(summary(NDVILSw)$bestmodel, ' +modelData$TA + modelData$RG + modelData$NDVILS + modelData$NDWI'))

NDVI_RE0  <- lm(modelData$GPP ~ modelData$RG * modelData$TA * modelData$NDVIRE)
NDVIRE <- glmulti(NDVI_RE0)
NDVI_RE <- lm(paste(summary(NDVIRE)$bestmodel, ' +modelData$TA + modelData$RG + modelData$NDVILS'))
NDVI_REw0  <- lm(modelData$GPP ~ modelData$RG * modelData$TA * modelData$NDVIRE * modelData$NDWI)
NDVIREw <- glmulti(NDVI_REw0)
NDVI_REw <- lm(paste(summary(NDVIREw)$bestmodel, ' +modelData$TA + modelData$RG + modelData$NDVIRE + modelData$NDWI'))

NDRE_RE0  <- lm(modelData$GPP ~ modelData$RG * modelData$TA * modelData$NDRE)
NDRE <- glmulti(NDRE_RE0)
NDRE_RE <- lm(paste(summary(NDRE)$bestmodel, ' +modelData$TA + modelData$RG + modelData$NDRE'))

NDRE_REw0 <- lm(modelData$GPP ~ modelData$RG * modelData$TA * modelData$NDRE * modelData$NDWI)
NDREw <- glmulti(NDRE_REw0)
NDRE_REw <- lm(paste(summary(NDREw)$bestmodel, ' +modelData$TA + modelData$RG + modelData$NDVIRE + modelData$NDWI'))

outputC <- data.frame(modelData[, 1:7], NDVILS = fitted(NDVI_LS), NDVILSW = fitted(NDVI_LSw),
                NDVIRE = fitted(NDVI_RE), NDVIREW = fitted(NDVI_REw), NDRE = fitted(NDRE_RE),
                NDREW = fitted(NDRE_REw))

AICsC <- akaike.weights(c(AICc(NDVI_LS), AICc(NDVI_LSw), AICc(NDVI_RE), AICc(NDVI_REw), AICc(NDRE_RE), AICc(NDRE_REw)))

labs <- matrix(nrow = 6, ncol = 4) 
labs[1,1] <- summary(NDVI_LS)$adj.r.squared
labs[2,1] <- summary(NDVI_LSw)$adj.r.squared
labs[3,1] <- summary(NDVI_RE)$adj.r.squared
labs[4,1] <- summary(NDVI_REw)$adj.r.squared
labs[5,1] <- summary(NDRE_RE)$adj.r.squared
labs[6,1] <- summary(NDRE_REw)$adj.r.squared
labs[1,2] <- AICsC$deltaAIC[1]
labs[2,2] <- AICsC$deltaAIC[2]
labs[3,2] <- AICsC$deltaAIC[3]
labs[4,2] <- AICsC$deltaAIC[4]
labs[5,2] <- AICsC$deltaAIC[5]
labs[6,2] <- AICsC$deltaAIC[6]
labs[1,3] <- AICsC$rel.LL[1]
labs[2,3] <- AICsC$rel.LL[2]
labs[3,3] <- AICsC$rel.LL[3]
labs[4,3] <- AICsC$rel.LL[4]
labs[5,3] <- AICsC$rel.LL[5]
labs[6,3] <- AICsC$rel.LL[6]
labs[1,4] <- AICsC$weights[1]
labs[2,4] <- AICsC$weights[2]
labs[3,4] <- AICsC$weights[3]
labs[4,4] <- AICsC$weights[4]
labs[5,4] <- AICsC$weights[5]
labs[6,4] <- AICsC$weights[6]
labs <- data.frame(labs)
labsC <- data.frame(labs)
names(labs) <- c('R2adj','deltaAIC','relLL','weights')

fittextC <- data.frame(x = 1, y = 3, SITE = 'Control', MODEL = c('NDVILS','NDVILSW','NDVIRE','NDVIREW',
                        'NDRE','NDREW'), fits = round(labs$R2adj, digits = 3)) 

modelData <- read.table('LMdataComplete_newv2.csv',head = TRUE, sep = ',')

modelData <- modelData[modelData$SITE == 'Girdle', ]
modelData <- modelData[abs(modelData$ANGLE) <= 7, ]
#modelData <- modelData[modelData$YEAR < '2011', ]
#ggplot(modelData, aes(NDVILS, GPP)) + geom_point()

NDVI_LS0  <- lm(modelData$GPP ~ modelData$RG * modelData$TA * modelData$NDVILS)
NDVILS <- glmulti(NDVI_LS0)
NDVI_LS <- lm(paste(summary(NDVILS)$bestmodel, ' +modelData$TA + modelData$RG + modelData$NDVILS'))
NDVI_LSw0 <- lm(modelData$GPP ~ modelData$RG * modelData$TA * modelData$NDVILS * modelData$NDWI)
NDVILSw <- glmulti(NDVI_LSw0)
NDVI_LSw <- lm(paste(summary(NDVILSw)$bestmodel, ' +modelData$TA + modelData$RG + modelData$NDVILS + modelData$NDWI'))

NDVI_RE0  <- lm(modelData$GPP ~ modelData$RG * modelData$TA * modelData$NDVIRE)
NDVIRE <- glmulti(NDVI_RE0)
NDVI_RE <- lm(paste(summary(NDVIRE)$bestmodel, ' +modelData$TA + modelData$RG + modelData$NDVILS'))
NDVI_REw0  <- lm(modelData$GPP ~ modelData$RG * modelData$TA * modelData$NDVIRE * modelData$NDWI)
NDVIREw <- glmulti(NDVI_REw0)
NDVI_REw <- lm(paste(summary(NDVIREw)$bestmodel, ' +modelData$TA + modelData$RG + modelData$NDVIRE + modelData$NDWI'))

NDRE_RE0  <- lm(modelData$GPP ~ modelData$RG * modelData$TA * modelData$NDRE)
NDRE <- glmulti(NDRE_RE0)
NDRE_RE <- lm(paste(summary(NDRE)$bestmodel, ' +modelData$TA + modelData$RG + modelData$NDRE'))

NDRE_REw0 <- lm(modelData$GPP ~ modelData$RG * modelData$TA * modelData$NDRE * modelData$NDWI)
NDREw <- glmulti(NDRE_REw0)
NDRE_REw <- lm(paste(summary(NDREw)$bestmodel, ' +modelData$TA + modelData$RG + modelData$NDVIRE + modelData$NDWI'))

AICsG <- akaike.weights(c(AICc(NDVI_LS), AICc(NDVI_LSw), AICc(NDVI_RE), AICc(NDVI_REw), AICc(NDRE_RE), AICc(NDRE_REw)))

labs <- matrix(nrow = 6, ncol = 4) 
labs[1,1] <- summary(NDVI_LS)$adj.r.squared
labs[2,1] <- summary(NDVI_LSw)$adj.r.squared
labs[3,1] <- summary(NDVI_RE)$adj.r.squared
labs[4,1] <- summary(NDVI_REw)$adj.r.squared
labs[5,1] <- summary(NDRE_RE)$adj.r.squared
labs[6,1] <- summary(NDRE_REw)$adj.r.squared
labs[1,2] <- AICsG$deltaAIC[1]
labs[2,2] <- AICsG$deltaAIC[2]
labs[3,2] <- AICsG$deltaAIC[3]
labs[4,2] <- AICsG$deltaAIC[4]
labs[5,2] <- AICsG$deltaAIC[5]
labs[6,2] <- AICsG$deltaAIC[6]
labs[1,3] <- AICsG$rel.LL[1]
labs[2,3] <- AICsG$rel.LL[2]
labs[3,3] <- AICsG$rel.LL[3]
labs[4,3] <- AICsG$rel.LL[4]
labs[5,3] <- AICsG$rel.LL[5]
labs[6,3] <- AICsG$rel.LL[6]
labs[1,4] <- AICsG$weights[1]
labs[2,4] <- AICsG$weights[2]
labs[3,4] <- AICsG$weights[3]
labs[4,4] <- AICsG$weights[4]
labs[5,4] <- AICsG$weights[5]
labs[6,4] <- AICsG$weights[6]

labs <- data.frame(labs)
labsG <- data.frame(labs)
names(labs) <- c('R2adj','deltaAIC','relLL','weights')

outputG <- data.frame(modelData[, 1:7], NDVILS = fitted(NDVI_LS), NDVILSW = fitted(NDVI_LSw),
                NDVIRE = fitted(NDVI_RE), NDVIREW = fitted(NDVI_REw), NDRE = fitted(NDRE_RE),
                NDREW = fitted(NDRE_REw))

fittextG <- data.frame(x = 1, y = 3, SITE = 'Girdle', MODEL = c('NDVILS','NDVILSW','NDVIRE','NDVIREW',
                        'NDRE','NDREW'), fits = round(labs$R2adj, digits = 3)) 

modelOutputGm <- melt(outputG, c('SITE','YEAR','DOY','TA','PAR','GPP','RG'))
modelOutputCm <- melt(outputC, c('SITE','YEAR','DOY','TA','PAR','GPP','RG'))
modelOutput <- rbind(modelOutputGm, modelOutputCm)
names(modelOutput) <- c('SITE','YEAR','DOY','TA','PAR','GPP','RG','MODEL','GPP_Est')

allLabs <- rbind(labsC, labsG)
names(allLabs) <- c('adjR2','delAICc','relLL','weights')

allFits <- rbind(fittextC, fittextG)

ggplot(modelOutput, aes(GPP, GPP_Est)) + geom_point(aes(shape = factor(YEAR), color = factor(DOY)), size = 4) +
                facet_grid(MODEL~SITE) + 
                geom_text(data = allFits, aes(x = x, y = y, label = fits)) + theme_bw() +
                geom_smooth(method = lm, se = FALSE) + ggsave('new_more72.tiff', width = 8, height = 14)

write.table(allLabs, 'fits_less7.csv',sep = ',', row.names = FALSE)
write.table(modelOutput, 'Less7.csv',sep = ',', row.names = FALSE)

