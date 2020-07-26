
AllADNISets <- list();
AllADNI_Model <- list();
n=1

#theFitMethod <- BSWiMS.model
theFitMethod <- e1071::svm

for (n in 1:resamplingNumber)
{
  randomnumber <- sample(1:nrow(ADNITRAIN),nrow(ADNITRAIN))
  AllADNI_RandomSet <- ADNITRAIN[randomnumber,]
  AllADNI_RandomSet <- AllADNI_RandomSet[order(AllADNI_RandomSet$PTID),]
  ptID <- AllADNI_RandomSet$PTID
  set1 <- AllADNI_RandomSet[c(ptID[1:length(ptID)-1] != ptID[-1],TRUE),]
  rownames(set1) <- set1$PTID
  AllADNISets[[n]] <- set1[,c("class",cpredictors)]
  AllADNISets[[n]] <- AllADNISets[[n]][complete.cases(AllADNISets[[n]]),]
  print(nrow(AllADNISets[[n]]))
  print(table(set1$DX_bl,set1$class))
#  AllADNISets[[n]]$class <- as.factor(AllADNISets[[n]]$class)
#  AllADNI_Model[[n]] <- filteredFit(class ~ .,
#                                    AllADNISets[[n]],
#                                    fitmethod = theFitMethod,
#                                    filtermethod.control=list(pvalue=0.1,limit=150))
  AllADNI_Model[[n]] <- BSWiMS.model(class ~ .,AllADNISets[[n]],NumberofRepeats = 5)
  print(length(AllADNI_Model[[n]]$selectedfeatures))
}

pred <- predict(AllADNI_Model[[1]],ADNITEST)

for (n in 2:resamplingNumber)
{
  pred <- pred+predict(AllADNI_Model[[n]],ADNITEST)
}
pred <- pred/resamplingNumber

pb <- predictionStats_ordinal(cbind(ADNITEST$class,as.integer(pred+0.5)),plotname = "ADNI Classificaiton")

print(pb$class95ci)

table(ADNITEST$class,as.integer(pred+0.5))
