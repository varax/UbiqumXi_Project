library(arules)
library(arulesViz)
transData <- read.transactions("ElectronidexTransactions2017.csv", 
                               format = "basket",sep = ",", rm.duplicates = TRUE)
summary(transData)
hist(size(transData))
summary(itemFrequency(transData))
mi <- min(itemFrequency(transData))
low <- head(sort(itemFrequency(transData)),5)
high <- tail(sort(itemFrequency(transData)),5)

names(fq)

fq <- c(low, high)

plot(fq)
text(fq, labels=unique(names(fq)), pos = 4, cex = 0.7)
identify(fq, labels=unique(names(fq)), plot= TRUE,
         offset = 1.5, tolerance = 10)
image(sample(transData, 100))
itemFrequencyPlot(transData, topN = 25)

ruledefault <- apriori(transData,
                       parameter = list(support = 0.01, confidence = 0.2, minlen = 2))
is.significant(ruledefault, transData)
inspect(ruledefault[!(is.significant(ruledefault, transData))])
rule.prune <- ruledefault[is.significant(ruledefault, transData)]
rd <- is.redundant(rule.prune)
rule.prune <- rule.prune[!rd]
summary(rule.prune)
inspect(rule.prune)



ruleMore <- apriori(transData,
                       parameter = list(support = 0.002, confidence = 0.2, minlen = 5))
is.significant(ruleMore, transData)
ruleM.prune <- ruleMore[is.significant(ruleMore, transData)]
inspect(sort(ruleM.prune, by = "lift"))
summary(ruleM.prune)
inspect(ruledefault)

#151 out of 158
rmR_subset <- subset(ruleM.prune, subset = (rhs %in% 
            desktopPr|rhs %in% laptopPr|rhs %in% mnt)
& (lhs %in% desktopPr|lhs %in% laptopPr|lhs %in% mnt))


sz <- size(transData)
largeAmount <- transData[sz >= 13]
inspect(transData[itemFrequency(largeAmount) == 0])
inspect(largeAmount)
singleAmount <- transData[sz == 1 ]
smAmount <- transData[sz <= 3]
lgA <- transData[sz > 4]
itemFrequencyPlot(lgA, topN = 10)
itemFrequencyPlot(smAmount, topN = 10)
summary(smAmount)
summary(lgA)
summary(singleAmount)
itemFrequencyPlot(singleAmount, topN = 5)

ruleTop <- head(ruledefault, n = 25, by = "lift")
plot(ruleTop, method = "paracoord")
inspect(ruleTop)
iMacTop <- head(iMacRules, n = 20 , by = "lift")
plot(iMacTop, method = "two-key plot")
rd <- is.redundant(ruledefault)
which(rd)
ruledefault<- ruledefault[!rd]
rRHS <- unique(as(rhs(ruledefault), "list"))
iMacRules <- subset(ruledefault, subset = rhs %in% "iMac" & lift > 1)
inspect(sort(iMacRules, by = "lift"))
summary(iMacRules)

HPlaptop <- subset(ruledefault, subset = rhs %in% "HP Laptop" & lift > 1)
inspect(sort(HPlaptop, by = "lift"))
lhs(apple_earPods)
apple_earPods <- subset(ruledefault, subset = rhs %in% "Apple Earpods" & lift >1)
inspect(sort(apple_earPods, by = "lift"))
app_Prod <- c("Apple TV","iPhone Charger Cable", "Apple Earpods","Apple Wired Keyboard" ,
              "Apple Wireless Keyboard" , "Apple Magic Keyboard", "iMac", "iPad", "iPad Pro",
              "Apple MacBook Pro", "Apple MacBook Air")


"%notin%"<- Negate("%in%")
familyss <- subset(ruledefault, subset = (lhs %in% laptopPr | lhs %in% desktopPr) &
             (rhs %notin% laptopPr & rhs %notin% desktopPr))
inspect(familyss)
plot(familyss, method = "graph", engine = "htmlwidget")
familyss <- subset(familyss, subset = (rhs %notin% "Apple Magic Keyboard"))
inspect(familyss)
# 73 vs. 18
appleRy <- subset(ruledefault, subset = rhs %in% app_Prod 
                  & lhs %notin% app_Prod)
inspect(sort(appleRy, by = "lift"))

topapr <- head(appleRy, n=20, by = "lift")
plot(topapr, method = "graph")
appleRy2 <- subset(ruledefault, subset = rhs %in% app_Prod 
                  & lhs %in% app_Prod)
inspect(sort(appleRy2, by = "lift"))
plot(appleRy2, method = "graph")

inspect(sort(appleRy,by = "lift") )
plot(appleRy, method = "paracoord")
plot(appleRy2, method = "paracoord")
inspect(sort(appleRy2, by = "lift"))
summary(appleRy2)


laptopPr <- c("LG Touchscreen Laptop",
              "Acer Aspire",
              "HP Laptop",
              "ASUS Chromebook",
              "Apple MacBook Pro",
              "Apple MacBook Air",
              "Dell Laptop",
              "Eluktronics Pro Gaming Laptop",
             "Alienware Laptop",
               "HP Notebook Touchscreen Laptop PC")

desktopPr <- c("Lenovo Desktop Computer",
              "iMac",
              "HP Desktop",
              "ASUS Desktop",
              "Dell Desktop",
              "Intel Desktop",
              "Acer Desktop",
              "CYBERPOWER Gamer Desktop",
              "Dell 2 Desktop")

mnt <- c(
         "Acer Monitor",
         "LG Monitor",
         "ASUS Monitor",
         "ASUS 2 Monitor",
         "Dell Monitor",
         "Samsung Monitor",
         "Sceptre Monitor",
         "ViewSonic Monitor",
         "AOC Monitor",
         "HP Monitor")

#14
LPrule <- subset(ruledefault, subset = rhs %in% 
                   laptopPr|rhs %in% desktopPr
                & lhs %in% laptopPr|lhs %in% desktopPr)
head(inspect(sort(LPrule, by = "lift")),20)
summary(LPrule)

Lpfq <- itemFrequency(rhs(LPrule)) != 0
lps <- subset(itemFrequency(rhs(LPrule)), Lpfq)
sort(lps)
LPlfq <-itemFrequency(lhs(LPrule)) != 0
lpls <- subset(itemFrequency(lhs(LPrule)), LPlfq)
sort(lpls)
hp <- subset(LPrule, subset = (rhs %in% "HP Laptop") & (lhs %in% laptopPr|lhs %in% desktopPr | lhs %in% mnt))
hptop <- head(sort(hp, by = "lift"), 20)
plot(hptop, method = "graph")
inspect(hptop)


hpl <-  subset(LPrule, subset = (lhs %in% "HP Laptop"))
hpltop <- head(sort(hpl, by = "lift"), 20)
inspect(hpltop)
hplfq <- itemFrequency(rhs(hpltop)) != 0
hplsub <- subset(itemFrequency(rhs(hpltop), "absolute"), hplfq)
sort(hplsub)
hplfq2 <- itemFrequency (lhs(hpltop))!= 0 
hplsub2 <- subset(itemFrequency (lhs(hpltop), "absolute"), hplfq2)

iMacrule <- subset(LPrule, subset = (rhs %in% "iMac") & (lhs %in% laptopPr|lhs %in% desktopPr | lhs %in% mnt))
iMactop <- head(sort(iMacrule, by = "lift"), 20)
inspect(iMactop)
iMaconleft <- subset(LPrule, subset = (lhs %in% "iMac"))
iMacTop2 <- head(sort(iMaconleft, by = "lift"), 20)
inspect(iMacTop2)
iMacfq <- itemFrequency(rhs(iMacTop2)) != 0
iMacsub <-subset(itemFrequency(rhs(iMacTop2), "absolute"), iMacfq)
sort(iMacsub)

lnrule <-  subset(LPrule, subset = (rhs %in% "Lenovo Desktop Computer") & (lhs %in% laptopPr|lhs %in% desktopPr | lhs %in% mnt))
lntop <- head(sort(lnrule, by = "lift"), 20)
inspect(lntop)

lnlrule <- subset(LPrule, subset = (lhs %in% "Lenovo Desktop Computer"))
lnltop <- head(sort(lnlrule, by = "lift"), 20)
inspect(lnltop)
lnrh <- itemFrequency(rhs(lnltop)) != 0
lnrs <- subset(itemFrequency(rhs(lnltop), "absolute"), lnrh)
sort(lnrs)

Dellrl <- subset(LPrule, subset = (rhs %in% "Dell Desktop") & (lhs %in% laptopPr|lhs %in% desktopPr | lhs %in% mnt))
inspect(Dellrl)

Delllr <- subset(LPrule, subset = (lhs %in% "Dell Desktop"))
Delllrtop <- head(sort(Delllr, by = "lift"), 20)
inspect(Delllrtop)
dellfq <- itemFrequency(rhs(Delllrtop)) != 0
dellrs <- subset(itemFrequency(rhs(Delllrtop), "absolute"), dellfq)

gamerl <- subset(LPrule, subset = (rhs %in% "CYBERPOWER Gamer Desktop") & (lhs %in% laptopPr|lhs %in% desktopPr | lhs %in% mnt))
inspect(gamerl)

cyber<- subset(LPrule, subset = (lhs %in%  "CYBERPOWER Gamer Desktop"))
inspect(cyber)
gamerhfq <- itemFrequency(rhs(cyber)) != 0
gamerrs <- subset(itemFrequency(rhs(cyber), "absolute"), gamerhfq)

mnr <- subset(LPrule, subset = (rhs %in% mnt) & (lhs %in% laptopPr|lhs %in% desktopPr | lhs %in% mnt))
inspect(mnr )

viewsn <- subset(LPrule, subset = (lhs %in% "ViewSonic Monitor"))
inspect(viewsn)
viewrhfq <- itemFrequency(rhs(viewsn)) != 0
viewrrs <- subset(itemFrequency(rhs(viewsn), "absolute"), viewrhfq)
#74
dskRule <- subset(ruledefault, subset = rhs %in% 
                    desktopPr
                  & lhs %in% desktopPr)
inspect(sort(dskRule, by ="lift"))



#205 
lp_dskRule <- subset(ruledefault, subset = (rhs %in% 
                       desktopPr|rhs %in% laptopPr|rhs %in% mnt)
                     & (lhs %in% desktopPr|lhs %in% laptopPr|lhs %in% mnt))

rule2 <-  subset(ruledefault, subset = (rhs %notin% 
                                          desktopPr & rhs %notin% laptopPr& rhs %notin% mnt)
                 | (lhs %notin% desktopPr&lhs %notin% laptopPr&lhs %notin% mnt))
inspect(sort(rule2,by = "lift"))

rule2Top <- head(rule2, n= 20, by = "lift")
plot(rule2, method="graph", engine = "htmlwidget")


apple_ep <- subset(ruledefault, subset = rhs %in% "Apple Earpods")
app_ky <- subset(ruledefault, subset = rhs %in% "Apple Magic Keyboard")
inspect(app_ky)
apple_epss <- subset(apple_ep,subset = (lhs %notin% "3-Button Mouse" & lhs %notin% "Microsoft Wireless Desktop Keyboard and Mouse"))
plot(apple_epss, method = "graph")
imac <- subset(ruledefault, subset = rhs %in% "iMac")
inspect(sort(imac, by = "lift"))
is.significant(LPrule, transData)
inspect(LPrule)
