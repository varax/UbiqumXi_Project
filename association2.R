library(arules)
library(arulesViz)
transData <- read.transactions("ElectronidexTransactions2017.csv", 
                               format = "basket",sep = ",", rm.duplicates = TRUE)
summary(transData)
hist(size(transData))
summary(itemFrequency(transData))
sort(itemFrequency(transData))
inspect(head(transData,10))

low <- head(sort(itemFrequency(transData)),5)
high <- tail(sort(itemFrequency(transData)),5)
fq <- c(low, high)
plot(fq)
itemFrequencyPlot(transData, topN = 25)

ruledefault <- apriori(transData,
                       parameter = list(support = 0.01, confidence = 0.2, minlen = 2))
is.significant(ruledefault, transData)
inspect(ruledefault[!(is.significant(ruledefault, transData))])
rule.prune <- ruledefault[is.significant(ruledefault, transData)]
rd <- is.redundant(rule.prune)
rule.prune <- rule.prune[!rd]
summary(rule.prune)
inspect(sort(rule.prune, by = "lift"))

subsetprune <- subset(rule.prune, subset=(lhs %in% laptopPr|
                                            lhs %in% desktopPr))

rhspru<- itemFrequency(rhs(subsetprune)) !=0
subset( itemFrequency(rhs(subsetprune)), rhspru)


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


"%notin%"<- Negate("%in%")
rulesubset <- subset(rule.prune,subset = (rhs %notin% laptopPr&
                                        rhs %notin% desktopPr))
inspect(rulesubset)

rule1 <- subset(rule.prune, subset = ((lhs %in% laptopPr|
                                        lhs %in% desktopPr) &
                  (rhs %notin% laptopPr&
                     rhs %notin% desktopPr)))
inspect(rule1)

rule_2 <- subset(rule.prune, subset = ((lhs %in% laptopPr|
                                         lhs %in% desktopPr) &
                   (rhs %in% laptopPr|
                      rhs %in% desktopPr |
                      rhs %in% mnt)))

inspect(sort(rule_2, by = "lift"))
rhsfqrule2<- itemFrequency(rhs(rule_2)) !=0
subset( itemFrequency(rhs(rule_2)), rhsfqrule2)


plot(rulesubset, method = "graph",  engine = "htmlwidget")
rsfq<- itemFrequency(rhs(rulesubset)) !=0
subset( itemFrequency(rhs(rulesubset)), rsfq)

#in order to check different size of items, 
#"minlen"is not a wise way to do,
#because it can't refelct the real amount of each transaction.
#instead, subset the data first, then apply to investigate the rule.

sz <- size(transData)
singleAmount <- transData[sz == 1 ]
summary(singleAmount)
itemFrequencyPlot(singleAmount, topN = 5)


#when per transaction < 4:
smAmount <-transData[sz < 4]
summary(smAmount)

rulesmall<-apriori(smAmount,
                   parameter = list(support = 0.001, confidence = 0.1, minlen = 2))

rulesmallsubset <- subset(rulesmall, subset = (lhs %in% laptopPr|
                                                      lhs %in% desktopPr)&
                            ( rhs %notin% desktopPr))
inspect(sort(rulesmallsubset, by = "lift"))
rulesmallset <- head(sort(rulesmallsubset, by = "lift"), 10)
plot(rulesmallset, method = "graph", engine = "htmlwidget")

rhssmfq<- itemFrequency(rhs(rulesmall)) !=0
subset( itemFrequency(rhs(rulesmall)), rhssmfq)
#when >=4
larAmount <- transData[sz >=4]
summary(larAmount)
ruleslar<-apriori(larAmount,
                   parameter = list(support = 0.01, confidence = 0.2, minlen = 2))
topruleslar <- head(ruleslar,n=20, by = "lift")
inspect(topruleslar)
plot(topruleslar, method = "graph",  engine = "htmlwidget")

ruleslar


rhsfq2<- itemFrequency(rhs(topruleslar)) !=0
subset( itemFrequency(rhs(topruleslar)), rhsfq2)
inspect(topruleslar)
