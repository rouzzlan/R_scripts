# bij return haakjes niet vergeten!!!!

#===================== ALGEMEEN =============================

#copy path in explorer naar clipboard en voer dit uit of geeft file naam als argument mee
leesCSV = function(file = readClipboard(), separator = ",", decimal=",") {
  return (read.csv(file, header=TRUE, sep=separator, dec=decimal))
}

rondAf = function(x) {
  return (round(x,4))
}

toonAlsBreuk = function(x) {
  library(MASS)
  return (fractions(x))
}

percent = function(x) {
  sprintf("%1.4f%%", 100*x)
}

faculteit = function(x) {
  return (factorial(x))
}

macht = function(x, exponent) {
  return (x ^ exponent)
}

# x = een dataframe
# indexArray = een array met rij indexen of een enkele index  e.g c(1:5) of 6
# cols een array van colomn namen of een enkele kolom naam e.g c("geslacht", "leeftijd") of "lengte"
select = function(x, indexArray, cols=T) {
  return (x[indexArray,cols])
}

# !!!SUBSET FUNCTIE!!!
#   
# waarvoor:
# subtabellen maken door gebruik van select statements
# 
# gebruik:
# subset(dataframe, selectStatementRows, columns)
# 
# & voor AND
# | voor OR
# 
# selectStatementRows = e.g. lengte>120 & lengte<180 & id==c(5:30)
# columns = e.g. col1:col4
# 
# Extra voorbeelden:
# df = data.frame(...) of read.csv(...)
# subset(df, col1 < 5)
# subset(df, col2 == "Sara")
# subset(df, col1==5, select=c(col2, col3))
# subset(df, col2 != "Sara" & geslacht == "F" & col1 > 5)
# 
# LET OP: kolom namen moeten niet in hoofdletters

#===================== WEEK 2: frequenties =============================

maakFrequentieTabel = function(kolom, metKlassen=F, breaks=nclass.Sturges(kolom)) {
  absFreq = 0
  if(metKlassen) {
    absFreq = table(factor(cut(kolom, breaks=breaks)))
  } else {
    absFreq = table(kolom)
  }
  
  relFreq = round(absFreq/length(kolom)*100,2)
  absCumFreq = cumsum(absFreq)
  cumPercentages = round(cumsum(absFreq/length(kolom)*100),2)
  tabel = cbind( absFreq, relFreq, absCumFreq, cumPercentages)
  colnames(tabel) = c("[Abs. Freq.]", "[Rel. Freq]", "[Abs. Cum. Freq.]", "[Cum. Percentages]")
  return (tabel)
}

#van frequentietabel naar raw values
convertFreqToRaw = function(values, freqs) {
  return (rep(values, freqs))
}

freqpoly = function(x, breaks=0, ...) {
  if (breaks[1]==0) {
    tmp=hist(x, plot=F)
  } else {
    tmp=hist(x, breaks=breaks, plot=F)
  }
  plot(c(min(tmp$breaks),tmp$mids,max(tmp$breaks)),c(0,tmp$counts,0),type="l", ...)
}

#===================== WEEK 3: centrumaten en spreidingsmaten =============================

modus <- function(x){
  return (names(sort(table(x), T))[1])
}

varcoefficient <- function(x) {
  return (sd(x)/mean(x))
}

gewogenGemiddelde = function(rij1, rij2) {
  return (sum(rij1*rij2)/sum(rij1))
}

meetkundigGemiddelde = function(koers) {
  return (prod(koers)^(1/length(koers)))
}

harmonischGemiddelde = function(rij) {
  return (1/mean(1/rij))
}

# <0 = linksscheef
# >0 = rechtsscheef
# =0 = symmetrisch
skew = function(x) {
  m3 = mean((x-mean(x))^3)
  m2 = mean((x-mean(x))^2)
  return(m3/(m2^(3/2)))
}

# <0 = linksscheef
# >0 = rechtsscheef
# =0 = symmetrisch
skew = function(mean, median) {
  return (mean-median)
}

bereik = function(x) {
  return (max(x)-min(x))
}

kwantiel = function(x, n) {
  return (quantile(x, n, names=F))
}

interkwartielafstand = function(x){
  return(quantile(x, 0.75, names=F) - quantile(x, 0.25, names=F)) #zie ook ingebouwde IQR functie
}

kleineuitschieter = function(x,factor=1.5){ #onbetrouwbaar
  return(x[x<quantile(x, 0.25, names=F) - interkwartielafstand(x) * factor])
}
groteuitschieter = function(x,factor=1.5){ #onbetrouwbaar
  return(x[x>quantile(x, 0.75, names=F) + interkwartielafstand(x) * factor])
}
uitschieters = function(x,factor=1.5){
  return(c(kleineuitschieter(x,factor),groteuitschieter(x,factor)))
}

gemAbsAfwijking = function(x) {
  return (median(abs(x-median(x))))
}

#===================== WEEK 4: kansrekenen =============================
#P(A|B) = kans dat B waar is als A waar is
#P(B|A) = kans dat A waar is als B waar is
#P(B EN A) = kans dat A en B waar zijn
#Hoe groot is de kans op B, gegeven dat A waar is? ==> gebruik P(A|B) forumle
wetVanBayes = function(B, AB, AnotB) { 
  A = (B*AB+AnotB)
  writeLines(sprintf("P(A)=%f\nP(B)=%f\nP(B|A)=%f\n***formule***\nP(A|B) = ( P(B|A) * P(A) ) / P(A) = %f",A, B,AB,B*AB/A))
}
#===================== WEEK 5: binomiale en normale verdeling =============================

maakKansTabel = function(x, size=length(x)-1, kans, percent=FALSE, afronding=4) {
  kans = dbinom(x,size,kans)
  cumKans = cumsum(kans)
  if(percent) {
    tabel = cbind(x,percent(kans),percent(cumKans))
  }else {
    tabel = cbind(x,round(kans, afronding),round(cumKans, afronding))
  }
  colnames(tabel) = c("x", "kans", "cum. kans")
  return (tabel)
}

magbenaderen = function(n,p){
  # Beide getallen moeten > 5 zijn
  getallen = c((n*p), (n*(1-p)))
  # writeLines(sprintf("N is %f, P is %f", n, p))
  if(sum(getallen > 5) > 1){
    writeLines("JA: De binomiaalverdeling mag benaderd worden met de normaalverdeling.")
  }else{
    writeLines("NEE: Je mag de binominaalverdeling niet benaderen met de normaalverdeling.")
  }
  return(getallen)
}

berekenOppervlakte = function(x1, x2, sd, mean){
  return(abs(pnorm(x1, mean, sd)-pnorm(x2, mean, sd)))
}

binomialeVerdeling = function(aantalGewensteUitkomsten, aantalUitkomsten, kansUItkomst) {
  kans = dbinom(aantalGewensteUitkomsten, aantalUitkomsten, kansUItkomst)
  return (kans)
}

waardeofhoger = function(waarde,aantal,kans){
  return(1 - pbinom(waarde-1,aantal,kans))
}
waardeoflager = function(waarde,aantal,kans){
  return(pbinom(waarde,aantal,kans))
}

verwachtewaarde = function(mogelijkeuitkomsten, aantal, kans){
  return(sum(mogelijkeuitkomsten*dbinom(mogelijkeuitkomsten, aantal, kans)))
}

verwachtewaardesimpel = function(aantal,kans){
  return(aantal*kans)
}

normaleVerdeling = function(gewenstResultaat, gemiddelde, sd) {
  return (1-pnorm(gewenstResultaat, mean=gemiddelde, sd=sd))
}

berekenNormaleVerdeling = function(gewenstResultaat, n, p) {
  mean = berekenMean(n, p)
  sd = berekensd(n, p)
  return (normaleVerdeling(gewenstResultaat=gewenstResultaat, mean, sd))
}

#Œº
berekenMean = function(n,p) {
  return(n*p)
}

#∆°
#bereken standaard afwijking
berekensd = function(n, p) {
  return (sqrt(n*p*(1-p)))
}

#berekent wat relatief het beste is (hoe hoger hoe beter)
zscore = function(x, gem=0, sd=1) {
  return ((x-gem)/sd)
}

# s= standaardafwijking van steekproef
# sigma (œ?) =standaardafwijking van populatie


# pbinom 
# test van 5 vragen slaag je als je 3 vragen juist hebt
# P(x>=3) = P(x=3) + P(x=4) + P(x=5)
# dus 1-pbinom(2, 5, 1/4) = 0.1035156
# de kans dat je buist is dus 0.8964844

# vuistregel:
#   ‚?? vanaf dat (n*p>5) EN (n*(1-p)>5) kan je binomiale verdeling dus benaderen
# met een normale verdeling 
# 
# n = aantal
# p = kansOpGoedResultaat
# Œº = n*p
# œ? = sqrt(n*p*(1-p))


#het zelfde als op u TI-84
normalcdf = function(lower, upper, mean, sd) {
  return(pnorm(upper, mean=mean, sd=sd)-pnorm(lower, mean=mean,sd=sd))
}

#het zelfde als op u TI-84
invNorm = function(p, mean, sf) {
  return (qnorm(p, mean, sf))
}


significantieniveau = function(p) {
  return (1-p)
}

#variatieco√´fficient
varCof = function(col) {
  return (sd(col)/mean(col))
}

#===================== WEEK 6: toetsen =============================

berekenFactor = function(p) {
  return (qnorm((1+p)/2))
}

#Z-toets
#als n>=100 of œ? gekend is
#als het beweerde gemiddelde (van H0) hier buiten ligt dan kan je de hypothese verwerpen
ztest = function(n, p, m, s,eenzijdig=F) {
  sd = 0
  if(n<100) {
    sd = s/sqrt(n)
  } else {
    sd = s
  }
  if(eenzijdig){
    return (qnorm(1-p,m,sd))
  }
  f = berekenFactor(p)
  interval.links = m-f*sd
  interval.rechts = m+f*sd
  return (sprintf("%.2f%% ligt tussen %.2f en %.2f", p*100, interval.links, interval.rechts))
}

ttestSteekproef = function(x, p) {
  return (ttest(length(x), p, mean(x), sd(x)))
}

#t-toets
#als n<100 en œ? niet gekend is
#als het beweerde gemiddelde (van H0) hier buiten ligt dan kan je de hypothese verwerpen
ttest = function(n, p, m, s) {
  sf = s/sqrt(n)
  f = qt((1+p)/2, n-1)
  interval.links = m-f*sf
  interval.rechts = m+f*sf  
  return (sprintf("%.2f%% ligt tussen %.4f en %.4f", p*100, interval.links, interval.rechts))
}


#nominale variabelen en een kansverdeling
# PARAMS:
# fo = geobserveerde absolute waarden
# fe = verwachte absolute waarden(percent) --> sum(fe) = 1!
# percent = hypothese
# RETURN:
# df = n-1
# p-value = aantal percent kans dat je een waarde kan krijgen die hoger is als X¬≤
# X¬≤ = afstand
# zekerheid (grens waarde)
chiKwadraatTest = function(fo, fe,percent=0.95) {
  print(chisq.test(fo,p=fe))
  zekerheid = qchisq(percent, length(fo)-1)
  return (sprintf(" als X¬≤ >= %.4f waarde ben je %.2f%% zeker dat de nulhypothese niet waar is.",zekerheid,percent*100))
}

#===================== WEEK 7 verbanden/samenhang =============================

#geeft getal tussen -1 en 1
# perfect stijgende lijn door punten => 1
# perfect dalende lijn door punten => -1
# geen verband => 0
# ‚?? 0 tot 0,2: nauwelijks verband
# ‚?? 0,2 tot 0,4: zwak verband
# ‚?? 0,4 tot 0,6: redelijk verband
# ‚?? 0,6 tot 0,8: sterk verband
# ‚?? 0,8 tot 1: zeer sterk verband
correlatie = function(x, y) {
  return (cor(x,y))
}

# x = onafhankelijke variabele
# y = afhankelijke variable
scatterplot = function(x, y) {
  return (plot(x,y))
}

# x = kolom
# y = kolom
# legendPos = "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" of "center"
nonLinearRegression = function(x, y, legendPos="topleft") {
  plot(x, y, ...)
  
  reg=lm(y~x)
  abline(reg, col="red", lwd=2)
  s=summary(reg)
  rr=s$r.squared
  
  reg=lm(y~x+I(x^2))
  s=summary(reg)
  rr=c(rr, s$r.squared)
  xx=seq(min(x), max(x), (max(x)-min(x))/100)
  yy=reg$coefficients[1]+reg$coefficients[2]*xx+reg$coefficients[3]*xx^2
  lines(xx, yy, lty=2, col="green", lwd=2)
  
  reg=lm(y~x+I(x^2)+I(x^3))
  s=summary(reg)
  rr=c(rr, s$r.squared)
  yy=reg$coefficients[1]+reg$coefficients[2]*xx+reg$coefficients[3]*xx^2+reg$coefficients[4]*xx^3
  lines(xx, yy, lty=3, col="blue", lwd=2)
  
  reg=lm(y~log(x))
  s=summary(reg)
  rr=c(rr, s$r.squared)
  yy=reg$coefficients[1]+reg$coefficients[2]*log(xx)
  lines(xx, yy, lty=4, col="magenta", lwd=2)
  
  reg=lm(log(y)~x)
  s=summary(reg)
  rr=c(rr, s$r.squared)
  yy=exp(reg$coefficients[1]+reg$coefficients[2]*xx)
  lines(xx, yy, lty=5, col="brown", lwd=2)
  legend = c("lin", "kwad", "kub", "log", "exp")
  legend(x=legendPos, legend=legend, lty=1:5, col=c("red", "green", "blue", "magenta", "brown"), lwd=2, xjust=1)
  
  t = as.table(rr)
  names(t) = legend
  return(t)
}

regressielijn = function(x, y, details=F){
  return(standaardschattingsfout(x,y,details))
}

# maak een grafiek met regressielijn en bereken de standaardschattingsfout
standaardschattingsfout = function(x, y, details=F,log=F) {
  # pagina 304
  
  plot(x,y)
  if(log){
    reg=lm(y~log(x)) #logaritmische regressie
    
  }else{
    reg=lm(y~x) # bereken de regressielijn
  }
  
  abline(reg, col='red') # voeg de lijn toe aan de grafiek en plot deze
  
  a = reg$coefficients[1] # het punt waar de rechte de y-as snijd (intercept)
  b = reg$coefficients[2] # richtingsco√´fficient van de rechte (slope)
  se=sqrt(deviance(reg)/df.residual(reg)) # standaardschattingsfout, geeft de grootte van de de foutenmarge rond de regressielijn
  writeLines(sprintf("a intercept (=y-as/rechte) is %.6f, b slope (helling) is %.6f", a, b))
  
  #Extra info
  s = summary(reg)
  sigma = s$sigma #standaardschattingsfout
  squared = s$r.squared #R¬≤=kwadraat van correlatie, het "percentage" van Y dat verklaard wordt door het model
  
  writeLines(sprintf("se is %.4f, R¬≤ is %.4f", sigma, squared))
  # betekenis van R¬≤
  #48,6% van de variantie in het totaalgebruik wordt verklaard door de variantie in het gebruik van de
  #smartphone. Het gebruik van de smartphone bepaalt dus voor 48,6% de tijd die je tussen 2
  #laadbeurten hebt.
  
  
  # if(details){
  #    return (summary(reg)) # bevat nog meer informatie
  #  }
  
  return (se)
}

#===================== WEEK 8 forecasting ====================

naieveFore <- function(d){
  leng=length(d)
  vo=d[leng]
  x=d[2:leng]
  f=d[1:leng-1]
  MAE=mean(abs(x-f))
  RMSE = sqrt(mean((x-f)^2))
  MAPE = mean(abs((x-f)/x))
  terug<- paste("MAE:",MAE," RMSE:",RMSE," MAPE:",MAPE)
  voorspelling<-paste("voorspelling:",vo)
  print(voorspelling)
  print(terug)
}

gemidFore <- function(d){
  leng=length(d)
  x=d[2:leng]
  f=NULL
  for(i in 1:leng-1){ f[i] = mean(d[1:i])}
  p=length(f)
  vo=f[p]
  
  MAE = mean(abs(x-f))
  RMSE = sqrt(mean((x-f)^2))
  MAPE = mean(abs((x-f)/x))
  terug<- paste("MAE:",MAE," RMSE:",RMSE," MAPE:",MAPE)
  voorspelling<-paste("voorspelling:",vo)
  print(voorspelling)
  print(terug)
}

gemidFore2<-function(d,m){  
  leng=length(d)
  x = d[(m+1):31]
  f=NULL
  f = filter(d, rep(1/m, m), sides=1)
  f = f[m:30]
  MAE = mean(abs(x-f))
  RMSE = sqrt(mean((x-f)^2))
  MAPE = mean(abs((x-f)/x))
  
  terug<- paste("MAE:",MAE," RMSE:",RMSE," MAPE:",MAPE)
  
  getal=m-1
  p<-d[(leng-getal):leng]
  vo=mean(p)
  getal=m-2
  p<-d[(leng-getal):leng]
  vo1=(mean(p)+vo)/2
  getal=m-3
  p<-d[(leng-getal):leng]
  vo2=(mean(p)+vo+vo1)/3
  
  
  
  voorspelling<-paste("voorspelling:",vo,vo1,vo2)
  print(voorspelling)
  print(terug)
}

regFore<-function(d){
  
  leng=length(d)
  reg=lm(d~c(1:leng))
  x = d
  f = reg$coefficients[1]+c(1:leng)*reg$coefficients[2]
  MAE = mean(abs(x-f))
  RMSE = sqrt(mean((x-f)^2))
  MAPE = mean(abs((x-f)/x))
  vo=reg$coefficients[1]+(leng+1)*reg$coefficients[2]
  
  terug<- paste("MAE:",MAE," RMSE:",RMSE," MAPE:",MAPE)
  voorspelling<-paste("voorspelling:",vo)
  print(voorspelling)
  print(terug)
}


# kolom = e.g. examen$verkoopappels
# seizoenstype = additive of multiplicative
# seizoenFrequency= e.g. 4 (per kwartaal) of 7 (weekelijks)
# mainTitel = e.g. kolomAppels
# mainTitel2 = e.g. Opbrengsten
# xTitel = e.g. kwartaal
# yTitel = e.g. opbrengst (in EUR)
seizoensGebondenFore = function(kolom, seizoenType="multiplicative", seizoenFrequency, mainTitel="kolom", mainTitel2="progress", xTitel="x-as", yTitel="y-as") {
  kLen = length(kolom)
  plot(c(1:kLen),kolom,type="b",main=mainTitel, xlab=xTitel, ylab=yTitel)
  # bepalen van seizoensgrootte
  acf(kolom)
  t = ts(kolom, frequency=seizoenFrequency)
  d = decompose(t, type=seizoenType)
  plot(d)
  # L(t) benaderen met lineaire regressie
  reg = lm(d$trend~c(1:kLen))
  plot(c(1:kLen),kolom, type="b", main=mainTitel2, xlab=xTitel, ylab=yTitel, xlim=c(1,kLen))
  abline(reg, col="red")
  # voorspellen met L(t) en S(t)
  intercept=reg$coefficients[1]
  slope=reg$coefficients[2]
  f = rep(d$figure,seizoenFrequency)*(intercept+slope*c(1:kLen))
  x = kolom[1:kLen]
  f = f[1:kLen]
  MAE = mean(abs(x-f))
  RMSE = sqrt(mean((x-f)^2))
  MAPE = mean(abs((x-f)/x))
  
  writeLines(sprintf("MAE = %.08f\nRMSE = %.08f\nMAPE = %.08f", MAE, RMSE, MAPE))
}


#===================== WEEK 9: beslissingsbomen =============================


# author: Kris Demuynck (kris.demuynck@kdg.be)
# date: 2012-04-24
# version 1.0
# description: this program calculates and displays a decision tree
#              using the ID3 algorithm
#              it is writen in R

# This is the main function.  It gets a dataframe as an argument.
# assumptions:
#  - only nominal data
#	- last column is the one that is predicted
#	- data should be consistent, otherwise an error is issued
ID3 = function(data, depth=1) {
  if (depth >= length(data)) {
    cat("ERROR: data is not consistent!\n------------\n")
    print(data)
    cat("------------\n")
    return()
  }
  
  # find the column that is predicted by the decision tree
  predictColumn = length(data)
  
  # find the column with the highest gain
  columnIndex = findColumnWithHighestGain(data)
  
  # determine if recursion can stop (predictColumn contains only one value)
  l = levels(factor(data[,predictColumn]))
  if (length(l)==1) {
    cat(l, "\n")
    return()
  }
  
  # print node-name
  cat("[",colnames(data)[columnIndex], "]\n", sep="")
  
  # loop through all possible values for this node
  for(level in levels(factor(data[,columnIndex]))) {
    # print the value
    cat(rep("    ", depth),"- ", level, ": ", sep="")
    
    # calculate the childtable for this value
    childTable = data[data[columnIndex] == level,]
    
    # call ID3 recursively
    #if (columnIndex<length(colnames(data))) {
    ID3(childTable, depth+1)
    #}
  }
}

# calculates the entropy of a given dataframe
calculateEntropy = function(data) {
  # find the column that is predicted by the decision tree
  predictColumn = length(data)
  
  # accumulator for result
  result = 0
  
  # number of rows in data
  n = length(data[,1])
  
  # loop through possible classes
  l=levels(factor(data[,predictColumn]))
  for(level in l) {
    # get table with only the selected class
    childTable = data[data[predictColumn]==level,]
    p = length(childTable[,1])
    result = result - p/n*log2(p/n)
  }
  
  result
}

# calculates the gain for a certain column in a given dataframe
calculateGain = function(data, columnIndex) {
  # accumulator for result
  result = calculateEntropy(data)
  
  # number of rows in data
  n = length(data[,1])
  
  # loop through possible levels of selected column
  l=levels(factor(data[,columnIndex]))
  for(level in l) {
    # get table with selected level for selected column
    childTable=data[data[columnIndex]==level,]
    p = length(childTable[,1])
    result = result - p/n*calculateEntropy(childTable)
  }
  
  result
}

# finds the column in a dataframe for which the gain is the highest
findColumnWithHighestGain = function(data) {
  # find the column that is predicted by the decision tree
  predictColumn = length(data)
  
  # gains will hold the gains for each column in data
  gains = 0
  for(columnIndex in c(1:(predictColumn-1))) {
    gains[columnIndex] = calculateGain(data, columnIndex)
  }
  
  # get the column number with the highest gain
  order(gains, decreasing=T)[1]
}


calculateGains = function(data, kolomIndexen = c(1:(length(data)-1)), sort=T) {
  
  # gains will hold the gains for each column in data
  gains = 0
  for(columnIndex in kolomIndexen) {
    gains[columnIndex] = calculateGain(data, columnIndex)
  }
  
  frame = data.frame(colnames(data)[kolomIndexen], round(gains, 5))
  colnames(frame) = c("[Kolom]", "[Gain]")
  
  if(!sort)
    return (frame)
  else
    return (frame[order(frame[,2], frame[,1], decreasing=T),])
}

#===================== WEEK 10: clusteranalyse =============================

# heel tabel vergelijken met verschillende methodes
# m= "euclidean","maximum", "manhattan", "canberra", "binary" or "minkowski"
maakAfstandsTabel = function(data,m="euclidean"){
  numeric = data
  for(i in c(1:(length(data)))) {
    numeric[,i] = as.numeric(numeric[,i])
  }
  
  round(dist(numeric, method = m, diag = TRUE, upper = T, p = 10),2)
  
}


#data is de frame die je maakt van de kolommen die je nodig hebt
#kan je gebruiken voor alle afstanden te berekenen
#zorg ervoor dat je de methode altijd juist schrijft
#je geeft gewoon je data mee met de methode
#als je 2 rijen moet vergelijken geef je de nummer van de rijen (index)
distanceRij = function(data,m="euclidean", index1=1, index2=1 ){
  #alles numeric zetten
  numeric = data.matrix(data)
  # verschillende soorten afstanden in een tabel
  if(m == "euclidean" && index1 == index2 || m == "manhattan" && index1 == index2|| m == "maximum" || m == "minkowski"){
    return(dist(numeric,m))
  }
  #euclidische afstand tussen 2 cellen
  else if(m == "euclidean"){
    return (sqrt(sum((numeric[index1,]-numeric[index2,])^2)))
  }
  #gestandaardiseerd euclidische afstand tussen 2 cellen
  else if(m == "standaard"){
    return (sqrt(sum((numeric[index1,]-numeric[index2,])^2/diag(var(numeric)))))
  }
  #manhattan afstand tussen 2 cellen (de absolute afstand)
  else if(m == "manhattan" && index1 != index2){
    return (sum(abs(numeric[index1,]-numeric[index2,])))
  }
}


#gebruik wanneer aantal clusters gegeven is
kmeansMethode = function(data,n=4) {
  k = kmeans(data.matrix(data),n)
  plot(data.matrix(data), pch=k$cluster)
  points(k$centers, pch=10, cex=3, col=6, lwd=2)
}



#boom tekenen om aantal clusters te bepalen
#m is de methode van de afstanden
#de methode die je moet gebruiken zal normaal gezien gegeven worden 
#anders gebruik je gewoon de euclidean als niks is gegeven
maakClusterBoom = function(data,m="euclidean") {
  hc=hclust(dist(data.matrix(data),method=m))
  plot(hc)
}

#doe eerst maakClusterBoom om te bepalen hoeveel clusters je moet hebben
#n is het aantal clusters
#gebruik de m die je bij maakClusterBoom hebt gebruikt
hierarchischeClusters = function(data, n,m="euclidean") {
  hc=hclust(dist(data.matrix(data),method=m))
  c=cutree(hc, k=n)
  plot(data.matrix(data), col=c)
}

clustercentra = function(data,n=4){
  return(kmeansMethode(data,n)$centers)
}

#boom tekenen om aantal clusters te bepalen
#m is de methode van de afstanden
#de methode die je moet gebruiken zal normaal gezien gegeven worden
#anders gebruik je gewoon de euclidean als niks is gegeven
maakClusterBoom = function(data,m="euclidean") {
  hc=hclust(dist(data.matrix(data),method=m))
  plot(hc)
}

dendrogram = function(...){
  return(maakClusterBoom(...))
}

#doe eerst maakClusterBoom om te bepalen hoeveel clusters je moet hebben
#n is het aantal clusters
#gebruik de m die je bij maakClusterBoom hebt gebruikt
hierarchischeClusters = function(data, n,m="euclidean") {
  hc=hclust(dist(data.matrix(data),method=m))
  c=cutree(hc, k=n)
  plot(data.matrix(data), col=c)
}
