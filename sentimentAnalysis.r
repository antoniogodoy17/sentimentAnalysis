#----------------------------------INSTALAR LIBRERIAS----------------------------------#
#Correr unicamente la primera vez
# install.packages("ROAuth")
# install.packages("twitteR")
# install.packages("base64enc")
# install.packages("httr")
# install.packages("devtools")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("RTextTools")
# install.packages("plotrix")

#-----------------------------------CARGAR LIBRERIAS-----------------------------------#
library(ROAuth)
library(twitteR)
library(base64enc)
library(httr)
library(devtools)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(RTextTools)
library(plotrix)

#-------------------------------AUTENTICACIÓN DE TWITTER-------------------------------#
#Realizar autenticación con Twitter
api_key = "Y5fbQA5lJodmk0E4q4c1DYbXD"
api_secret = "f4OQTcDmCg56EiHEaSZ7Zo3POHrEn2T0QHj0KbqBmZyRWmNkVL"
access_token = "919998032938012672-6GP05oGCs1SZ6cwP9QwXw8VWulKDXDe"
access_token_secret = "YWOmWI4B3UTzOeYMVyipwVtiQvzIqygfO2cN2ySd1RZk4"
request_url = 'https://api.twitter.com/oauth/request_token'
access_url = 'https://api.twitter.com/oauth/access_token'
auth_url = 'https://api.twitter.com/oauth/authorize'

#Realizar autenticación de la app
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#Obtener credencial
credential = OAuthFactory$new(consumerKey = api_key,
                              consumerSecret = api_secret,
                              requestURL = request_url,
                              accessURL = access_url,
                              authURL = auth_url)

#Autorizar credencial de la app
credential$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package ="RCurl")) 

#--------------------------------EXTRACCIÓN DE TWEETS--------------------------------#
#Buscar y extraer tweets
iphoneTweets = searchTwitter("iPhone X", n=5000, lang="en", since = "2017-06-01")
noteTweets = searchTwitter("Galaxy Note 8", n=5000, lang="en", since = "2017-06-01")
pixelTweets = searchTwitter("Google+Pixel 2", n=5000, lang="en", since = "2017-06-01")

#Convertir la lista de tweets a dataframe
iphoneTweets.df = twListToDF(iphoneTweets)
noteTweets.df = twListToDF(noteTweets)
pixelTweets.df = twListToDF(pixelTweets)

#Obtener nombres de columnas de los datos
names(iphoneTweets.df)

#Obtener solo la parte textual del dataframe
iphoneTweets.text = iphoneTweets.df$text
noteTweets.text = noteTweets.df$text
pixelTweets.text = pixelTweets.df$text

#---------------------------------LIMPIEZA DE TWEETS---------------------------------#
#Crear función para depurar los tweets
tweets.clean = function(tweetsList){
  cleanedTweets = sapply(tweetsList,function(row) iconv(row, "latin1", "ASCII", sub=""))
    
  #Eliminar cualquier tipo de caracter no manejable por r
  cleanedTweets = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", cleanedTweets)
  
  #Convertir a Corpus (lista de documentos de texto) el vector de caracteres
  tweetsCorpus = Corpus(VectorSource(cleanedTweets[names(cleanedTweets)]))
  
  #Eliminar enlaces que comiencen con "http"
  tweetsCorpus = tm_map(tweetsCorpus, function(tweet) gsub("http[^[:space:]]*", "", tweet))

  #Eliminar caracteres
  tweetsCorpus = tm_map(tweetsCorpus, function(tweet) gsub("\xed[^[:space:]]*", "", tweet))

  #Eliminar otros enlaces raros que comiencen con "/"
  tweetsCorpus = tm_map(tweetsCorpus, function(tweet) gsub("/[^[:space:]]*", "", tweet))

  #Eliminar usuarios (@usuarioX)
  tweetsCorpus = tm_map(tweetsCorpus, function(tweet) gsub("@[^[:space:]]*", "", tweet))

  #Elimina signos de puntuación
  tweetsCorpus = tm_map(tweetsCorpus, removePunctuation)

  #Transformar todo a minúsculas
  tweetsCorpus = tm_map(tweetsCorpus, content_transformer(tolower))

  #Eliminar palabras innecesarias, saltos de línea y rt's.
  tweetsCorpus = tm_map(tweetsCorpus, removeWords, c(stopwords("english"),"\n","rt")) 

  #Eliminar números
  tweetsCorpus = tm_map(tweetsCorpus, removeNumbers) 

  #Eliminar la(s) palabra(s) buscada(s) o fuertemente relacionadas con la búsqueda
  searchedWords = c("iphone x","iphonex","iphone","apple","ios","galaxy","galaxynote",
                       "note","note 8","samsung","android","google","pixel","pixel 2",
                       "pixel2","phone","smartphone","cellphone","giveaway","international","tweet")
     
  tweetsCorpus = tm_map(tweetsCorpus, removeWords, searchedWords) 

  #Eliminar espacios en blanco extras
  tweetsCorpus = tm_map(tweetsCorpus, stripWhitespace)
  
  cleanedTweets = data.frame(text = sapply(tweetsCorpus, as.character), stringsAsFactors = FALSE)
  
  return(cleanedTweets)
}
iphoneCleanedTweets = tweets.clean(iphoneTweets.text)
noteCleanedTweets = tweets.clean(noteTweets.text)
pixelCleanedTweets = tweets.clean(pixelTweets.text)

#----------------------ADJUNTAR CONJUNTO DE DATOS DE PALABRAS----------------------#
#Almacenar en posWords la lista de palabras positivas ignorando la sección comentada
posWords = scan('./posWords.txt', what='character', comment.char = ';')

#Almacenar en negWords la lista de palabras negativas ignorando la sección comentada
negWords = scan('./negWords.txt', what='character', comment.char = ';')

#------------FUNCIONES DE PUNTUACION POR PALABRAS POSITIVAS Y NEGATIVAS------------#
#Crear función para obtener puntuaciones
getScores = function(tweets, pos.words, neg.words){
  results.df = data.frame(matrix(nrow=0,ncol=5))
  
  for(i in 1:length(tweets)){
    tweet = tweets[i]
    words = strsplit(tweet,' ')
    words = unlist(words)
    words = words[!words %in% c(""," ","NA")]
    
    pos.match  = match(words, posWords)
    pos.match = !is.na(pos.match)
    neg.match = match(words,negWords)
    neg.match = !is.na(neg.match)
    totalPos = sum(pos.match)
    totalNeg = sum(neg.match)
    score = totalPos - totalNeg
    
    #Score = {{-5,-4,-3},{-2,-1},{0},{1,2},{3,4,5}}
    if(score <= -3){
      category = "very negative"
    }
    else if(score > -3 && score < 0){
      category = "negative"
    }
    else if(score == 0){
      category = "neutral"
    }
    else if(score > 0 && score <= 2){
      category = "positive"
    }
    else{
      category = "very positive"
    }
    if(length(words) != 0L){
      results.df[i,]= c(tweet,totalPos,totalNeg,score,category)
    }
  }
  
  colnames(results.df) = c("tweet","pos","neg","score","category")
  return(results.df)
}

#Almacenar puntuaciones en variables de los dispositivos
iphone.scores = getScores(iphoneCleanedTweets$text, posWords, negWords)
note.scores = getScores(noteCleanedTweets$text, posWords, negWords)
pixel.scores = getScores(pixelCleanedTweets$text, posWords, negWords)

#------------------------FUNCIONES DE POLARIDAD Y EMOCIONES------------------------#
#Funcion para crear la matriz necesaria en bayesianEmotions() y en bayesianPolarity()
createMatrix = function(textColumns,language="english", minDocFreq=1, minWordLength=3,weighting=weightTf){
  #Se crea el control para el Corpus
  control = list(language=language,minDocFreq=minDocFreq,minWordLength=minWordLength,weighting=weighting)
  
  #Se genera una columna en forma de matriz con los tweets
  trainingColumn = apply(as.matrix(textColumns),1,paste,collapse=" ")
  
  #Se convierte la matriz a un vector
  trainingColumn = sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")
  
  #Se convierte el vector a un corpus
  corpus = Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
  
  #Se genera la matriz 
  matrix = DocumentTermMatrix(corpus,control=control)
  
  return(matrix)
}

#Funcion para obtener las emociones correspondientes a cada tweet
bayesianEmotions = function(tweets,prior=1.0){
  matrix = createMatrix(tweets)
  
  #Leer el documento de emociones
  lexicon = read.csv(file = "emotions.csv",header=FALSE,sep=",")
  
  #Se obtiene una lista con la cantidad de palabras correspondientes a las emociones
  counts = list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
  
  #Se crea un vector vacio
  documents = c()
  
  #Se realiza una iteracion por tweet
  for(i in 1:nrow(matrix)){
    scores = list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
    doc = matrix[i,]
    words = findFreqTerms(doc,lowfreq=1)
    
    #Se obtienen las palabras del tweet iterado
    for(word in words){
      for(key in names(scores)){
        emotions = lexicon[which(lexicon[,2]==key),]
        index = pmatch(word,emotions[,1],nomatch=0)
        if(index>0){
          entry = emotions[index,]
          category = as.character(entry[[2]])
          count = counts[[category]]
          score = 1.0
          score = abs(log(score*prior/count))
          
          #Se asigna su puntuación en la categoría de emociones correspondiente
          scores[[category]] = scores[[category]]+score
        }
      }
    }
    
    #Se itera sobre las columnas de "scores"
    for(key in names(scores)){
      count = counts[[key]]
      total = counts[["total"]]
      score = abs(log(count/total))
      
      #Se asigna la puntuacion del tweet 
      scores[[key]]  = scores[[key]]+score
    }
    
    best_fit = names(scores)[which.max(unlist(scores))]
    
    #Si el radio es menor a .01 se considera como no clasificable
    if(best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01){
      best_fit = NA
    }
    
    #Se anexan las puntuaciones y la clasificación al vector vacio 
    documents = rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
  }
  
  #Se nombran las columnas del vector documents
  colnames(documents) = c("Anger","Disgust","Fear","Joy","Sadness","Surprise","Emotion")
  return(documents)
}

#Funcion para obtener la polaridad de la lista de tweets
bayesianPolarity = function(tweets,pStrong=0.5,pWeak=1.0,prior=1.0){
  matrix = createMatrix(tweets)
  #Leer el documento de subjetividad de palabras
  lexicon = read.csv(file = "subjectivity.csv",header=FALSE,sep=",")
  
  #Se obtiene una lista con la cantidad de palabras positivas, negativas, y la suma de dichas palabras
  counts = list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
  
  #Se crea un vector vacio
  documents = c()
  
  #Se realiza una iteración por tweet
  for(i in 1:nrow(matrix)){
    scores = list(positive=0,negative=0)
    doc = matrix[i,]
    words = findFreqTerms(doc,lowfreq=1)
    
    #Se obtienen las palabras del tweet iterado
    for(word in words){
      index = pmatch(word,lexicon[,1],nomatch=0)
      if(index>0){
        entry = lexicon[index,]
        polarity = as.character(entry[[2]])
        category = as.character(entry[[3]])
        count = counts[[category]]
        score = pWeak
        score = abs(log(score*prior/count))
      
        #Se asigna su puntuación en la categoría (positivo o negativo) correspondiente
        scores[[category]] = scores[[category]]+score
      }
    }
    
    #Se itera sobre las columnas de "scores"
    for(key in names(scores)){
      count = counts[[key]]
      total = counts[["total"]]
      score = abs(log(count/total))
      
      #Se asigna la puntuacion del tweet (polar)
      scores[[key]] = scores[[key]]+score
    }
    
    best_fit = names(scores)[which.max(unlist(scores))]
    ratio = as.integer(abs(scores$positive/scores$negative))
    
    #Si el radio es igual a 1, se asigna al grupo de "neutral"
    if(!length(ratio)==0){
      if(ratio==1){
        best_fit="neutral"
      }
    }
    
    #Se anexan las puntuaciones y la clasificación al vector vacio
    documents = rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
  }
  
  #Se nombran las columnas del vector documents
  colnames(documents) = c("Pos","Neg","Pos/Neg","Polarity")
  return(documents)
}

emotions = c("anger","disgust","fear","joy","sadness","surprise")
#Obtener la clasificacion de los tweets por emociones
iphoneEmotions.class = bayesianEmotions(iphone.scores$tweet)
noteEmotions.class = bayesianEmotions(note.scores$tweet)
pixelEmotions.class = bayesianEmotions(pixel.scores$tweet)

#Se obtiene la lista de las emociones que mejor se ajustan al tweet
iphoneEmotions = iphoneEmotions.class[,7]
noteEmotions = noteEmotions.class[,7]
pixelEmotions = pixelEmotions.class[,7]

#Reemplazar los valores "NA" por desconocido ("unknown")
iphoneEmotions[is.na(iphoneEmotions)] = 'unknown'
noteEmotions[is.na(noteEmotions)] = 'unknown'
pixelEmotions[is.na(pixelEmotions)] = 'unknown'

#Obtener la clasificación de los tweets por polaridad
iphonePolarity.class = bayesianPolarity(iphone.scores$tweet)
notePolarity.class = bayesianPolarity(note.scores$tweet)
pixelPolarity.class = bayesianPolarity(pixel.scores$tweet)

#Se obtiene la lista de la clasificación de polaridad
iphonePolarity = iphonePolarity.class[,4]
notePolarity = notePolarity.class[,4]
pixelPolarity = pixelPolarity.class[,4]

#Se crea un dataframe con las estadísticas de los tweets
iphonePolarity = data.frame(text=iphone.scores$tweet, emotion=iphoneEmotions, polarity=iphonePolarity, stringAsFactors=FALSE)
notePolarity = data.frame(text=note.scores$tweet, emotion=noteEmotions, polarity=notePolarity, stringAsFactors=FALSE)
pixelPolarity = data.frame(text=pixel.scores$tweet, emotion=pixelEmotions, polarity=pixelPolarity, stringAsFactors=FALSE)

#-----------------------------GRAFICACION DE RESULTADOS----------------------------#
          #--------------Subjetividad por dispositivo-------------#

#Se obtiene la cantidad de tweets pertenecientes a cada categoría por dispositivo
iphone.sub = data.frame(Pos = length(which(iphonePolarity.class=="positive")), Neg = length(which(iphonePolarity.class=="negative")), Neutral = length(which(iphonePolarity.class=="neutral")))
note.sub = data.frame(Pos = length(which(notePolarity.class=="positive")), Neg = length(which(notePolarity.class=="negative")), Neutral = length(which(notePolarity.class=="neutral")))
pixel.sub = data.frame(Pos = length(which(pixelPolarity.class=="positive")), Neg = length(which(pixelPolarity.class=="negative")), Neutral = length(which(pixelPolarity.class=="neutral")))

#Se grafican las tres figuras  
par(fig = c(0.05, 0.35, 0, 0.95), mar = c(5,4,3,0))
barplot(as.numeric(c(iphone.sub$Pos,iphone.sub$Neg,iphone.sub$Neutral)), names.arg = names(iphone.sub), space=c(0.1,1), cex.names = 0.5, xlab="iPhone X", ylab="# de Tweets",las = 1, col = brewer.pal(3,'Dark2'))
text(c(1.5,2.6,3.7),c(iphone.sub$Pos/2,iphone.sub$Neg/2,iphone.sub$Neutral/2),c(iphone.sub$Pos,iphone.sub$Neg,iphone.sub$Neutral),cex = 0.7)

par(fig = c(0.35, 0.65, 0, 0.95), mar = c(5,4,3,0), new = TRUE)
barplot(as.numeric(c(note.sub$Pos,note.sub$Neg,note.sub$Neutral)), names.arg = names(note.sub), space=c(0.1,0), cex.names = 0.5, xlab="Note 8", las = 1, col = brewer.pal(3,'Dark2'))
text(c(0.5,1.6,2.7),c(note.sub$Pos/2,note.sub$Neg/2,note.sub$Neutral/2),c(note.sub$Pos,note.sub$Neg,note.sub$Neutral),cex = 0.7)

par(fig = c(0.65, 0.95, 0, 0.95), mar = c(5,4,3,0), new = TRUE)
barplot(as.numeric(c(pixel.sub$Pos,pixel.sub$Neg,pixel.sub$Neutral)), names.arg = names(pixel.sub), space=c(0.1,0), cex.names = 0.5, xlab="Pixel 2", las = 1, col = brewer.pal(3,'Dark2'))
text(c(0.5,1.6,2.7),c(pixel.sub$Pos/2,pixel.sub$Neg/2,pixel.sub$Neutral/2),c(pixel.sub$Pos,pixel.sub$Neg,pixel.sub$Neutral),cex = 0.7)
title(main = 'Tweets Categorizados por Subjetividad por Dispositivo', outer = TRUE, line = -2)

          #--------Subjetividad por todos los dispositivos--------#
#Se calcula la suma de la cantidad de tweets pertenecientes a cada categoría
mixed.sub = data.frame(Pos = length(which(iphonePolarity.class=="positive")) + length(which(notePolarity.class=="positive")) + length(which(pixelPolarity.class=="positive")), 
                        Neg = length(which(iphonePolarity.class=="negative")) + length(which(notePolarity.class=="negative")) + length(which(pixelPolarity.class=="negative")),
                        Neutral = length(which(iphonePolarity.class=="neutral")) + length(which(notePolarity.class=="neutral")) + length(which(pixelPolarity.class=="neutral")))

#Se grafica la figura
barplot(as.numeric(c(mixed.sub$Pos,mixed.sub$Neg,mixed.sub$Neutral)), names.arg = names(mixed.sub), space=c(0.1,1), cex.names = 0.7, xlab="Categoría", ylab="# de Tweets", las = 1, col = brewer.pal(3,'Dark2'))
text(c(1.5,2.6,3.7),c(mixed.sub$Pos/2,mixed.sub$Neg/2,mixed.sub$Neutral/2),c(mixed.sub$Pos,mixed.sub$Neg,mixed.sub$Neutral),cex = 1)
title(main = 'Tweets Categorizados por Subjetividad de Todos los Dispositivos', outer = TRUE, line = -2)

          #---------------Emociones por dispositivo---------------#
#Se obtiene la cantidad de tweets pertenecientes a cada categoría para el iPhone X
iphoneAnger = sum(length(which(iphoneEmotions == "anger")))
iphoneDisgust = sum(length(which(iphoneEmotions == "disgust")))
iphoneFear = sum(length(which(iphoneEmotions == "fear")))
iphoneJoy =  sum(length(which(iphoneEmotions == "joy")))
iphoneSadness =  sum(length(which(iphoneEmotions == "sadness")))
iphoneSurprise =  sum(length(which(iphoneEmotions == "surprise")))
emotionsIphone = c(iphoneAnger,iphoneDisgust, iphoneFear, iphoneJoy, iphoneSadness, iphoneSurprise)
total = sum(iphoneAnger,iphoneDisgust,iphoneFear,iphoneJoy,iphoneSadness,iphoneSurprise)
#Se calcula su peso
percentages = c(paste(round(iphoneAnger/total*100,2),"%"),paste(round(iphoneDisgust/total*100,2),"%"),paste(round(iphoneFear/total*100,2),"%"),paste(round(iphoneJoy/total*100,2),"%"),paste(round(iphoneSadness/total*100,2),"%"),paste(round(iphoneSurprise/total*100,2),"%"))

#Se grafica la figura
pie3D(x=emotionsIphone,labels = percentages, col = brewer.pal(6,'Set1'),main="Distribución de Emociones en el iPhone X (%)", labelcex = 1.1)
legend("bottomleft", legend = emotions, fill = brewer.pal(6,'Set1'), cex = 0.8)

#Se obtiene la cantidad de tweets pertenecientes a cada categoría para el Note 8
noteAnger = sum(length(which(noteEmotions == "anger")))
noteDisgust = sum(length(which(noteEmotions == "disgust")))
noteFear = sum(length(which(noteEmotions == "fear")))
noteJoy =  sum(length(which(noteEmotions == "joy")))
noteSadness =  sum(length(which(noteEmotions == "sadness")))
noteSurprise =  sum(length(which(noteEmotions == "surprise")))
emotionsNote = c(noteAnger,noteDisgust, noteFear, noteJoy, noteSadness, noteSurprise)
total = sum(noteAnger,noteDisgust,noteFear,noteJoy,noteSadness,noteSurprise)
#Se calcula su peso
percentages = c(paste(round(noteAnger/total*100,2),"%"),paste(round(noteDisgust/total*100,2),"%"),paste(round(noteFear/total*100,2),"%"),paste(round(noteJoy/total*100,2),"%"),paste(round(noteSadness/total*100,2),"%"),paste(round(noteSurprise/total*100,2),"%"))

#Se grafica la figura
pie3D(emotionsNote, labels = percentages, col = brewer.pal(6,'Set1'),main = "Distribución de Emociones en el Note 8 (%)", labelcex = 1.1) 
legend("bottomleft", legend = emotions, fill = brewer.pal(6,'Set1'), cex = .8)

#Se obtiene la cantidad de tweets pertenecientes a cada categoría para el Pixel 2
pixelAnger = sum(length(which(pixelEmotions == "anger")))
pixelDisgust = sum(length(which(pixelEmotions == "disgust")))
pixelFear = sum(length(which(pixelEmotions == "fear")))
pixelJoy =  sum(length(which(pixelEmotions == "joy")))
pixelSadness =  sum(length(which(pixelEmotions == "sadness")))
pixelSurprise =  sum(length(which(pixelEmotions == "surprise")))
emotionsPixel = c(pixelAnger,pixelDisgust, pixelFear, pixelJoy, pixelSadness, pixelSurprise)
total = sum(pixelAnger,pixelDisgust,pixelFear,pixelJoy,pixelSadness,pixelSurprise)
#Se calcula su peso
percentages = c(paste(round(pixelAnger/total*100,2),"%"),paste(round(pixelDisgust/total*100,2),"%"),paste(round(pixelFear/total*100,2),"%"),paste(round(pixelJoy/total*100,2),"%"),paste(round(pixelSadness/total*100,2),"%"),paste(round(pixelSurprise/total*100,2),"%"))

#Se grafica la figura
pie3D(emotionsPixel, labels = percentages, col = brewer.pal(6,'Set1'), main = "Distribución de Emociones en el Pixel 2 (%)", labelcex = 1.1) 
legend("bottomleft", legend = emotions, fill = brewer.pal(6,'Set1'), cex = .8)

          #---------Emociones por todos los dispositivos----------#
#Se calcula la suma de la cantidad de tweets pertenecientes a cada categoría
mixPos = sum(length(which(iphonePolarity.class =="positive")),length(which(notePolarity.class =="positive")),length(which(notePolarity.class =="positive")))
mixNeg = sum(length(which(iphonePolarity.class =="negative")),length(which(notePolarity.class =="negative")),length(which(notePolarity.class =="negative")))
mixNeutral = sum(length(which(iphonePolarity.class =="neutral")),length(which(notePolarity.class =="neutral")),length(which(notePolarity.class =="neutral")))
devices = c(mixPos,mixNeg,mixNeutral)
total = sum(devices)
#Se calcula su peso
percentages = c(paste(round(mixPos*100/total,2),"%"), paste(round(mixNeg*100/total,2),"%"),paste(round(mixNeutral*100/total,2),"%"))

#Se grafica la figura
pie3D(devices, labels = percentages, col = brewer.pal(3,'Dark2'), main = "Distribución de Emociones en Todos los Dispositivos (%)", labelcex = 1.1)
legend("bottomleft",legend = c("Positive","Negative","Neutral"), fill = brewer.pal(3,'Dark2'), cex = .8)

          #---Puntuaciones de tweets por todos los dispositivos---#
getAccumulated = function(list){
  accum = c()
  sum = 0
  for(i in 1:length(list)){
    if(!is.na(list[i])){
      sum = sum+as.numeric(list[i])
      accum = c(accum,sum)
    }
  }
  return(accum)
}

iphoneAccum = getAccumulated(iphone.scores$score)
noteAccum = getAccumulated(note.scores$score)
pixelAccum = getAccumulated(pixel.scores$score)

plot(iphoneAccum,type = "l", ylim = c(min(iphoneAccum,noteAccum,pixelAccum),max(iphoneAccum,noteAccum,pixelAccum)) ,col = brewer.pal(8,'Set1')[3], xlab = "# de Tweets", ylab = "Puntuación Acumulada", main = "Puntuación Histórica Acumulada por Dispositivo")
lines(noteAccum, type = "l", col = brewer.pal(8,'Set1')[4])
lines(pixelAccum, type = "l", col = brewer.pal(8,'Set1')[1])
legend(0,2900,c("iPhone X","Note 8","Pixel 2"),lty=c(1), lwd=c(2.5),col = c(brewer.pal(8,'Set1')[3],brewer.pal(8,'Set1')[4],brewer.pal(8,'Set1')[1]))

          #------------Nube de palabras por emociones-------------#

#Funcion para obtener un TermDocumentMatrix para la nube
getTdm = function(tweets,tweetsEmotions){
  #Se obtienen las emociones
  wcEmotions = levels(factor(emotions))
  nEmotions = length(wcEmotions)
  wcEmotions.docs = rep('', nEmotions)
  
  #Se separa el texto por emociones
  for(i in 1:nEmotions)
  {
    tmp = tweets$text[tweetsEmotions == wcEmotions[i]]
    wcEmotions.docs[i] = paste(tmp, collapse=' ')
  }
  
  #Se crea un corpus y se convierte a un TermDocumentMatrix
  corpus = Corpus(VectorSource(wcEmotions.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = wcEmotions
  return (tdm)
}

#Se almacena el corpus de cada dispositivo
iphoneCorpus = getTdm(iphoneCleanedTweets,iphoneEmotions)
noteCorpus = getTdm(noteCleanedTweets,noteEmotions)
pixelCorpus = getTdm(pixelCleanedTweets,pixelEmotions)

#Se construye una estructura con los datos de todos los dispositivos
mixedTweets = rbind(iphoneCleanedTweets,noteCleanedTweets,pixelCleanedTweets)
mixedEmotions = rbind(iphoneEmotions,noteEmotions,pixelEmotions)

#Se almacena el corpus
mixedCorpus = getTdm(mixedTweets,mixedEmotions)

#Se grafican las nubes de palabras comparativas por emociones
#iPhone X
comparison.cloud(iphoneCorpus, random.order = FALSE, max.words = 1000, rot.per=.15, colors = brewer.pal(emotions, 'Dark2'), scale = c(4,0.5), title.size = 1)
title(main = "Nube de Palabras de Emociones del iPhone X", outer = TRUE, line = -0.7)
#Note 8
comparison.cloud(noteCorpus, random.order = FALSE, max.words = 1000, rot.per=.15, colors = brewer.pal(emotions, 'Dark2'), scale = c(4,0.5), title.size = 1)
title(main = "Nube de Palabras de Emociones del Note 8", outer = TRUE, line = -0.7)
#Pixel 2
comparison.cloud(pixelCorpus, random.order = FALSE, max.words = 1000, rot.per=.15, colors = brewer.pal(emotions, 'Dark2'), scale = c(4,0.5), title.size = 1)
title(main = "Nube de Palabras de Emociones del Pixel 2", outer = TRUE, line = -0.7)
#Todos los dispositivos
comparison.cloud(mixedCorpus, random.order = FALSE, max.words = 1000, rot.per=.15, colors = brewer.pal(emotions, 'Dark2'), scale = c(4,0.5), title.size = 1)
title(main = "Nube de Palabras de Emociones de Todos los Dispositivos", outer = TRUE, line = -0.7)

          #-----------------Frecuencia de palabras----------------#
#Se obtiene la frecuencia de las palabras
termsFreq = rowSums(as.matrix(mixedCorpus))
mostFreq = subset(termsFreq,termsFreq>=50)
df = data.frame(term = names(mostFreq), freq=mostFreq)

#Se grafica la figura
par(mai=c(1,1.5,1,1))
barplot(mostFreq, xlim=c(0,round(max(mostFreq)/100,0)*100), col = c(brewer.pal(name = 'Dark2',n = 8),brewer.pal(name = 'Paired',n = 12)), horiz = TRUE, las=1, cex.names = 0.5, space = c(0.5,0))
par(mar = c(5,7,4,2) + 0.1)
title(main = "Palabras Más Frecuentes por Todos los Dispositivos", ylab = "Palabras", xlab = "Frecuencia")