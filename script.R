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

#Sentiment Palabras
# https://cran.r-project.org/src/contrib/Archive/sentiment/
# http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz

# interface to the C code that implements Porter's word stemming algorithm for collapsing words to a common root to aid comparison of texts. There 
# install_url('http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz')

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
  #   
  # #Eliminar caracteres
  tweetsCorpus = tm_map(tweetsCorpus, function(tweet) gsub("\xed[^[:space:]]*", "", tweet))
  #   
  # #Eliminar otros enlaces raros que comiencen con "/"
  tweetsCorpus = tm_map(tweetsCorpus, function(tweet) gsub("/[^[:space:]]*", "", tweet))
  #   
  # #Eliminar usuarios (@usuarioX)
  tweetsCorpus = tm_map(tweetsCorpus, function(tweet) gsub("@[^[:space:]]*", "", tweet))
  # 
  # #Elimina signos de puntuación
  tweetsCorpus = tm_map(tweetsCorpus, removePunctuation)
  #   
  # #Transformar todo a minúsculas
  tweetsCorpus = tm_map(tweetsCorpus, content_transformer(tolower))
  #   
  # #Eliminar palabras innecesarias, saltos de línea y rt's.
  tweetsCorpus = tm_map(tweetsCorpus, removeWords, c(stopwords("english"),"\n","rt")) 
  #   
  # #Eliminar números
  tweetsCorpus = tm_map(tweetsCorpus, removeNumbers) 
  #   
  # #Eliminar la(s) palabra(s) buscada(s) o fuertemente relacionadas con la búsqueda
  searchedWords = c("iphone x","iphonex","iphone","apple","ios","galaxy","galaxynote",
                    "note","note 8","samsung","android","google","pixel","pixel 2",
                    "pixel2","phone","smartphone","cellphone","giveaway","international","tweet")
  
  tweetsCorpus = tm_map(tweetsCorpus, removeWords, searchedWords) 
  #  
  # #Eliminar espacios en blanco extras
  tweetsCorpus = tm_map(tweetsCorpus, stripWhitespace)
  
  cleanedTweets = data.frame(text = sapply(tweetsCorpus, as.character), stringsAsFactors = FALSE)
  #cleanedTweets = list(text = sapply(tweetsCorpus, as.character))
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

iphone.sub = data.frame(Pos = length(which(iphone.scores$category=="positive")) + length(which(iphone.scores$category=="very positive")), 
                        Neg = length(which(iphone.scores$category=="negative")) + length(which(iphone.scores$category=="very negative")),
                        Neutral = length(which(iphone.scores$category=="neutral")))

note.sub = data.frame(Pos = length(which(note.scores$category=="positive")) + length(which(note.scores$category=="very positive")), 
                      Neg = length(which(note.scores$category=="negative")) + length(which(note.scores$category=="very negative")),
                      Neutral = length(which(note.scores$category=="neutral")))

pixel.sub = data.frame(Pos = length(which(pixel.scores$category=="positive")) + length(which(pixel.scores$category=="very positive")), 
                       Neg = length(which(pixel.scores$category=="negative")) + length(which(pixel.scores$category=="very negative")),
                       Neutral = length(which(pixel.scores$category=="neutral")))

par(fig = c(0.05, 0.35, 0, 0.95), mar = c(5,4,3,0))
barplot(as.numeric(c(iphone.sub$Pos,iphone.sub$Neg,iphone.sub$Neutral)), names.arg = names(iphone.sub), space=c(0.1,1), cex.names = 0.7, xlab="iPhone X", las = 1, col = brewer.pal(3,'Dark2'))
text(c(1.5,2.6,3.7),c(iphone.sub$Pos/2,iphone.sub$Neg/2,iphone.sub$Neutral/2),c(iphone.sub$Pos,iphone.sub$Neg,iphone.sub$Neutral),cex = 0.7)

par(fig = c(0.35, 0.65, 0, 0.95), mar = c(5,4,3,0), new = TRUE)
barplot(as.numeric(c(note.sub$Pos,note.sub$Neg,note.sub$Neutral)), names.arg = names(note.sub), space=c(0.1,0), cex.names = 0.7, xlab="Note 8", las = 1, col = brewer.pal(3,'Dark2'))
text(c(0.5,1.6,2.7),c(note.sub$Pos/2,note.sub$Neg/2,note.sub$Neutral/2),c(note.sub$Pos,note.sub$Neg,note.sub$Neutral),cex = 0.7)

par(fig = c(0.65, 0.95, 0, 0.95), mar = c(5,4,3,0), new = TRUE)
barplot(as.numeric(c(pixel.sub$Pos,pixel.sub$Neg,pixel.sub$Neutral)), names.arg = names(pixel.sub), space=c(0.1,0), cex.names = 0.7, xlab="Pixel 2", las = 1, col = brewer.pal(3,'Dark2'))
text(c(0.5,1.6,2.7),c(pixel.sub$Pos/2,pixel.sub$Neg/2,pixel.sub$Neutral/2),c(pixel.sub$Pos,pixel.sub$Neg,pixel.sub$Neutral),cex = 0.7)
title(main = 'Tweets Categorizados por Subjetividad por Dispositivo', outer = TRUE, line = -2)

#--------Subjetividad por todos los dispositivos--------#

mixed.sub = data.frame(Pos = length(which(iphone.scores$category=="positive")) + length(which(iphone.scores$category=="very positive"))
                       + length(which(note.scores$category=="positive")) + length(which(note.scores$category=="very positive"))
                       + length(which(pixel.scores$category=="positive")) + length(which(pixel.scores$category=="very positive")), 
                       Neg = length(which(iphone.scores$category=="negative")) + length(which(iphone.scores$category=="very negative"))
                       + length(which(note.scores$category=="negative")) + length(which(note.scores$category=="very negative"))
                       + length(which(pixel.scores$category=="negative")) + length(which(pixel.scores$category=="very negative")),
                       Neutral = length(which(iphone.scores$category=="neutral"))
                       + length(which(note.scores$category=="neutral"))
                       + length(which(pixel.scores$category=="neutral")))

barplot(as.numeric(c(mixed.sub$Pos,mixed.sub$Neg,mixed.sub$Neutral)), names.arg = names(mixed.sub), space=c(0.1,1), cex.names = 0.7, xlab="Categoría", las = 1, col = brewer.pal(3,'Dark2'))
text(c(1.5,2.6,3.7),c(mixed.sub$Pos/2,mixed.sub$Neg/2,mixed.sub$Neutral/2),c(mixed.sub$Pos,mixed.sub$Neg,mixed.sub$Neutral),cex = 1)
title(main = 'Tweets Categorizados por Subjetividad de todos los dispositivos', outer = TRUE, line = -2)

#---------------Emociones por dispositivo---------------#
lbl <- c("Anger","Disgust","Fear","Joy","Sadness","Surprise","Emotion")
#Iphone Pie
iphoneAnger <- sum(length(which(iphoneEmotions == "anger")))
iphoneDisgust <- sum(length(which(iphoneEmotions == "disgust")))
iphoneFear <- sum(length(which(iphoneEmotions == "fear")))
iphoneJoy <-  sum(length(which(iphoneEmotions == "joy")))
iphoneSadness <-  sum(length(which(iphoneEmotions == "sadness")))
iphoneSurprise <-  sum(length(which(iphoneEmotions == "surprise")))
iphoneEmotion <-  sum(length(which(iphoneEmotions == "emotion")))
#iphoneUnknown <-sum(length(which(iphoneEmotions == "unknown")))
emotionsIphone <- c(iphoneAnger,iphoneDisgust, iphoneFear, iphoneJoy, iphoneSadness, iphoneSurprise, iphoneEmotion)
Iphonepercen <- sum(emotionsIphone)
Iphonelbl<- c((iphoneAnger*100)/Iphonepercen,(iphoneDisgust*100)/Iphonepercen,(iphoneFear*100)/Iphonepercen,(iphoneJoy*100)/Iphonepercen,(iphoneSadness*100)/Iphonepercen,(iphoneSurprise*100)/Iphonepercen,(iphoneEmotion*100)/Iphonepercen)

for( i in 1:7){
  x<- as.numeric(Iphonelbl[i])
  x<-format(round(x,2))
  Iphonelbl[i] <-paste(x ,"%")
}

pie(emotionsIphone, labels = Iphonelbl, col = c("red","purple","yellow","green","blue","orange","pink"),main = "Iphone Emotions") 
legend("bottomleft", legend = lbl, fill = c("red","purple","yellow","green","blue","orange","pink"), cex = .5)

#Note Pie
noteAnger <- sum(length(which( noteEmotions == "anger")))
noteDisgust <- sum(length(which(noteEmotions == "disgust")))
noteFear <- sum(length(which(noteEmotions == "fear")))
noteJoy <-  sum(length(which(noteEmotions == "joy")))
noteSadness <-  sum(length(which(noteEmotions == "sadness")))
noteSurprise <-  sum(length(which(noteEmotions == "surprise")))
noteEmotion <-  sum(length(which(noteEmotions == "emotion")))
#iphoneUnknown <-sum(length(which(iphoneEmotions == "unknown")))
emotionsNote <- c(noteAnger,noteDisgust, noteFear, noteJoy, noteSadness, noteSurprise, noteEmotion)
Notepercen <- sum(emotionsNote)
Notelbl<- c((noteAnger*100)/Notepercen,(noteDisgust*100)/Notepercen,(noteFear*100)/Notepercen,(noteJoy*100)/Notepercen,(noteSadness*100)/Notepercen,(noteSurprise*100)/Notepercen,(noteEmotion*100)/Notepercen)

for( i in 1:7){
  x<- as.numeric(Notelbl[i])
  x<-format(round(x,2))
  Notelbl[i] <-paste(x ,"%")
}


pie(emotionsNote, labels = Notelbl, col = c("red","purple","yellow","green","blue","orange","pink"),main = "Note Emotions") 
legend("bottomleft", legend = lbl, fill = c("red","purple","yellow","green","blue","orange","pink"), cex = .5)

#Pixel Pie
pixelAnger <- sum(length(which( pixelEmotions == "anger")))
pixelDisgust <- sum(length(which(pixelEmotions == "disgust")))
pixelFear <- sum(length(which(pixelEmotions == "fear")))
pixelJoy <-  sum(length(which(pixelEmotions == "joy")))
pixelSadness <-  sum(length(which(pixelEmotions == "sadness")))
pixelSurprise <-  sum(length(which(pixelEmotions == "surprise")))
pixelEmotion <-  sum(length(which(pixelEmotions == "emotion")))
emotionsPixel <- c(pixelAnger,pixelDisgust, pixelFear, pixelJoy, pixelSadness, pixelSurprise, pixelEmotion)
pixelpercen <- sum(emotionsPixel)
Pixellbl<- c((pixelAnger*100)/pixelpercen,(pixelDisgust*100)/pixelpercen,(pixelFear*100)/pixelpercen,(pixelJoy*100)/pixelpercen,(pixelSadness*100)/pixelpercen,(pixelSurprise*100)/pixelpercen,(pixelEmotion*100)/pixelpercen)

for( i in 1:7){
  x<- as.numeric(Pixellbl[i])
  x<-format(round(x,2))
  Pixellbl[i] <-paste(x ,"%")
}

pie(emotionsPixel, labels =  Pixellbl, col = c("red","purple","yellow","green","blue","orange","pink"), main = "Pixel Emotions") 
legend("bottomleft", legend = lbl, fill = c("red","purple","yellow","green","blue","orange","pink"), cex = .5)

#---------Emociones por todos los dispositivos----------#
library(plotrix)
labe = c("Positivo","Negativo","Neutral")
dispositivosPos <- sum(length(which(iphone.scores$category =="positive")))
dispositivosNeg <- sum(length(which(iphone.scores$category =="negative")))
dispositivosNeutral <-sum(length(which(iphone.scores$category =="neutral")))
dispositivos <- c(dispositivosPos,dispositivosNeg,dispositivosNeutral)
totalSumatoria <- sum(dispositivos)
etiquetas <- c((dispositivosPos*100)/totalSumatoria, (dispositivosNeg*100)/totalSumatoria,(dispositivosNeutral*100)/totalSumatoria)
print(dispositivosPos)

for( i in 1:3){
  x<- as.numeric(etiquetas[i])
  x<-format(round(x,2))
  etiquetas[i] <-paste(x ,"%")
}
dispositivosEmo <-c(dispositivosPos,dispositivosNeg,dispositivosNeutral)
pie3D(dispositivos, col = c("red","blue","green"), labels = etiquetas)
legend("bottomleft",legend = labe, fill = c("red","blue","green"), cex = .35)

#--------Puntuaciones de tweets por dispositivo---------#

# totalScore = 0
# for(score in iphone.scores$score){
#   if(!is.na(score)){
#     totalScore = totalScore + as.numeric(score)
#   }
# }
# totalScore
# 
# sum(as.numeric(iphone.scores$score))     

#---Puntuaciones de tweets por todos los dispositivos---#



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
title(main = "Nube de palabras de emociones del iPhone X", outer = TRUE, line = -0.7)
#Note 8
comparison.cloud(noteCorpus, random.order = FALSE, max.words = 1000, rot.per=.15, colors = brewer.pal(nEmotions, 'Dark2'), scale = c(4,0.5), title.size = 1)
title(main = "Nube de palabras de emociones del Note 8", outer = TRUE, line = -0.7)
#Pixel 2
comparison.cloud(pixelCorpus, random.order = FALSE, max.words = 1000, rot.per=.15, colors = brewer.pal(nEmotions, 'Dark2'), scale = c(4,0.5), title.size = 1)
title(main = "Nube de palabras de emociones del Pixel 2", outer = TRUE, line = -0.7)
#Todos los dispositivos
comparison.cloud(mixedCorpus, random.order = FALSE, max.words = 1000, rot.per=.15, colors = brewer.pal(nEmotions, 'Dark2'), scale = c(4,0.5), title.size = 1)
title(main = "Nube de palabras de emociones de todos los dispositivos", outer = TRUE, line = -0.7)

#-----------------Frecuencia de palabras----------------#

termsFreq = rowSums(as.matrix(mixedCorpus))
mostFreq = subset(termsFreq,termsFreq>=50)
df = data.frame(term = names(mostFreq), freq=mostFreq)

par(mai=c(1,1.5,1,1))
barplot(mostFreq, xlim=c(0,round(max(mostFreq)/100,0)*100), col = c(brewer.pal(name = 'Dark2',n = 8),brewer.pal(name = 'Paired',n = 12)), horiz = TRUE, las=1, cex.names = 0.5, space = c(0.5,0))
par(mar = c(5,7,4,2) + 0.1)
title(main = "Palabras Más Frecuentes por Todos los Dispositivos", ylab = "Palabras", xlab = "Frecuencia")

################################################################################
################################################################################

#https://github.com/timjurka/sentiment/tree/master/sentiment/R   -> Funciones de sentimiento

#https://rstudio-pubs-static.s3.amazonaws.com/66739_c4422a1761bd4ee0b0bb8821d7780e12.html

###############################################################################
###############################################################################