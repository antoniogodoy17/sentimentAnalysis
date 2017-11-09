#-------------------------------INSTALAR/CARGAR LIBRERIAS-------------------------------#
packages = c("ROAuth","twitteR","base64enc","httr","devtools","tm","wordcloud","RColorBrewer")
for(lib in packages){
  if(!require(lib)){
    install.packages(lib)
  }
}
lapply(packages, library, character.only=TRUE)
# library(ROAuth) 
# library(twitteR)
# library(base64enc)
# library(httr)
# library(devtools)
# library(tm)
# library(wordcloud)

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

#---------------------------------LIMPIEZA DE TWEETS---------------------------------#
#Crear función para depurar los tweets
Clean = function(tweetsList){
  
  tweetsList$text = sapply(tweetsList$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  
  #Eliminar cualquier tipo de caracter no manejable por r
  tweetsList$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweetsList$text)
  
  #Almacenar solo la parte textual de los tweets
  tweetsText = tweetsList$text
  
  #Convertir a Corpus (lista de documentos de texto) el vector de caracteres
  tweetsCorpus = Corpus(VectorSource(tweetsText))
  
  #Eliminar enlaces que comiencen con "http"
  tweetsClean = tm_map(tweetsCorpus, function(tweet) gsub("http[^[:space:]]*", "", tweet))

  #Eliminar caracteres
  tweetsClean = tm_map(tweetsClean, function(tweet) gsub("\xed[^[:space:]]*", "", tweet))

  #Eliminar otros enlaces raros que comiencen con "/"
  tweetsClean = tm_map(tweetsClean, function(tweet) gsub("/[^[:space:]]*", "", tweet))

  #Eliminar usuarios (@usuarioX)
  tweetsClean = tm_map(tweetsClean, function(tweet) gsub("@[^[:space:]]*", "", tweet))

  #Elimina signos de puntuación
  tweetsClean = tm_map(tweetsClean, removePunctuation)
  
  #Transformar todo a minúsculas
  tweetsClean = tm_map(tweetsClean, content_transformer(tolower))
  
  #Eliminar palabras innecesarias, saltos de línea y rt's.
  tweetsClean = tm_map(tweetsClean, removeWords, c(stopwords("english"),"\n","rt")) 
  
  #Eliminar números
  tweetsClean = tm_map(tweetsClean, removeNumbers) 
  
  #Eliminar la(s) palabra(s) buscada(s) o fuertemente relacionadas con la búsqueda
  searchedWords = c("iphone x","iphonex","iphone","apple","ios","galaxy","galaxynote",
                    "note","note 8","samsung","android","google","pixel","pixel 2",
                    "pixel2","phone","smartphone","cellphone","giveaway","international","tweet")

  tweetsClean = tm_map(tweetsClean, removeWords, searchedWords) 

  #Eliminar espacios en blanco extras
  tweetsClean = tm_map(tweetsClean, stripWhitespace)
  
  return(tweetsClean)
}

iphoneTweetsClean = Clean(iphoneTweets.df)
noteTweetsClean = Clean(noteTweets.df)
pixelTweetsClean = Clean(pixelTweets.df)

#---------------------------------NUBE DE PALABRAS---------------------------------#
#Generar la nube de palabras
iphoneWordCloud = wordcloud(iphoneTweetsClean, random.order = FALSE, min.freq = 15, max.words = Inf, scale = c(2,0.25), rot.per=.1, col=brewer.pal(10,"Paired"))
noteWordcloud = wordcloud(noteTweetsClean, random.order = FALSE, min.freq = 15, max.words = Inf, scale = c(2,0.25), rot.per=.1, col=brewer.pal(10,"Paired"))
pixelWordCloud = wordcloud(pixelTweetsClean, random.order = FALSE, min.freq = 15, max.words = Inf, scale = c(2,0.25), rot.per=.1, col=brewer.pal(10,"Paired"))

#----------------------ADJUNTAR CONJUNTO DE DATOS DE PALABRAS----------------------#
#Almacenar en posWords la lista de palabras positivas ignorando la sección comentada
posWords = scan('./posWords.txt', what='character', comment.char = ';')

#Almacenar en negWords la lista de palabras negativas ignorando la sección comentada
negWords = scan('./negWords.txt', what='character', comment.char = ';')

#------------------------MAPEO DE SENTIMIENTOS A PALABRAS--------------------------#
#Mapeo de palabras positivas

iphonePosWords.match = match(posWords,iphoneTweetsClean)
notePosWords.match = match(posWords,noteTweetsClean)
pixelPosWords.match = match(posWords,pixelTweetsClean)

#Verifica que elementos no son nulos
iphonePosWords.match = !is.na(iphonePosWords.match)
notePosWords.match = !is.na(notePosWords.match)
pixelPosWords.match = !is.na(pixelPosWords.match)

#Mapeo de palabras Negativas
iphoneNegWords.match = match(negWords,iphoneTweetsClean)
noteNegWords.match = match(negWords,noteTweetsClean)
pixelNegWords.match = match(negWords,noteTweetsClean)

#Verifica que elementos no son nulos
iphoneNegWords.match = !is.na(iphoneNegWords.match)
noteNegWords.match = !is.na(noteNegWords.match)
pixelNegWords.match = !is.na(pixelNegWords.match)

#Puntuación de twets
# list.score = function(tweetsList, posWords, negWords){
#   
# }
tweet.score = function(tweet, posWords, negWords ){
  text = strsplit(tweet,' ')
  words = unlist(text)
  pos.match  = match(words, posWords)
  pos.match = !is.na(pos.match)
  neg.match = match(words,negWords)
  neg.match = !is.na(neg.match)
  score = sum(pos.match) - sum(neg.match)
  # tweet.df = list(tweet,pos.match,neg.match,score)
  # print(tweet.df[tweet]$score)
  #return(score)
}

x = lapply(iphoneTweetsClean, function(tweet) tweet.score(tweet, posWords , negWords))

