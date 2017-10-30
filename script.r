#Instalar/cargar librerias
# packages = c("ROAuth","twitteR","base64enc","httr","devtools","tm","wordcloud")
# for(lib in packages){
#   if(!require(lib)){
#     install.packages(lib)
#   }
# }
# lapply(packages, library, character.only=TRUE)
library(ROAuth)
library(twitteR)
library(base64enc)
library(httr)
library(devtools)
library(tm)
library(wordcloud)

#Realizar autenticación con Twitter
api_key = "Y5fbQA5lJodmk0E4q4c1DYbXD"
api_secret = "f4OQTcDmCg56EiHEaSZ7Zo3POHrEn2T0QHj0KbqBmZyRWmNkVL"
access_token = "919998032938012672-6GP05oGCs1SZ6cwP9QwXw8VWulKDXDe"
access_token_secret = "YWOmWI4B3UTzOeYMVyipwVtiQvzIqygfO2cN2ySd1RZk4"
request_url = 'https://api.twitter.com/oauth/request_token'
access_url = 'https://api.twitter.com/oauth/access_token'
auth_url = 'https://api.twitter.com/oauth/authorize'

#Realizar autenticación de la app
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Obtener credencial
credential = OAuthFactory$new(consumerKey=api_key,
                              consumerSecret=api_secret,
                              requestURL=request_url,
                              accessURL=access_url,
                              authURL=auth_url)

#Autorizar credencial de la app
credential$handshake(cainfo = system.file("CurlSSL","cacert.pem",package ="RCurl"))

#Buscar y extraer tweets
x = searchTwitter("Trump",n=5000,lang="en")

#Eliminar emoticones/carácteres extraños
tweets = sapply(x$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#Convertir lista a vector
tweets = sapply(x, function(x) x$getText())

#Transformar a corpus (lista de documentos de texto)
tweetCorpus = Corpus(VectorSource(tweets))
inspect(tweetCorpus[1])

#Eliminar enlaces: elimina una cadena de texto que comience con "http" seguida por cualquier caractér sin espacios
tweetsClean = tm_map(tweetCorpus, function(x) gsub("http[^[:space:]]*", "", x))
inspect(tweetsClean[1]) 

#Elimina signos de puntuación
tweetsClean = tm_map(tweetsClean,removePunctuation)
inspect(tweetsClean[1])

#Transformar todo a minúsculas
tweetsClean = tm_map(tweetsClean,content_transformer(tolower))
inspect(tweetsClean[1])

#Eliminar palabras innecesarias
tweetsClean = tm_map(tweetsClean,removeWords, c(stopwords("english"),"\n","rt")) 
inspect(tweetsClean[1]) 

#Eliminar números
tweetsClean = tm_map(tweetsClean, removeNumbers) 
inspect(tweetsClean[1]) 

#Eliminar la(s) palabra(s) buscada(s)
palabrasBuscadas = c("donald", "trump")
for (palabra in palabrasBuscadas){
  tweetsClean = tm_map(tweetsClean, removeWords, palabra) 
}
inspect(tweetsClean[1]) 

#Eliminar espacios en blanco extras
tweetsClean = tm_map(tweetsClean, stripWhitespace) 
inspect(tweetsClean[1]) 

#Generar la nube de palabras
wordcloud(tweetsClean,random.order = FALSE, max.words = 50, scale = c(3,0.25)) 