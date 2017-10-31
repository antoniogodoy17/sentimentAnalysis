#-------------------------------INSTALAR/CARGAR LIBRERIAS-------------------------------#
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
tweets = searchTwitter("iPhone X", n=10000, lang="en")

#---------------------------------LIMPIEZA DE TWEETS---------------------------------#
#Convertir la lista de tweets a dataframe
tweets.df = twListToDF(tweets)

tweets.df$text = sapply(tweets.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
tweets.df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweets.df$text)

#Almacenar solo la parte textual de los tweets
tweetsText = tweets.df$text

#Convertir a Corpus (lista de documentos de texto) el vector de caracteres
tweetsCorpus = Corpus(VectorSource(tweetsText))

#Eliminar enlaces que comiencen con "http" 
tweetsClean = tm_map(tweetsCorpus, function(x) gsub("http[^[:space:]]*", "", x))

#Eliminar caracteres
tweetsClean = tm_map(tweetsClean, function(x) gsub("\xed[^[:space:]]*", "", x))

#Eliminar otros enlaces raros que comiencen con "/"
tweetsClean = tm_map(tweetsClean, function(x) gsub("/[^[:space:]]*", "", x))

#Eliminar símbolos raros
tweetsClean = tm_map(tweetsClean, removeWords, c('í', '½')) 

#Eliminar usuarios (@usuarioX)
tweetsClean = tm_map(tweetsClean, function(x) gsub("@[^[:space:]]*", "", x))

#Elimina signos de puntuación
tweetsClean = tm_map(tweetsClean, removePunctuation)

#Transformar todo a minúsculas
tweetsClean = tm_map(tweetsClean, content_transformer(tolower))

#Eliminar palabras innecesarias, saltos de línea y rt's.
tweetsClean = tm_map(tweetsClean, removeWords, c(stopwords("english"),"\n","rt")) 

#Eliminar números
tweetsClean = tm_map(tweetsClean, removeNumbers) 

#Eliminar la(s) palabra(s) buscada(s) o fuertemente relacionadas con la búsqueda
palabrasBuscadas = c("iphone x", "iphone", "apple","iphonex")
for (palabra in palabrasBuscadas){
  tweetsClean = tm_map(tweetsClean, removeWords, palabra) 
}

#Eliminar espacios en blanco extras
tweetsClean = tm_map(tweetsClean, stripWhitespace)

#---------------------------------NUBE DE PALABRAS---------------------------------#
#Generar la nube de palabras
wordcloud(tweetsClean, random.order = FALSE, max.words = 100, scale = c(4,0.25), col=rainbow(25))