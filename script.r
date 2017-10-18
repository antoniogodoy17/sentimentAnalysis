install.packages("twitteR")
#install.packages("ROAuth")

library(ROAuth)
library(twitteR)
library(base64enc)
library(httr)

#Realizar autenticación con Twitter
api_key = "lnSwUgN8bZbhcM7C9sHjHBzDE"
api_secret = "btut6khdihDlCylopWvnJyQs8GMJr2KMt5L3UkSPEoqbQEAXMA"
access_token = "919998032938012672-e3jqNBq4mWb9iO3T11V5M8vCkmUUlGW"
access_token_secret = "Ek70jTOUkqRA2YN3BzKM1npBdsf4JL8bS1muXBjDNLqrB"
request_url = 'https://api.twitter.com/oauth/request_token'
access_url = 'https://api.twitter.com/oauth/access_token'
auth_url = 'https://api.twitter.com/oauth/authorize'

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

credential = OAuthFactory$new(consumerKey=api_key,
                        consumerSecret=api_secret,
                        requestURL=request_url,
                        accessURL=access_url,
                        authURL=auth_url)

credential$handshake(cainfo = system.file("CurlSSL","cacert.pem",package ="RCurl"))

x = searchTwitter('real madrid')