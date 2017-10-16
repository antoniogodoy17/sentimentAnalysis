install.packages("twitteR")
library(twitteR)

#Realizar autenticación con Twitter
api_key <- "Txy9YeSDQsmgzLBNxyguOYZPV"
api_secret <- "CAwh4O9oXzsWznxR0ErsGKvkGDx66Nl1jMTuuPXaBM0Hv6q090"
access_token <- "919998032938012672-kgFsvZRar2zfJjkwptVUMT1Zm9cqDYz"
access_token_secret <- "5bAsULotAzmN9Hu9yQC2SOBJUdP9Ebv0xZZ9fQshA4HDe"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

