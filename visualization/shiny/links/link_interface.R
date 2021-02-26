source('server/server_utils.R')

LINK.DELIMITER = '&'

#returns a named vector
#if url is www.jheem.org/?preset=123
# --> id = 123
get.link.key.values <- function(id, method=c('s3', 'db')[1]) {
  # @Todd: server_utils.R/getPresetIdFromUrl() also fetches URL from session
  # - Code below taken from server.routes.runModel.old.R:70
  if (method == 'db') {
    # 1. get record
    presetTable.df = db.presets.read.all()
    presetRecord = presetTable.df[presetTable.df$id==id,]
    presetStr = presetRecord$urlQueryParamString
    # 2. parse it
    presets.list = presets.urlQueryParamString.parse(presetStr)  # list
    presets = unlist(presets)
  } else if (method == 's3') {
    presets = presets.load(id)  
  }
  
  return(presets)
}

#key values is a named list
#each element of this list is a character vector (which may be named or not)
#also shows modal.
set.link.key.values <- function(id, key.values) {
  # This test was ok. Can delete these comments:
  # key.values = c('key1'='lkjslkdfjl', 'key2'='eopwtuq', 'key3'='cxmvnzxvi')
  # queryStr = presets.urlQueryParamString.create(key.values)
  handleCreatePreset(id, key.values)
}

# Random String
# Quicker than my own implementation
# https://stackoverflow.com/questions/42734547/generating-random-strings
randString <- function(
  characters=0, numbers=0, symbols=0, lowerCase=0, upperCase=0
) {
  ASCII <- NULL
  if(symbols>0) 
    ASCII <- c(ASCII, sample(c(33:47, 58:34, 91:96, 123:126), symbols))
  if(numbers>0) ASCII <- c(ASCII, sample(48:57, numbers))
  if(upperCase>0) ASCII <- c(ASCII, sample(65:90, upperCase))
  if(lowerCase>0) ASCII <- c(ASCII, sample(97:122, lowerCase))
  if(characters>0) ASCII <- c(ASCII, sample(c(65:90, 97:122), characters))
  
  return( rawToChar(as.raw(sample(ASCII, length(ASCII)))) )
}

#return a random 5-character alphanumeric key that is not already in the DB
get.new.link.id <- function(method=c('s3', 'db')[1]) {
  if (method == 'db') 
    ids = db.presets.read.all()$id
  else if (method == 's3')
    ids = presets.list()  %>% str_replace('.Rdata', '')
  while (TRUE) {
    id = randString(characters=3, numbers=2)
    if (!(id %in% ids))
      break
  }
  return(id)
}
