#' description
#' @param vidID string variable
#' @return twitter_share numeric veriable 
#' @export 
#' 
#' 

twittershare<-function(vidID){
  url="https://www.youtube.com/watch?v="
  url=paste(url,vidID,sep = "")
  target=paste0("http://urls.api.twitter.com/1/urls/count.json?url=",url)
  rd <- readLines(target, warn="F") 
  dat <- rjson::fromJSON(rd)
  return(dat$count)}