#' This function returns total number of video in a channel
#' 
#' @param x a string variable
#' @return n a numaric variable
#' 
#' @export 

gettotalItems<-function(channelid){
  url=paste0("https://gdata.youtube.com/feeds/api/users/",channelid,"/uploads?v=2&alt=jsonc&max-results=0")
  raw.data <- retry(httr::GET(paste0(url)),url)
  raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
  data<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
  n<-as.numeric(data$data$totalItems)
  return (n)}