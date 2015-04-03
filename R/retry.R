#'This a general retry function with a wrapper with try() and trycatch(). 
#'In general cases this functions is to be used with readLines or 
#'rjson::fromJSON. 
#'
#'@param .FUN is a function which is going to be executed. 
#'@param url is a function which is going to be executed. 
#'
#'@return it returns API content 
#'@export



retry <- function(.FUN,url,max.attempts=5,sleep.seconds=60) 
{
  #utils::setInternet2(use=TRUE)
  x <- NULL
  url<-url
  for (i in 1:max.attempts)
  {
    f <- substitute(.FUN)
    
    x <- try({#utils::setInternet2(use=TRUE)
      eval(f)})
    if (class(x) == "try-error")
    {
      #print("Readline Error")
      #Sys.sleep(sleep.seconds)
      x <- try({#utils::setInternet2(use=TRUE)
               eval(f)})
    }
    else
    {
      return (x)
    }
  }
  x
}