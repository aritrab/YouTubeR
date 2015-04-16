#'This a general retryfromJSOM function with a wrapper with try() and trycatch(). 
#'In general cases this functions is to be used with rjson::fromJSON. 
#'
#'
#'@param .FUN is a function which is going to be executed. 
#'@param raw.data is a function which is going to be executed. 
#'
#'@return it returns API content 
#'@export



retryfromJSON <- function(.FUN0,raw.data,.FUN1,url,.FUN2,max.attempts=5,sleep.seconds=30) 
{
  #utils::setInternet2(use=TRUE)
  x0 <- NULL
  x1 <- NULL
  raw.data<-raw.data
  url<-url
  for (i in 1:max.attempts)
  {
    f0 <- substitute(.FUN0)
    f1 <- substitute(.FUN1)
    f2 <- substitute(.FUN2)
    
    x0 <- try({#utils::setInternet2(use=TRUE)
      eval(f0)})
    if (class(x0) == "try-error")
    {
      #print("fromJSON Error")
      #Sys.sleep(sleep.seconds)
      x0 <- try({#utils::setInternet2(use=TRUE)
        raw.data<-eval(f2) 
        raw.data<-eval(f1) 
               eval(f0)})
    }
    else
    {
      return (x0)
    }
  }
  x0
}