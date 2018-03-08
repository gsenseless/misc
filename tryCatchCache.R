require(archivist)
require(digest)
require(devtools)
require(testthat)
require(lubridate)


tryCatchCache4 <- function( funcName, useCache, cacheFreshness = weeks(99), ...) {  #newObjectName,
  ### use only functions defined in a global environment, 'cos of serialize() inside digest(). Cause error: "cannot allocate buffer"
  print(cacheFreshness)
  expect_true( class(cacheFreshness) == "Period", info = "@@435 use lubridate, like 'days(3)'")
  # arguments1 <- as.list(match.call())
  # arguments2 <- as.list(substitute(list(...)))[-1L]
  arguments2 <- list(...)
  # print( arguments2$funcName)
  # arguments1 <- arguments1[c(-1,-2)]
  #   print(substitute( do.call(what = cache, args =  c(list(cacheRepo = cacheDir, FUN = funcName, notOlderThan = now() ),arguments1) ) )) 
  
  print("--------")
  tmpl <- arguments2 ### this copy and .FUN for digest
  attributes(funcName) <- NULL ### according to cache function
  tmpl$.FUN <- funcName
  print(object.size(arguments2), units = "Mb")
  ## hash depends on function (its code) and arguments (its values as well)
  outputHash <- digest(tmpl)
  localTags <- showLocalRepo(repoDir = cacheDir, "tags")
  isInRepo <- localTags[localTags$tag == paste0("cacheId:",
        outputHash), , drop = FALSE]
  print(paste("@@82948 number of saves for current artifact:", nrow(isInRepo) ))
  
  tryToUseCache <- function(isInRepo) {
    # arguments2 <- eval(substitute( do.call(what = list, args = arguments1 
    #                                  , envir = parent.frame(n = 2) ) ))
    # arguments2 <- eval( do.call(what = list, args = arguments2
    #                                  , envir = parent.frame(n = 2)  ))
    if (nrow(isInRepo) > 0) {
      print(paste("@@23984--- we r going to use last saved cache date is:", max(isInRepo$createdDate)))
      lastEntry <- max(isInRepo$createdDate)
      notOlderThan <- (now() - cacheFreshness)
      if ( notOlderThan < lastEntry) {
        lastOne <- order(isInRepo$createdDate, decreasing = TRUE)[1]
        return(loadFromLocalRepo(isInRepo$artifact[lastOne],
            repoDir = cacheDir, value = TRUE))
      } else {
        print(paste("@@234 cache is too old. Last one:", lastEntry))
        return(NA)
      }
    } else {
      print(paste("@@01 cache doesn't exist." ))  # don't use stop here 'cos it will be a loop in tryCatch
      return(NA)
    }
  }
  
  cacheExistsCurDay <- F
  print(Sys.time())
  if (useCache == F) {
    if (nrow(isInRepo) > 0) {
      lastEntry <- max(isInRepo$createdDate)
      cacheExistsCurDay <- (as.Date(now()) == as.Date(lastEntry))  
    }
    if (cacheExistsCurDay == T) {
      print("@0 we r going to use cache.")
    } else {
      ### regular try catch
      callResult <- try(expr = {  # 5 +  "adf"
                                do.call(what = cache    # eval(substitute(    ### eval(quote(
                      , args =  c(list(cacheRepo = cacheDir, FUN = funcName, notOlderThan = now() )
                                  , arguments2)
                                  , envir = parent.frame() )   } )  # ) )
      if ( inherits(callResult, "try-error") ) {
        print(traceback())
        e <- attr(callResult,"condition")$message
        print(paste("@0 try for" #, funcName
                     , "error:", e)) #, e
        print(warnings())
         # if (e$message == "Error in (function (cacheRepo")
        # print("note that u won't avoid an error if arguments r specified with error.")
    
       ### report if cache exists and its date (if so). Almost copied from package inner fanction named "cache"
        # eval( substitute( do.call(what = cache
        #     , args =  c(list(cacheRepo = cacheDir, FUN = funcName
        #                      , notOlderThan = now() - days(20) )
        #                 , arguments1) ) ) )
        callResult <- tryToUseCache(isInRepo = isInRepo)
      } else {
        print(paste("@@0 expecting that new cache has been just saved."))
        print( tail(summaryLocalRepo(repoDir = cacheDir)$savesPerDay,1) )
        localTags <- showLocalRepo(repoDir = cacheDir, "tags")
        isInRepo <- localTags[localTags$tag == paste0("cacheId:",
            outputHash), , drop = FALSE]
        print(paste("@@0 number of a saves for current artifact:", nrow(isInRepo) ))
        print(paste("@@0--- last saved cache date is:", max(isInRepo$createdDate)))
      }
    }
  } 
  if (useCache == T | cacheExistsCurDay == T){
    callResult <- tryToUseCache(isInRepo)
    if (is.na(callResult)) {
      ### regular call via cache()
      callResult <- do.call(what = cache
                    , args =  c(list(cacheRepo = cacheDir, FUN = funcName, notOlderThan = now() )
                                , arguments2)
                                , envir = parent.frame() )
    }
  } 
   
  if (is.null(callResult) | is.na(callResult)) {stop("@@0 seems there was an error in tryCatch cache")}
  return(callResult)
}

cacheDir <- normalizePath("/home/blaBla/archivistRepo")
# createLocalRepo(repoDir = cacheDir)
print( summaryLocalRepo(repoDir = cacheDir) )
# showLocalRepo(repoDir = cacheDir, "tags")
# searchInLocalRepo(pattern =  "class:character", repoDir = cacheDir )

# sampleFunction <- function(x) {sample(1:10, size = x)}
# tryCatchCache4(funcName = sampleFunction, useCache = F, cacheFreshness = months(3)
#                     , x = 5)