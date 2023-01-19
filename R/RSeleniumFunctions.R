# Author: Rachel Addlespurger(rachel.addlespurger@abbott.com) ####

# Message to Log function ####
Message2Log <- function(M) {
  # Log
  log_string <- paste(now(), (M))
  cat(log_string, file = log_file, sep="\n")
  # Message in Console
  message(M)
}
#Ex.
#log_file <- file("X:\\Data\\D107641_LA\\CDA_Shr_fldr\\Retail Analytics\\Automation Script Logs\\Vendor Central Log.txt", open="a")

# Function receives Chrome Driver Information ####
getChromeVersion <- function() {
  chrome_version <-
    system2(command = "wmic",
            args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
            stdout = TRUE,
            stderr = TRUE) %>%
    stringr::str_extract(pattern = "(?<=Version=)(\\d+\\.){3}") %>%
    magrittr::extract(!is.na(.))

  versions = binman::list_versions("chromedriver")

  chrome_version %>%
    magrittr::extract(!is.na(.)) %>%
    stringr::str_replace_all(pattern = "\\.",
                             replacement = "\\\\.") %>%
    paste0("^",  .) %>%
    stringr::str_subset(string = dplyr::last(versions)) %>%
    as.numeric_version() %>%
    max() %>%
    as.character()
}

getChromeVersion2 <- function() {
  chrome_version <-
    system2(command = "wmic",
            args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
            stdout = TRUE,
            stderr = TRUE) %>%
    stringr::str_extract(pattern = "(?<=Version=)(\\d+\\.){3}") %>%
    magrittr::extract(!is.na(.))

  versions = binman::list_versions("chromedriver")

  chrome_version %>%
    magrittr::extract(!is.na(.)) %>%
    stringr::str_replace_all(pattern = "\\.",
                             replacement = "\\\\.") %>%
    paste0("^",  .) %>%
    stringr::str_subset(string = dplyr::last(versions)) %>%
    as.numeric_version() %>%
    min() %>%
    as.character()
}

# Start Selenium Function ####
start_selenium <- function(attempted = 0, condition = "Success starting Selenium web driver!", browserpreference = "chrome"){

  require(netstat)
  require(RSelenium)
  require(dplyr)
  require(stringr)
  require(binman)
  require(wdman)
  require(retry)
  require(XML)
  require(rJava)

  if(attempted >= 2){
    return("Failure starting Selenium!")
  }

  tryCatch({

    # Create Java Task List Before Starting Selenium
    .GlobalEnv$before.tasklist <-  system2("tasklist", stdout = TRUE )
    .GlobalEnv$before.tasklist <- before.tasklist[-(1:3)]
    .GlobalEnv$df <- as.data.frame(before.tasklist)
    .GlobalEnv$df$taskname <- trimws(substr(before.tasklist, 1, 29))
    .GlobalEnv$df$pid <- as.integer(substr(before.tasklist, 30, 34))
    .GlobalEnv$df$before.tasklist <- NULL
    .GlobalEnv$df.java.before <- df[df$taskname == 'java.exe', ]

    # Define args and Connect to Chrome
    .GlobalEnv$eCaps <- list(chromeOptions = list(args = c('--disable-gpu'
                                                           ,'--disable-dev-shm-usage'
                                                           , '--no-sandbox'
                                                           , '--start-maximized'
                                                           ,'--disable-blink-features'
                                                           ,'--disable-blink-features=AutomationControlled'
                                                           ,'--disable-browser-side-navigation'
                                                           ,'--dns-prefetch-disable'
                                                           #,'--disable-popup-blocking'
                                                           #,'--disable-extensions'
                                                            )))
    .GlobalEnv$chrome.version<-getChromeVersion()

    options(warn=-1)

    .GlobalEnv$driver <- rsDriver(browser=  c("chrome") #paste0(browserpreference)
                                  ,version = "latest"
                                  ,chromever= chrome.version
                                  ,port= free_port()
                                  ,extraCapabilities=eCaps)
    Sys.sleep(2)

    try(sel <- wdman::selenium(), silent = TRUE)
    try(FindError <- sel$log(), silent = TRUE)
    try(FindError <- as.data.frame(FindError$stderr), silent = TRUE)

    if(exists("FindError") == TRUE & grepl('Error',FindError[1])==TRUE){
      message("issue found - checking older 'chrome.version'")
      .GlobalEnv$chrome.version<-getChromeVersion2()
      .GlobalEnv$driver <- rsDriver(browser=  c("chrome") #paste0(browserpreference)
                                    ,version = "latest"
                                    ,chromever= chrome.version
                                    ,port= free_port()
                                    ,extraCapabilities=eCaps)
      rm(FindError)
    }

    Sys.sleep(2)

    try(sel <- wdman::selenium(), silent = TRUE)
    try(FindError <- sel$log(), silent = TRUE)
    try(FindError <- as.data.frame(FindError$stderr), silent = TRUE)

    if(exists("FindError") == TRUE & grepl('Error',FindError[1])==TRUE & attempted > 2 ){
        stop("Failure starting Selenium!")
    } else { message("Successful connect to remDr")
             options(warn=0)}

    .GlobalEnv$remDr <- driver[['client']]

    # Message Port
    message(c("Using Port : ",driver$client$port))

    # Create Java Task List After Starting Selenium
    .GlobalEnv$after.tasklist <-  system2("tasklist", stdout = TRUE )
    .GlobalEnv$after.tasklist <- after.tasklist[-(1:3)]
    .GlobalEnv$df <- as.data.frame(after.tasklist)
    .GlobalEnv$df$taskname <- trimws(substr(after.tasklist, 1, 29))
    .GlobalEnv$df$pid <- as.integer(substr(after.tasklist, 30, 34))
    .GlobalEnv$df$after.tasklist <- NULL
    .GlobalEnv$df.java.after <- df[df$taskname == 'java.exe', ]

    # Find the new Java Tast
    .GlobalEnv$new.java <- subset(df.java.after, !(df.java.after$pid %in% df.java.before$pid))

    # Remove
    .GlobalEnv$df <- NULL
    .GlobalEnv$before.tasklist <- NULL
    .GlobalEnv$after.tasklist <- NULL
    .GlobalEnv$df.java.before <- NULL
    .GlobalEnv$df.java.after <- NULL
    .GlobalEnv$eCaps <- NULL

    # Close Startup Window
    #remDr$closeWindow()

  }, error = function(error_condition) {
    if(grepl("already in use",error_condition, fixed = TRUE)){
      tryCatch(driver$close(),error = function(error_condition){message(error_condition)})
      rD[["server"]]$stop()
      attempted <- attempted + 1
      condition <<- start_selenium(attempted)
      options(warn=0)
      }
    })
  return(condition)
}

# Kill New Java Task Function - Determined in start_selenium()
KillCurrentJava <- function(x) {

  try(remDr$close(),silent = TRUE)
  try(remDr$quit(),silent = TRUE)
  try(driver$server$stop(),silent = TRUE)
  try(gc(),silent = TRUE)

  for (j in 1:nrow(new.java)){
    pid.to.kill <- as.integer(new.java$pid[j])
    message(c("Killing Pid - ", pid.to.kill))
    taskkill.cmd <- paste( "taskkill" , "/F /PID" , pid.to.kill)
    system( taskkill.cmd )
  }
}

# Retry Element until No Error, Then Click ####
RetryElement <- function(using,value){
  .GlobalEnv$click <- retry(driver$client$findElement(using = using , value = value),
                            until = ~remDr$status[1]=='0',
                            timeout = 6000,
                            silent = TRUE)
  .GlobalEnv$click$clickElement()
}

# Retry Element until No Error ####
RetryElementxClick <- function(using,value){
  .GlobalEnv$click <- retry(driver$client$findElement(using = using , value = value),
                            until = ~remDr$status[1]=='0',
                            timeout = 6000,
                            silent = TRUE)
}
