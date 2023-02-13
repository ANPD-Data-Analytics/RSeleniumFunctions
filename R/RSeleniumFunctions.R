# Author: Rachel Addlespurger(rachel.addlespurger@abbott.com) ####

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

  .GlobalEnv$versionsdf <- as.data.frame(versions)

  .GlobalEnv$chrome.version <- chrome_version %>%
    magrittr::extract(!is.na(.)) %>%
    stringr::str_replace_all(pattern = "\\.",
                             replacement = "\\\\.") %>%
    paste0("^",  .) %>%
    stringr::str_subset(string = dplyr::last(versions)) %>%
    as.numeric_version() %>%
    min() %>%
    as.character()

  try(.GlobalEnv$chrome_version_row <- which(versionsdf$win32 == chrome.version), silent = TRUE)
  try(.GlobalEnv$chrome.version2 <- versionsdf [(chrome_version_row - 1),], silent = TRUE)
  try(.GlobalEnv$chrome.version3 <- versionsdf [(chrome_version_row - 2),], silent = TRUE)
  try(.GlobalEnv$chrome.version4 <- versionsdf [(chrome_version_row - 3),], silent = TRUE)
  #try(.GlobalEnv$chrome.version3 <- versionsdf [(chrome_version_row + 1),], silent = TRUE)

}

# Start Selenium Function ####
start_selenium <- function(browserpreference = "chrome"){

  require(netstat)
  require(RSelenium)
  require(dplyr)
  require(stringr)
  require(binman)
  require(wdman)
  require(retry)
  require(XML)
  require(rJava)

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

    getChromeVersion()

    options(warn=-1)

    try((statusdf <- as.data.frame(driver$client$getStatus())), silent = TRUE)  #statusdf$ready[1]
    if((!exists("statusdf") == TRUE)){

      try((statusdf <- as.data.frame(driver$client$getStatus())),silent = TRUE)
      if((!exists("statusdf") == TRUE)){
        try(.GlobalEnv$driver <- rsDriver(browser=  c("chrome") #paste0(browserpreference)
                                          ,version = "latest"
                                          ,chromever= chrome.version4
                                          # ,geckover = NULL
                                          # ,iedriver = NULL
                                          # ,phantomver = NULL
                                          ,port= free_port()
                                          ,extraCapabilities=eCaps
                                          ,verbose = FALSE), silent = TRUE)
      }
      try(.GlobalEnv$driver <- rsDriver(browser=  c("chrome") #paste0(browserpreference)
                                        ,version = "latest"
                                        ,chromever= chrome.version3
                                        # ,geckover = NULL
                                        # ,iedriver = NULL
                                        # ,phantomver = NULL
                                        ,port= free_port()
                                        ,check = TRUE
                                        ,extraCapabilities=eCaps
                                        , verbose = FALSE) , silent = TRUE)
    }

    try((statusdf <- as.data.frame(driver$client$getStatus())),silent = TRUE)
    if((!exists("statusdf") == TRUE)){
      try(.GlobalEnv$driver <- rsDriver(browser=  c("chrome") #paste0(browserpreference)
                                        ,version = "latest"
                                        ,chromever= chrome.version2
                                        # ,geckover = NULL
                                        # ,iedriver = NULL
                                        # ,phantomver = NULL
                                        ,port= free_port()
                                        ,extraCapabilities=eCaps
                                        , verbose = FALSE),silent = TRUE)
    }

    try((statusdf <- as.data.frame(driver$client$getStatus())),silent = TRUE)
    if((!exists("statusdf") == TRUE)){
      try(.GlobalEnv$driver <- rsDriver(browser=  c("chrome") #paste0(browserpreference)
                                        ,version = "latest"
                                        ,chromever= chrome.version
                                        # ,geckover = NULL
                                        # ,iedriver = NULL
                                        # ,phantomver = NULL
                                        ,port= free_port()
                                        ,extraCapabilities=eCaps
                                        ,verbose = FALSE), silent = TRUE)
    }

    Sys.sleep(2)

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
