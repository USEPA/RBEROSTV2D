## Automatic upload to NEOS source script

my_savematch <- function(x, set, include = TRUE) {
  if(include == TRUE) {
    tmp <- x[which(x %in% set)]
  } else if(include == FALSE) {
    tmp <- x[which(!(x %in% set))]
  }
  
  return(tmp)
}

options(RCurlOptions = list(ssl.verifypeer = FALSE))

my_NgetSolverTemplate <- function (
    category, solvername, inputMethod, nc = CreateNeosComm()
) {
  if (!(class(nc) == "NeosComm")) {
    stop("\nObject provided for 'nc' must be of class 'NeosComm'.\n")
  }
  
  call <- match.call()
  
  ans <- xml.rpc(
    url = nc@url, 
    method = "getSolverTemplate", 
    .args = list(
      category = category, solvername = solvername, inputMethod = inputMethod
    ), 
    .convert = TRUE, 
    .opts = nc@curlopts, 
    .curl = nc@curlhandle
  )
  
  email.xmlcode <- "\n\n<email><![CDATA[...Insert Value Here...]]></email>"
  
  ans_rev <- if(
    grepl("email", ans)
  ) {
    ans
  } else {
    paste0(
      substr(ans, 1, nchar(ans)-13), 
      email.xmlcode, 
      substr(ans, (nchar(ans)-13), nchar(ans))
    )
  }
  
  xml <- xmlRoot(xmlTreeParse(ans_rev, asText = TRUE))
  
  res <- new("NeosXml", xml = xml, method = "getSolverTemplate",
             call = call, nc = nc)
  
  return(res)
}