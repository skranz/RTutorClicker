target.app.token.link.ui = function(app.url, token,url = get.url.with.login.token(app.url, token),new.tab=TRUE,target=if (new.tab) "_blank" else "") {

  tagList(
    HTML(paste0("<a href='",url,"'>Click here if the app does not open automatically</a>"))
  )
}


open.app.with.login.token = function(app.url, token, app=getApp(), new.tab=TRUE,target=if (new.tab) "_blank" else "", url = get.login.token.url(app.url, token)) {
  restore.point("opem.app.with.login.token")

  callJS(.fun = "window.open",list(url,target))
}


get.login.token.url = function(app.url, token, app=getApp()) {
  url = app.url
  if (!is.null(token[["token_key"]])) {
    url = paste0(app.url,"?token_key=",token$token_key,"&token_file=",token$file)
  } else {
    url = paste0(app.url,"?token_file=",token$file)
  }
  url
}

check.login.token = function(query=parseQueryString(session$clientData$url_search), token.dir=getwd(), session=getApp()$session) {
  restore.point("check.login.token")

  # does token exist
  file = file.path(token.dir, query$token)
  if (!file.exists(file)) return(NA)

  # is token still valid
  expire = as.numeric(str.right.of(query$token,"-"))
  if (expire < as.integer(Sys.time())) return(NA)

  tok = read.login.token(file = file)
  # check if token_key is valid
  if (!is.null(tok[["token_key"]])) {
    if (tok$token_key != query$token_key) return(NA)
  }

  return(tok)
}

save.login.token = function(token=make.login.token(..., valid.min=valid.min), token.dir=getwd(),valid.min=120,...) {
  restore.point("save.login.token")

  writeLines(token$txt,file.path(token.dir,token$file))
  return(token)
}

make.login.token = function(..., token_key=random.string(1,200), valid.min = 120, file.random=random.string(1,30), permament= valid.min==Inf) {
  args = list(token_key=token_key,...)
  restore.point("make.login.token")
  if (permament | valid.min == Inf) {
    args$expire = as.integer(Sys.time()) + round(valid.min*60)
  } else {
    args$expire = "Inf"
  }

  txt = paste0(names(args),": ",args, collapse="\n")
  file = paste0(args$expire,"-",file.random)
  list(file=file, txt=txt, token_key=token_key)
}

read.login.token = function(file, dir=NULL, txt=NULL) {
  if (is.null(txt)) {
    if (is.null(dir)) {
      long.file = file
    } else {
      long.file = file.path(dir, file)
    }
    txt = readLines(long.file, warn = FALSE)
  } else {
    txt = sep.lines(txt)
  }
  names = str.left.of(txt,": ")
  vals = as.list(str.right.of(txt,": "))
  names(vals) =names
  vals
}

remove.expired.login.tokens = function(token.dir) {
  files = list.files(token.dir, full.names=FALSE)
  if (length(files)==0) return()
  expire = as.numeric(str.right.of(files,"-"))
  now = as.integer(Sys.time())

  del = expire < now
  for (file in files[del]) {
    try(file.remove(file.path(token.dir,file)))
  }
}

clicker.default.token = function() {
  list(courseid="default", userid="Guest", created=Sys.time(), valid=Inf)
}
