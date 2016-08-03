examples.mem.store = function() {
  setwd("D:/libraries/webforms/")
  con = "test.json"
  li = list(a=6,b=list(x="hi",y=3, z=data_frame(1:4,2:5)), fun=function(x) x*5)

  con = file("test1.ser", open="at")
  bi = serialize(li, connection=textConnection("tbi", open="w"), ascii=TRUE)
  close(con)
  con = file("test1.ser", open="rt")
  unserialize(con)

  ch = as.character(bi)
  raw = as.raw(ch)
  uli = unserialize(raw)
  ch = paste0(bi,collapse="")
  bi =
  ch = rawToChar(bi)

  append.ndjson(li,con)
  li = read.ndjson("test.json", as.data.frame = FALSE, num.lines=-1)
  page = readLines("test.json")
  df = stream_in(file("test.json",open = "r"))
  fromJSON(txt)
}

read.ndjson = function(con, as.data.frame=TRUE, txt=NULL,encoding = "UTF-8", num.lines = NULL, ..., unlist.cols = TRUE, list.cols=NULL) {
  restore.point("read.ndjson")

  if (is.null(txt))
    txt <- readLines(con, encoding=encoding)
  if (as.data.frame) {
    li = lapply(txt, jsonlite:::parseJSON)
    df = unlist.cols(jsonlite:::simplify(li, ,...), ignore=list.cols)
    return(df)
  }
  if (length(txt)==0) return(NULL)
  if (!is.null(num.lines)) {
    if (num.lines > 0) {
      txt = txt[1:num.lines]
    } else if (num.lines <0) {
      txt = txt[max(1,length(txt)+num.lines+1):length(txt)]
    }
  }

  li = lapply(txt, jsonlite:::fromJSON,...)
  li
}

write.ndjson = function(x,file) {
  write(toJSON(x),file)
}

append.ndjson = function(x,file) {
  if (is.character(file))
    file = file(file,open = "at")
  write(toJSON(x),file)
  close(file)
}

store.form.values = function(li,file,user="", time=Sys.time()) {
  x = c(list(.user=user, .time=time),li)
  append.ndjson(x,file)
}

unlist.cols = function(df, ignore=NULL, cols=colnames(df)) {
  cols = setdiff(cols, ignore)
  n = NROW(df)
  ul = lapply(df[cols], function(vals) {
    uv = unlist(vals)
    if (length(uv)>n) return(vals)
    uv
  })
  df[cols] = ul
  df
}

