is.false = function(x) {
  if (!is.logical(x)) return(isTRUE(x))
  !x
}

has.col = function(df, col) {
  col %in% names(df)
}

replace.na = function(x, replace.value = 0) {
  x[is.na(x)] = replace.value
  x
}


copy.non.null.fields = function(dest, source, fields=names(source)) {
  restore.point("copy.into.empty.fields")
  copy.fields = fields[!sapply(source[fields], is.null)]

  if (is.environment(dest)) {
    for (field in copy.fields) dest[[field]] = source[[field]]
  } else {
    dest[copy.fields] = source[copy.fields]
  }

  invisible(dest)
}



view.ui = function(ui, launch.browser=rstudio::viewer) {
  restore.point("view.ui")

  library(shinyEvents)

  app = eventsApp()
  app$ui = fluidPage(ui)
  runEventsApp(app,launch.browser = launch.browser)
  return(invisible())
}



filter_.NULL = function(...) return(NULL)


replace.na = function(df, replace=0, cols=1:NCOL(df)) {
  ind = which(is.na(df[,cols]), arr.ind=TRUE)
  df[,cols][ind] = replace
  df
}

copy.intersect = function(dest, source, keep.dest.types = FALSE) {
  names = intersect(names(dest),names(source))
  if (keep.dest.types) {
    temp = lapply(names, function(name) as(source[name], class(dest[name])))
    dest[names] = temp
  } else {
    dest[names] = source[names]
  }
  dest
}

copy.into.missing.fields = function(dest, source) {
  restore.point("copy.into.empty.fields")

  new.fields = setdiff(names(source), names(dest))
  dest[new.fields] = source[new.fields]
  dest
}

examples.first.none.null = function() {
  get.none.null(NULL, 5,4)
}

first.none.null = function(...) {
  args = list(...)
  for (val in args) {
    if (!is.null(val)) return(val)
  }
  return(NULL)

}


colored.html = function(txt, color="blue") {
  if (is.null(color)) return(txt)
  paste0("<font color='",color,"'>",txt,"</font>")
}

stop.without.error <-function(...){
  opt <- options(show.error.messages=FALSE)
  on.exit(options(opt))
  display(...)
  stop()
}

# Knits the text in a temporary directory
knit.text = function(text, envir=parent.frame(), fragment.only=TRUE, quiet=TRUE) {
  owd <- setwd(tempdir())
  on.exit(setwd(owd))

  #knitr::opts_knit$set(root.dir = owd)

  html = knitr::knit2html(text=text, quiet=TRUE,envir=envir, fragment.only=fragment.only)
  html
}
