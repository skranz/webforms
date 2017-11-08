# A list of multiple forms: to input a data frame
examples.listform.ui = function() {
  setwd("D:/libraries/stuko/stuko/inst/yaml")

  li = read.yaml("module.yaml",keep.quotes = FALSE)
  fields = li$fields

  sets = read.yaml("sets.yaml",keep.quotes = FALSE)$lang_de

  inner.form = li
  inner.form$lang = "de"
  inner.form$sets = sets
  inner.ui.fun = function(data, ns) {
    prefix=ns("")
    tagList(
      form.ui.simple(form=inner.form,values = data, prefix=prefix),
      p("hi")
    )
  }


  data = list(
    list(),
    list()
  )


  app = eventsApp()

  ui = listform.default.middle.ui(inner.ui.fun=inner.ui.fun)

  #form = listform(id="staff_form", fields=fields,lang="de", sets = sets)

  app$ui = fluidPage(
    selectizeHeaders(),
    tags$head(tags$style(HTML(listform.default.css()))),
    htmlDependency("font-awesome","4.7.0", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css"),
    tags$style(HTML(
      ".vector-input-container, .vector-input-container .form-control {margin-bottom: 0px;}")),
    div(style="margin-bottom: 5px; margin-top:5px;",ui),
    simpleButton("addCourseStaffBtn","Modul hinzufügen")
  )

  viewApp(app)

}



listform.default.middle.ui = function(inner.ui.fun, row.data = NULL, rowid = random.string(), use.delete.btn=TRUE, use.move.btn=TRUE,title="",ns=NS("listform"), extra.class=NULL,btn.size=c("xs", "sm","default")[1], ...) {
  restore.point("listform.default.middle.ui")

  row.ns = ns("rowid")

  inner.ui = inner.ui.fun(row.data, row.ns)


  div(id=rowid,
    if (use.move.btn)
      HTML(listform.move.btn(rowid,"up", ns, size=btn.size)),
    if (use.move.btn)
      HTML(listform.move.btn(rowid,"down", ns, size=btn.size)),
    if (use.delete.btn)
      HTML(listform.delete.btn(rowid,ns, size=btn.size)),
    div(class=paste0("listform-middle-ui ", extra.class),
      inner.ui
    )
  )

}


listform = function(id,fields, prefix = paste0("listform-", id), lang="en", sets=NULL) {
  nlist(
    id,
    fields,
    prefix,
    lang,
    ns = NS(prefix),
    sets
  )
}

extract.listform.formValues = function(formValues, form) {
  restore.point("extract.listform.formValues")
  prefix = paste0(form$prefix,"-")
  rows = str.starts.with(names(formValues), prefix)
  vals = unlist(formValues[rows])
  names = str.right.of(names(vals),prefix)

  vfields = str.left.of(names,"_-_")
  rowids = str.right.of(names,"_-_")

  nc = length(unique(vfields))
  nr = length(unique(rowids))

  # We hope that the css selector always fetches
  # the inputs rowwise
  df = as_data_frame(matrix(vals,nrow = nr, byrow=TRUE))
  colnames(df) = unique(vfields)
  df

}


listform.delete.btn.handler = function(table.id=form$id, ns=form$ns, form=NULL, pre.delete.fun=NULL,...) {
  restore.point("listform.delete.btn.handler")

  classEventHandler(class = ns("delBtn"), event="click", function(data,...) {
    args = list(...)
    restore.point("delBtn")

    if (!is.null(pre.delete.fun)) {
      res = pre.delete.fun(data, ...)
      if (!res$ok) return()
    }

    cat("\ndel pressed...")
    sel = paste0("#",table.id," tr.tr-row-",data$rowid)
    evalJS(paste0('$("', sel,'").remove();'))
  })

}

listform.move.btn.handler = function(table.id=form$id, ns=form$ns, form=NULL, after.move.fun=NULL,...) {
  restore.point("listform.delete.btn.handler")

  classEventHandler(class = ns("moveBtn"), event="click", function(data,...) {
    args = list(...)
    restore.point("delBtn")
    dir = data$dir

    sel = paste0("#",table.id," tr.tr-row-",data$rowid)
    if (dir=="up") {
      code=paste0('$("',sel,'").prev().before($("',sel,'"));')
    } else {
      code=paste0('$("',sel,'").next().after($("',sel,'"));')
    }

    cat("\nmove pressed...")
    evalJS(code)

    if (!is.null(after.move.fun)) {
      after.move.fun(data, ...)
    }

  })

}


listform.delete.btn = function(rowid, ns, label="", icon=shiny::icon("trash-o"), size="sm") {
  restore.point("listform.delete.btn.vector")

  simpleButtonVector(
    ns(paste0("delBtn_-_",rowid)),
    label,
    icon=icon,
    size=size,
    extra.class = ns("delBtn"),
    extra.head = paste0('data-rowid="', rowid,'"')
  )
}

listform.move.btn = function(rowid, dir="up",ns, label="", icon=if(dir=="up") shiny::icon("arrow-up") else shiny::icon("arrow-down"), size="sm") {
  restore.point("listform.move.btn")

  btn = simpleButtonVector(
    ns(paste0(dir,"Btn_-_",rowid)),
    label,
    icon=icon,
    size=size,
    extra.class = ns(paste0("moveBtn")),
    extra.head = paste0('data-rowid="', rowid,'" data-dir="',dir,'"')
  )
}


listform.default.css = function() {
  css = paste0('
div.listform-middle-ui {
  padding: 4px;
  border: 1px solid #cccccc;
}

')
}

