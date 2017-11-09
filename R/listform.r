# A list of multiple forms: to input a data frame
examples.listform.ui = function() {
  setwd("D:/libraries/stuko/stuko/inst/yaml")

  li = read.yaml("test.yaml",keep.quotes = FALSE)
  fields = li$fields

  sets = read.yaml("sets.yaml",keep.quotes = FALSE)$lang_de

  inner.form = li
  inner.form$lang = "de"
  inner.form$sets = sets
  inner.ui.fun = function(data, ns) {
    restore.point("inner.ui.fun")
    prefix=ns("")
    tagList(
      form.ui.simple(form=inner.form,values = data, prefix=prefix,add.submit = FALSE)
    )
  }


  data = list(
    list(),
    list()
  )


  app = eventsApp()

  id = "modules"
  ui = listform.ui(id=id,inner.ui.fun=inner.ui.fun, num.rows = 2)

  app$ui = fluidPage(
    selectizeHeaders(),
    tags$head(tags$style(HTML(listform.default.css()))),
    htmlDependency("font-awesome","4.7.0", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css"),
    tags$style(HTML(
      ".vector-input-container, .vector-input-container .form-control {margin-bottom: 0px;}")),
    div(style="margin-bottom: 5px; margin-top:5px;",ui),
    simpleButton("addModuleBtn","Zusätzliches Modul hinzufügen"),
    simpleButton("saveBtn", "Speichern", form.sel = listform.input.selector("modules"))
  )

  buttonHandler("addModuleBtn", function(...) {
    restore.point("addModuleBtn")
    listform.add.row(id=id,inner.ui.fun = inner.ui.fun)
  })

  buttonHandler("saveBtn", function(formValues, ...) {
    restore.point("listformSaveBtn")
    values = extract.listform.formValues(id=id, formValues=formValues)
    cat("\nsave...")
  })

  viewApp(app)

}

extract.listform.formValues = function(id, formValues,..., rowid.as.name=TRUE) {
  restore.point("extract.listform.formValues")
  formValues = formValues[names(formValues)!="undefined"]

  names = names(formValues)
  names = str.right.of(names,paste0(id,"-"))
  rowid = str.left.of(names,"-")
  field.name = str.right.of(names,"-")

  li = lapply(unique(rowid), function(rid) {
    rows = rowid == rid
    res = formValues[rows]
    names(res) = field.name[rows]
    res
  })
  if (rowid.as.name)
    names(li) = unique(rowid)
  li


}

listform.input.selector = function(id) {
  restore.point("listform.input.selector")
  paste0("#",id," .middle-ui ", c("select", "input"), collapse=",")
}

listform.handlers = function(id=first.non.null(form[["id"]],listform),ns=first.non.null(form$ns, NS(id)), use.delete.btn=TRUE, use.move.btn=TRUE, pre.delete.fun=NULL, after.move.fun=NULL, form=NULL,...) {

  if (use.delete.btn)
    listform.delete.btn.handler(id=id, ns=ns, pre.delete.fun=pre.delete.fun)

  if (use.move.btn)
    listform.move.btn.handler(id=id, ns=ns, after.move.fun=after.move.fun)

}

listform.ui = function(id=first.non.null(form[["id"]],listform),inner.ui.fun, data=NULL, num.rows=max(NROW(data),1), middle.ui.fun = listform.default.middle.ui, form=NULL, ns=first.non.null(form$ns, NS(id)), add.handlers=TRUE,...) {
  restore.point("listform.ui")


  ui.li = lapply(seq_len(num.rows), function(i) {
    row.data = if (i<=NROW(data)) data[i,]
    middle.ui.fun(inner.ui.fun=inner.ui.fun, row.data=row.data, ns=ns, form=form,...)
  })


  if (add.handlers) {
    listform.handlers(id=id, ns=ns,...)
  }

  ui = div(id=id,
    ui.li
  )
  ui
}


listform.default.middle.ui = function(inner.ui.fun, row.data = NULL, rowid = random.string(), use.delete.btn=TRUE, use.move.btn=TRUE,title="",ns=NS("listform"), extra.class=NULL,btn.size=c("xs", "sm","default")[1], id=first.non.null(form$id,"listform"), form=NULL,  ...) {
  restore.point("listform.default.middle.ui")

  row.ns = NS(ns(rowid))

  inner.ui = inner.ui.fun(row.data, ns=row.ns)


  div(id=rowid, class=paste0("middle-ui middle-ui-",rowid," ", extra.class),
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


listform.delete.btn.handler = function(id=form$id, ns=form$ns, form=NULL, pre.delete.fun=NULL,...) {
  restore.point("listform.delete.btn.handler")

  classEventHandler(class = ns("delBtn"), event="click", function(data,...) {
    args = list(...)
    restore.point("delBtn")

    if (!is.null(pre.delete.fun)) {
      res = pre.delete.fun(data, ...)
      if (!res$ok) return()
    }

    cat("\ndel pressed...")
    sel = paste0("#",id," div.middle-ui-",data$rowid)
    evalJS(paste0('$("', sel,'").remove();'))
  })

}

listform.move.btn.handler = function(id=form$id, ns=form$ns, form=NULL, after.move.fun=NULL,...) {
  restore.point("listform.delete.btn.handler")

  classEventHandler(class = ns("moveBtn"), event="click", function(data,...) {
    args = list(...)
    restore.point("moveBtn")
    dir = data$dir

    sel = paste0("#",id," div.middle-ui-",data$rowid)
    if (dir=="up") {
      code=paste0('$("',sel,'").prev("div.middle-ui").before($("',sel,'"));')
    } else {
      code=paste0('$("',sel,'").next("div.middle-ui").after($("',sel,'"));')
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


listform.add.row = function(id=form$id,row.data=NULL, inner.ui.fun, middle.ui.fun=listform.default.middle.ui,form=NULL, ...) {
  restore.point("listform.add.row")
  cat("\nadd.row")

  html = as.character(middle.ui.fun(id=id, inner.ui.fun=inner.ui.fun, row.data=data,...))

  restore.point("listform.add.row2")

  fun = paste0('$("#', id,'").append')
  callJS(fun, html)
  evalJS(paste0('$("#', id,' select").selectize({dropdownParent: "body"});'))

}


listform.default.css = function() {
  css = paste0('
div.listform-middle-ui {
  padding: 4px;
  border: 1px solid #cccccc;
}

')
}

