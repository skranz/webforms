examples.tableform.ui = function() {
  setwd("D:/libraries/stuko/stuko/inst/yaml")

  li = read.yaml("coursestaff.yaml",keep.quotes = FALSE)
  fields = li$fields

  sets = read.yaml("sets.yaml",keep.quotes = FALSE)$lang_de

  selectizeInput("test", "test", 1:2, width="5em")
  data = data_frame(staffid=c("skranz","NN"), teaching_role=c("LE","TA"), responsible=FALSE, commission="")
  sets = c(sets, list(staff=list("Sebastian Kranz"="skranz","Max Mustermann"="mm","NN"="NN")))

  empty.staff = data.frame(staffid="", teaching_role="", responsible=FALSE, commission="no")

  app = eventsApp()

  form = tableform(id="staff_form", fields=fields,lang="de", sets = sets)
  ui = tableform.ui(form=form, data=data, use.delete.btn = TRUE)

  app$ui = fluidPage(
    selectizeHeaders(),
    htmlDependency("font-awesome","4.7.0", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css"),
    tags$head(tags$style(form.table.default.css())),
    tags$style(HTML(
      ".vector-input-container, .vector-input-container .form-control {margin-bottom: 0px;}")),
    div(style="margin-bottom: 5px; margin-top:5px;",ui),
    simpleButton("addCourseStaffBtn","Mitarbeiterzeile hinzufügen"),
    simpleButton("createStaffBtn","Neue Person in Datenbank erstellen."),
    hr(),
    simpleButton("btnSave","Speichern",form.sel = paste0(".",form$ns("input")))
  )

  buttonHandler("addCourseStaffBtn", function(...) {
    restore.point("addCourseStaffBtn")
    form.html.table.add.row(form=form, data=empty.staff)
  })


  buttonHandler("btnSave", function(formValues,...) {
    args = list(...)
    restore.point("btnSave")
    df = extract.tableform.formValues(formValues, form)
    cat("\nsave pressed...")
  })

  viewApp(app)

}


form.html.table.add.row = function(table.id=form$id,data=NULL, html=NULL,form=NULL,  ...) {
  restore.point("form.html.table.add.row")
  cat("\nadd.row")
  if (is.null(html)) {
    html = tableform.ui(table.id=table.id, data=data, form=form, ..., just.tr=TRUE)
  }
  restore.point("form.html.table.add.row2")

  fun = paste0('$("#', table.id,' tr:last").after')
  callJS(fun, html)
  evalJS(paste0('$("#', table.id,' select").selectize({dropdownParent: "body"});'))
}

selectizeHeaders = function() {
  htmlDependency("selectize", "0.11.2", c(href = "shared/selectize"),
        stylesheet = "css/selectize.bootstrap3.css", head = format(tagList(HTML("<!--[if lt IE 9]>"),
            tags$script(src = "shared/selectize/js/es5-shim.min.js"),
            HTML("<![endif]-->"), tags$script(src = "shared/selectize/js/selectize.min.js"))))

}


tableform = function(id,fields, prefix = paste0("tableform-", id), lang="en", sets=NULL) {
  nlist(
    id,
    fields,
    prefix,
    lang,
    ns = NS(prefix),
    sets
  )
}

extract.tableform.formValues = function(formValues, form) {
  restore.point("extract.tableform.formValues")
  prefix = paste0(form$prefix,"-")
  rows = str.starts.with(names(formValues), prefix)
  vals = unlist(formValues[rows])
  names = str.right.of(names(vals),prefix)

  vfields = str.left.of(names,"_-_")
  row.ids = str.right.of(names,"_-_")

  nc = length(unique(vfields))
  nr = length(unique(row.ids))

  # We hope that the css selector always fetches
  # the inputs rowwise
  df = as_data_frame(matrix(vals,nrow = nr, byrow=TRUE))
  colnames(df) = unique(vfields)
  df

}

tableform.ui = function(table.id=first.non.null(form$id,random.string()),fields=form$fields,data=NULL, n=NROW(data), row.ids = random.string(n,nchar=10), colnames=tableform.colnames(fields,data, lang), sets=form$sets,lang=first.non.null(form$lang, "en"), buttons.col=Inf, add.btn.label = labels$add.btn, delete.btn.label = "", delete.btn.icon=icon("trash-o"), use.move.btn=TRUE, use.add.btn=TRUE, use.delete.btn=TRUE, labels=tableform.default.labels(lang), prefix=form$prefix, form=NULL, just.tr=FALSE, add.handlers=!just.tr, ...) {
  restore.point("tableform.ui")

  ns = NS(prefix)

  field.names = names(fields)
  df = data
  col = field.names[1]
  for (col in field.names) {
    df[[col]] = fieldInputVector(name=col,fields = fields, value=data[[col]], sets=sets, label=NULL, lang=lang, row.ids=row.ids,ns=ns, extra.class=ns("input"))
  }

  btns = NULL
  if (use.delete.btn) {
    btns = tableform.delete.btn.vector(id=id,n=n,ns=ns,label=delete.btn.label, icon = delete.btn.icon, row.ids=row.ids)
  }

  if (!is.null(use.move.btn)) {
    btns = paste0(
      tableform.move.btn.vector(id=id,dir="up",n=n,ns=ns, row.ids=row.ids),
      tableform.move.btn.vector(id=id,dir="down",n=n,ns=ns, row.ids=row.ids),
      btns
    )
  }

  if (!is.null(btns)) {
    df$.btns = btns
    if (length(colnames)<NCOL(df)) {
      colnames=c(colnames," ")
    }
  }

  html = form.html.table(id=table.id,df,col.names=colnames, just.tr = just.tr, row.ids=row.ids)

  if (just.tr) return(html)

  ui = HTML(html)


  if (add.handlers) {
    if (use.delete.btn) {
      tableform.delete.btn.handler(table.id = table.id, ns=ns)
    }
    if (use.move.btn) {
      tableform.move.btn.handler(table.id = table.id, ns=ns)
    }
  }

  ui
}

tableform.delete.btn.handler = function(table.id=form$id, ns=form$ns, form=NULL, pre.delete.fun=NULL,...) {
  restore.point("tableform.delete.btn.handler")

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

tableform.move.btn.handler = function(table.id=form$id, ns=form$ns, form=NULL, after.move.fun=NULL,...) {
  restore.point("tableform.delete.btn.handler")

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


tableform.delete.btn.vector = function(id, n, ns, label="", icon=icon("o-trash"), row.ids = seq_len(n)) {
  restore.point("tableform.delete.btn.vector")

  del.btn = simpleButtonVector(
    ns(paste0("delBtn_-_",row.ids)),
    label,
    icon=icon,
    extra.class = ns("delBtn"),
    extra.head = paste0('data-rowid="', row.ids,'"')
  )
}

tableform.move.btn.vector = function(id, dir="up", n, ns, label="", icon=if(dir=="up") shiny::icon("arrow-up") else shiny::icon("arrow-down"), row.ids = seq_len(n)) {
  restore.point("tableform.move.btn.vector")

  btn = simpleButtonVector(
    ns(paste0(dir,"Btn_-_",row.ids)),
    label,
    icon=icon,
    extra.class = ns(paste0("moveBtn")),
    extra.head = paste0('data-rowid="', row.ids,'" data-dir="',dir,'"')
  )
}


tableform.colnames = function(fields, data, lang="en") {
  restore.point("tableform.colnames")
  names = colnames(data)
  field.names = names(fields)
  labels = sapply(fields, function(field) {
    field = get.lang.field(field, lang)
    field$label
  })
  ma = match(field.names, names)
  rows = !is.na(ma)
  names[ma[rows]] = labels[rows]
  names
}

tableform.default.labels = function(lang="en") {
  if (lang=="de") {
    list(
      add.btn = "Hinzufügen",
      delete.btn = "Löschen"
    )
  } else {
    list(
      add.btn = "Add",
      delete.btn = "Delete"
    )

  }
}