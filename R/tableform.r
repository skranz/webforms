examples.tableform.ui = function() {

  setwd("D:/libraries/stuko/")
  mo = readRDS("mo_fast_edit.Rds")
  mo = mo[1:5,]

  setwd("D:/libraries/stuko/stuko/inst/yaml")

  li = read.yaml("modul_table_edit.yaml",keep.quotes = FALSE)
  fields = li$fields

  sets = read.yaml("sets.yaml",keep.quotes = FALSE)

  data = select(mo, titel, extern, zuordnung, studiengang)

  app = eventsApp()

  form = tableform(id="module_table", fields=fields[colnames(data)],lang="de", sets = sets, data=data, use.checkbox = TRUE)
  ui = tableform.ui(form=form, data=data, auto.filter = TRUE)

  tableformFilterHandler("module_table", form=form, function(value,filter, fdata,...) {
    restore.point("filter.change")
    tableform.update.body(form=form, data=fdata)
    cat("\nfilter changed", sample.int(1000,1))
  })
  app$ui = fluidPage(
    selectizeHeaders(),
    htmlDependency("font-awesome","4.7.0", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css"),
    tags$head(tags$style(form.table.default.css())),
    tags$style(HTML(
      ".vector-input-container, .vector-input-container .form-control {margin-bottom: 0px;}
      .field_filter .selectize-input, input.field_filter {border: 1px solid #000000;}
      ")),
    div(style="margin-bottom: 5px; margin-top:5px;",ui),
    hr(),
    simpleButton("btnSave","Speichern",form.sel = paste0(".",form$ns("input")))
  )

  buttonHandler("btnSave", function(formValues,...) {
    args = list(...)
    restore.point("btnSave")
    df = extract.tableform.formValues(formValues, form, convert.types = TRUE)
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


tableform = function(id,fields, prefix = paste0("tableform-", id), lang="en", sets=NULL, data=NULL, use.move.btn=FALSE, use.delete.btn=FALSE, use.checkbox=FALSE) {
  nlist(
    id,
    fields,
    prefix,
    lang,
    ns = NS(prefix),
    sets,
    use.move.btn,
    use.delete.btn,
    use.checkbox,
    data = data
  )
}



field.filter.input = function(id, field, sets, lang=NULL, prefix="filter__") {
  restore.point("field.filter.input")
  choices = get.field.choices(field, sets, lang)

  inputId = paste0(prefix,id)
  if (is.null(choices)) {
    textInputVector(inputId,label = NULL,value="", size=field$size,extra.class = "field_filter")
  } else {
    #selectInput(inputId, "", choices, multiple = TRUE, width = field$width, size=field$size)
    multiSelectizeInputVector(inputId,value=list(character(0)),choices = choices,width = field$width, size=field$size,extra.class = "field_filter")
  }
}

tableformFilterHandler = function(tableid=form$id, fun, form) {
  restore.point("tableformFilterHandler")
  sel = paste0("#", tableid, " .field_filter")

  eventId = "tableFormFilterChange"
  js = paste0('
  $("body").on("change","select.field_filter, input.field_filter", function (e) {
    var tab = $(e.target).parents(".r-webform-table");
    var filters = tab.find(".field_filter");
    var vals = shinyEventsWidgetsValues(filters);
    Shiny.onInputChange("',eventId,'", {eventId: "',eventId,'", id: tab.attr("id"), filterId: e.target.id, value: vals, nonce: Math.random()});
  });
  ')

  eventHandler(eventId = eventId,id = tableid, fun = function(...) tableform.filter.process(tableid=tableid, fun=fun, form=form,...), jscript = js)

}

tableform.filter.process = function(tableid=form$tableid, fun,  value, form, data=form$data, ...) {
  restore.point("tableform.filter.process")
  fields = form$fields
  left = paste0(tableid, "_filter__")
  value = value[str.starts.with(names(value), left)]
  names(value) = str.right.of(names(value), left)

  value = lapply(value, unlist)

  use = sapply(value, function(val) {
    if (is.null(val)) return(FALSE)
    if (is.character(val)) return(any(nchar(val))>0)
    TRUE
  })

  filter = value[use]
  fields = fields[names(filter)]

  input = unlist(lapply(fields,function(field) determine.field.input.type(field=field)))

  is.text.filter = !input %in% c("select","selectize","radio")

  fd = form$data
  # Text filter
  for (col in names(filter[is.text.filter])) {
    fd = apply.text.filter(fd,col=col, filter[[col]])
  }
  # Choice filter
  for (col in names(filter[!is.text.filter])) {
    fd = apply.choices.filter(fd,col=col, unlist(filter[[col]]))
  }

  fun(value=value, filter=filter, tableid=tableid, form=form, fdata=fd, data=data)

}

apply.text.filter = function(data, col, text) {
  restore.point("apply.text.filter")
  rows = has.substr(tolower(as.character(data[[col]])), tolower(text))
  data[rows,]
}

apply.choices.filter = function(data, col, choices) {
  restore.point("apply.choices.filter")
  vals = data[[col]]
  multi = is.list(vals)
  if (NROW(vals)==0) return(vals)

  neg.choices = str.right.of(choices,"No-_-",not.found = NA)
  neg.choices = neg.choices[!is.na(neg.choices)]
  pos.choices = setdiff(choices, neg.choices)

  rows = rep(TRUE, NROW(data))
  if (!multi) {
    vals = as.character(vals)
    if (length(pos.choices)>0)
      rows = rows & vals %in% pos.choices

    if (length(neg.choices)>0)
      rows = rows & (!vals %in% neg.choices)
  } else {
    stop("filter for multiple selection not yet implemented")
  }
  data[rows,]
}

extract.tableform.formValues = function(formValues, form, convert.types = FALSE) {
  restore.point("extract.tableform.formValues")

  multiple.cols = which(sapply(form$fields, function(field) isTRUE(field$multiple)))

  prefix = paste0(form$prefix,"-")
  rows = str.starts.with(names(formValues), prefix)
  if (sum(rows)==0) return(NULL)
  #vals = unlist(formValues[rows])
  vals = formValues[rows]
  if (length(multiple.cols)==0) {
    vals = unlist(vals)
  } else {
    is.li = sapply(vals, is.list)
    vals[is.li] = lapply(vals[is.li], unlist)
  }

  names = str.right.of(names(vals),prefix)

  vfields = str.left.of(names,"_-_")
  rowids = str.right.of(names,"_-_")

  nc = length(unique(vfields))
  nr = length(unique(rowids))

  # We hope that the css selector always fetches
  # the inputs rowwise
  df = as_data_frame(matrix(vals,nrow = nr, byrow=TRUE))
  colnames(df) = unique(vfields)
  if (length(multiple.cols)>0) {
    simple.cols = setdiff(1:nc, multiple.cols)
    df[simple.cols] = lapply(simple.cols, function(col) unlist(df[[col]]))
  }
  if (convert.types) {
    df[simple.cols] = lapply(simple.cols, function(col) {
      restore.point("skjhfhfj")
      convert.field.values(df[[col]], field=form$fields[[col]],sets=form$sets)
    })

    df[multiple.cols] = lapply(multiple.cols, function(col) {
      class = determine.field.r.class(form$fields[[col]], form$sets)
      if (class=="character") return(df[[col]])
      lapply(df[[col]], as, Class=class)
    })
  }

  df

}


tableform.ui = function(table.id=first.non.null(form$id,random.string()),fields=form$fields,data=NULL, n=NROW(data), rowids = random.string(n,nchar=10), colnames=tableform.colnames(fields,data, lang), sets=form$sets,lang=first.non.null(form$lang, "en"), buttons.col=Inf, add.btn.label = labels$add.btn, delete.btn.label = "", delete.btn.icon=icon("trash-o"), use.move.btn=first.none.null(form$use.move.btn,TRUE), use.add.btn=first.none.null(form$use.add.btn,TRUE), use.delete.btn=first.none.null(form$use.delete.btn,TRUE), use.checkbox=first.none.null(form$use.checkbox,FALSE), labels=tableform.default.labels(lang), prefix=form$prefix, form=NULL, just.tr=FALSE, add.handlers=!just.tr, button.col=1, auto.filter=FALSE, ...) {
  restore.point("tableform.ui")

  ns = NS(prefix)

  field.names = names(fields)
  df = data
  col = field.names[1]
  for (col in field.names) {
    df[[col]] = fieldInputVector(name=col,fields = fields, value=data[[col]], sets=sets, label=NULL, lang=lang, rowids=rowids,ns=ns, extra.class=ns("input"))
  }

  btns = NULL

  if (use.delete.btn) {
    btns = tableform.delete.btn.vector(id=id,n=n,ns=ns,label=delete.btn.label, icon = delete.btn.icon, rowids=rowids)
  }
  if (use.checkbox) {
    btns = paste0(tableform.checkbox.vector(id=id,n=n,ns=ns, rowids=rowids), btns)
  }

  if (use.move.btn) {
    btns = paste0(
      tableform.move.btn.vector(id=id,dir="up",n=n,ns=ns, rowids=rowids),
      tableform.move.btn.vector(id=id,dir="down",n=n,ns=ns, rowids=rowids),
      btns
    )
  }

  if (!is.null(btns)) {
    if (NROW(df)>0) {
      df$.btns = btns
    } else {
      df$.btns = character(0)
    }
    df = select(df, .btns, everything())

    if (length(colnames)<NCOL(df)) {
      colnames=c(" ", colnames)
    }
  }

  if (auto.filter) {
    restore.point("make.auto.filters")
    filters = lapply(field.names, function(fn) {
      field.filter.input(id=fn,fields[[fn]], sets=sets, lang=lang, prefix=paste0(table.id,"_filter__"))
    })
    names(filters) = field.names
    if (!is.null(btns)) {
      filters = c(list(.btns=""), filters)
    }
    df = rbind(as_data_frame(filters), df)

  }

  html = form.html.table(id=table.id,df,col.names=colnames, just.tr = just.tr, rowids=rowids, has.filter=auto.filter)


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

tableform.update.body = function(table.id=form$id, data=first.non.null(form$fdata,form$data), form, html=NULL) {

  if (is.null(html)) {
    html = tableform.ui(table.id=table.id, data=data, form=form, ..., just.tr=TRUE)
  }
  restore.point("tableform.update.body")

  fun = paste0('$("#', table.id,' tbody").html')
  callJS(fun, HTML(html))
  evalJS(paste0('$("#', table.id,' tbody select").selectize({dropdownParent: "body"});'))


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
    code = paste0('$("#', table.id," tr.tr-row-",data$rowid,'").remove();')
    evalJS(code)
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
      code=paste0('$("',sel,'").prev(".data-row").before($("',sel,'"));')
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


tableform.delete.btn.vector = function(id, n, ns, label="", icon=icon("o-trash"), rowids = seq_len(n)) {
  restore.point("tableform.delete.btn.vector")

  del.btn = simpleButtonVector(
    ns(paste0("delBtn_-_",rowids)),
    label,
    icon=icon,
    extra.class = ns("delBtn"),
    extra.head = paste0('data-rowid="', rowids,'"')
  )
}

tableform.checkbox.vector = function(id, n, ns, rowids = seq_len(n)) {
  restore.point("tableform.checkbox.vector")

  checkBoxInputVector(
    ns(paste0("checkbox_-_",rowids)),
    value = FALSE,
    extra.class = ns("checkbox"),
    extra.head = paste0('data-rowid="', rowids,'"')
  )
}
tableform.move.btn.vector = function(id, dir="up", n, ns, label="", icon=if(dir=="up") shiny::icon("arrow-up") else shiny::icon("arrow-down"), rowids = seq_len(n)) {
  restore.point("tableform.move.btn.vector")

  btn = simpleButtonVector(
    ns(paste0(dir,"Btn_-_",rowids)),
    label,
    icon=icon,
    extra.class = ns(paste0("moveBtn")),
    extra.head = paste0('data-rowid="', rowids,'" data-dir="',dir,'"')
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
