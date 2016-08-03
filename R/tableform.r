
examples.vecForm = function() {
  setwd("D:/libraries/SeminarMatching/")
  library(yaml)
  library(YamlObjects)


  restore.point.options(display.restore.point = TRUE)
  app = eventsApp()

  sets =read.yaml(file="sets.yaml")
  yaml = paste0(readLines("sempointsform.yaml"), collapse="\n")


  form = read.yaml(text=yaml,utf8 = TRUE)
  form$success.handler = function(...) {
    cat("\nGreat you inserted valid numbers!")
  }
  form$lang = "de"
  form$sets = sets

  set.form(form)
  data = data.frame(
    points = c(10,5),
    slots = c('3',''),
    subject = c('',''),
    spec = c('',''),
    email = c('','')
  )

  ui = form.ui.handsone.table(form=form, data=data)

  add.form.handlers(form)
  app$ui = fluidPage(ui)
  runEventsApp(app, launch.browser = rstudio::viewer)

}


form.ui.handsone.table = function(form, data, fields=form$fields, label=first.none.null(lang.form[["label"]],form[["label"]]), help_html=lang.form[["help_html"]],note_html=lang.form[["note_html"]],note_title=first.none.null(lang.form[["note_title"]],"Info"), sets = form[["sets"]],
  submitBtn=NULL, submitLabel="Submit",add.submit=TRUE,lang=form[["lang"]], addLabel="",addIcon=icon(name = icon("plus",lib = "glyphicon")), width=first.none.null(form$width,"100%"), height=first.none.null(form$height,"100%"), stretchH='all', lang.form = get.lang.form(form, lang), rowHeaders=NULL, readOnly=FALSE, ...) {
  restore.point("form.ui.handsone.table")

  library(rhandsontable)

  cols = names(fields)
  df = data[,cols,drop=FALSE]

  id = paste0(form$prefix,"handsoneTableFormUI", form$postfix)

  ui = rHandsontableOutput(outputId = id, width=width, height=height)

  labels = sapply(names(fields),function(name) {
    first.none.null(get.lang.field(fields[[name]],lang)$label, name)
  })
  names(labels) = NULL

  hot = rhandsontable(data=df, colHeaders=labels, rowHeaders=rowHeaders, useTypes = TRUE, readOnly = readOnly, selectCallback = FALSE,stretchH=stretchH)

  for (col in seq_along(fields)) {
    field = fields[[col]]
    input = field[["input"]]
    choices = field[["choices"]]
    choice_set = field[["choice_set"]]

    if (is.null(input)) {
      if (is.null(choices) & is.null(choice_set)) {
        input = "text"
      } else {
        input = "selectize"
      }
    }
    type = NULL
    if (input == "selectize") {
      if (!is.null(choice_set)) {
        for (set in sets[choice_set])
          choices = c(choices, set)
      }
      choices = unlist(choices)
      hot = hot_col(hot,col=col,type="dropdown", source=choices, readOnly=isTRUE(field$readonly))
    } else {
      hot = hot_col(hot,col=col,type="text", strict=FALSE, readOnly=isTRUE(field$readonly))
    }
  }

  setRHandsontable(id, hot)

  alert_id = paste0(id,"__Alert")
  ui = list(ui,uiOutput(alert_id))
  if (!is.null(label)) {
    ui = c(list(h4(label)),ui)
  }
  if (!is.null(help_html)) {
    ui = c(ui,list(br(),HTML(help_html)))
  }
  if (!is.null(note_html)) {
    ui =c(ui, list(br(),bsCollapse(bsCollapsePanel(title=note_title,HTML(note_html)))))
  }
  ui
}

#' Get the data frame from a table form
#' @param form the form on which the table is based
#' @param null.value rhandsonetable does not return a value if nothing is changed, but only returns null. null.value can be se to the initial data.frame which will then be returned in case nothing has been changed.
get.table.form.df = function(form, null.value = NULL) {
  id = paste0(form$prefix,"handsoneTableFormUI", form$postfix)
  hot = getInputValue(id)
  restore.point("get.table.form.df")
  if (is.null(hot)) {
    cat("\n*********************************")
    cat("\nhandsoneTableFormUI == NULL")
    cat("\n*********************************")
    return(null.value)
  }
  df = as_data_frame(data.table::rbindlist(hot$data))
  #df = hot_to_r(hot)
  colnames(df) = names(form$fields)
  df
}

table.form.default.values = function(form, data = NULL, nrow=max(NROW(data),1), sets=NULL, boolean.correction=TRUE) {
  restore.point("table.form.default.values")

  df = data
  vals = lapply(form$fields, function(field) {
    rep(field.default.values(form=form, field=field,sets=sets),nrow)
  })
  replace = intersect(names(vals), names(df))

  if (length(replace)>0) {
    if (boolean.correction)
      boolean = sapply(vals[replace], function(val) any(is.logical(val) & !is.na(val)))
    vals[replace] = df[replace]
    if (boolean.correction)
      vals[replace][boolean] =  lapply(vals[replace][boolean], as.logical)
  }
  as.data.frame(vals, stringsAsFactors = FALSE)
}

check.field.values = function(values, field) {
  restore.point("check.field.values")

  if (is.null(values))
    return(list(ok=TRUE,msg="", values=values, err.rows=NULL))

  if (isTRUE(field$type=="numeric")) {
    chars = as.character(values)
    values = as.numeric(values)
    na.rows = larger.rows = smaller.rows = rep(FALSE, NROW(values))

    na.rows = is.na(values)
    if (isTRUE(field$optional))
      na.rows = na.rows & nchar(chars)>0

    if (!is.null(field$max))
      larger.rows = is.true(values > field$max)

    if (!is.null(field$min))
      smaller.rows = is.true(values < field$max)

    err.rows = which(na.rows | larger.rows | smaller.rows)

    if (length(err.rows)>0) {
      msg = "Not all entries are correct"
      return(list(ok=FALSE,msg=msg, values=values,err.rows=err.rows))
    }
    return(list(ok=TRUE,msg="", values=values,err.rows=NULL))
  }
  if (!isTRUE(fiel$optional)) {
    na.rows = is.na(values) | nchar(value)==0
    if (na.rows) {
      msg = "Forbidden empty entries"
      err.rows = which(na.rows)
      return(list(ok=FALSE,msg=msg, values=values,err.rows=err.rows))
    }
  }
  return(list(ok=TRUE,msg="", values=values))
}
