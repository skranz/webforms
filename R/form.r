.APP.FORMS.GLOB = new.env()

examples.appForm = function() {
  setwd("D:/libraries/SeminarMatching/")
  library(yaml)
  library(YamlObjects)


  restore.point.options(display.restore.point = TRUE)
  setwd("D:/libraries/webforms/examples/investgame1")
  file = "game1_input.yaml"

  setwd()
  form = yaml.form(file,utf8 = TRUE, prefix = "myform_", lang="de")
  form$submit.handler = function(ok,values,...) {
    if (!ok) return()
    restore.point("snhdhhfrbf")

    cat("\nGreat you inserted valid numbers!")
    store.form.values(values, file="formvalues.json")
  }
  #form$lang = "de"
  #form$widget.as.character=FALSE
  ui = form.ui.simple(form)


  rmd = readLines("game1_input.rmd")
  set.form(form)
  comp = compile.rmd(text=rmd, whiskers="render",use.commonmark = FALSE,out.type = "shiny")
  eval.placeholders(comp)
  ph = comp$ph
  ui = render.compiled.rmd(comp)

  app = eventsApp()
  add.form.handlers(form)
  app$ui = fluidPage(ui)
  add.form.handlers(form)
  runEventsApp(app, launch.browser = rstudio::viewer)

  df = read.ndjson("formvalues.json")
  df
  df$alpha_1
}


view.form = function(form, params, launch.browser = rstudioapi::viewer, ...) {
  app = eventsApp()
  if (is.null(ui)) {
    ui = form.ui(form=form, params=params,...)
    form = get.form()
  }
  if (!is.null(form)) {
    add.form.handlers(form,function(...) cat("\nGreat, all values are ok!"))
  }
  app$ui = fluidPage(with_mathjax(ui))
  runEventsApp(app, launch.browser=launch.browser)
}


load.and.init.form = function(file=NULL, text=NULL, utf8 = TRUE, lang="en", prefix=NULL, postfix=NULL, warn.no.prefix = TRUE,...) {
  form = read.yaml(file=file, text=text,utf8 = utf8)
  init.form(form, lang=lang, prefix=prefix, postfix=postfix, warn.no.prefix = warn.no.prefix, ...)
}

init.form = function(form, compile.markdown.help=TRUE, lang="en", prefix=form$prefix, postfix=form$postfix, show.alerts = TRUE,   widget.as.character=first.none.null(form$widget.as.character,FALSE), warn.no.prefix=TRUE ) {
  restore.point("init.form")

  if (is.null(prefix) & is.null(postfix) & isTRUE(warn.no.prefix)) {
    warning("It is strongly recommended to init forms with a prefix or postfix that guarantees that field names are unique in your application. If field inputs have the same name, errors can arise that are not easy to detect. So please provide a prefix or postfix to init.form.")
  }


  form$submit.on.failure = first.none.null(form$submit.on.failure, FALSE)
  form$widget.as.character=widget.as.character
  form$show.alerts = show.alerts
  form$prefix = prefix
  form$postfix = postfix
  form$lang = lang
  form$texts = get.form.texts(form, lang=lang)

  if (is.null(form$type))
    form$type = infer.form.type(form)

  if (compile.markdown.help) {
    form = compile.form.markdown.tags(form=form, tags=c("help","note"))
  }

  if (!identical(form$type,"simple")) {
    init.form.fun = paste0("init.form.",form$type)
    if (exists(init.form.fun,mode = "function"))
      form = do.call(init.form.fun, list(form=form))
  }


  form
}

compile.form.markdown.tags = function(form, values=NULL,  tags = c("help","note")
) {
  restore.point("compile.form.markdown.tags")

  inner.compile = function(obj, tags, values, use.lang = TRUE) {
    restore.point("inner.compile")
    if (!is.list(obj)) return(obj)

    for (tag in tags) {
      if (!is.null(obj[[tag]])) {
        obj[[paste0(tag,"_html")]] = replace.whiskers(md2html(obj[[tag]]), values=values)
      }
    }
    if (use.lang) {
      lang.tags = names(obj)[str.starts.with(names(obj),"lang_")]
      for (lang.tag in lang.tags) {
        obj[[lang.tag]] = inner.compile(obj[[lang.tag]], tags=tags, use.lang=FALSE,values=values)
      }
    }
    obj
  }

  form = inner.compile(form,tags=tags, values=values)
  form$fields = lapply(form$fields, inner.compile, tags=tags, values=values)
  form

}

infer.form.type = function(form) {
  restore.point("infer.form.type")

  if (!is.null(form[["type"]])) form$type

  if (!is.null(form[["forms"]])) "side_by_side"

  if (!is.null(form[["file"]])) {
    ext = tolower(tools::file_ext(form$file))
    if (ext == "md" | ext == "rmd") return("markdown")
  }

  type = "simple"
  return(type)

}

set.form = function(form, app=getApp()) {
  if (is.null(app)) {
    .APP.FORMS.GLOB[[".ACTIVE.FORM"]] = form
  } else {
    app$.ACTIVE.FORM = form
  }
}

get.form = function(app=getApp()) {
  if (is.null(app)) {
    .APP.FORMS.GLOB[[".ACTIVE.FORM"]]
  } else {
    app$.ACTIVE.FORM
  }
}

formSubmitButton = function(label=form$texts$submitBtnLabel, form=get.form(), add.form.alert=TRUE) {
  restore.point("formSubmitButton")

  id = paste0(form$prefix,"submitBtn",form$postfix)
  btn = HTML(as.character(actionButton(id, label)))
  if (!add.form.alert) return(btn)

  tagList(formAlertUI(form), btn)


}

formAlertUI = function(form=get.form()) {
  alertId = paste0(form$prefix,"form___Alert",form$postfix)
  formAlert = uiOutput(alertId)
  try(clear.form.alert(form=form), silent = TRUE)
  formAlert
}

add.form.handlers = function(form, submit.handler=form$submit.handler, btn.id = paste0(form$prefix,"submitBtn",form$postfix),...) {
  restore.point("add.form.handlers")


  buttonHandler(btn.id,formSubmitClick, form=form, submit.handler=submit.handler,...)
}

form.ui = function(form, params=form$params, add_handlers=FALSE,  success_fun=form$success_fun,...) {
  restore.point("form.ui")

  ui = NULL

  form.fun = paste0("form.ui.",form$type)
  ui = do.call(form.fun, list(form=form, params=params,...))
  if (add_handlers) {
    add.form.handlers(form=form, success_fun=success_fun,...)
  }
  ui
}


formSubmitClick = function(form, submit.handler = NULL,app=getApp(),id=NULL,session=NULL,...) {
  restore.point("formSubmitClick")

  res = get.form.values(form=form)
  restore.point("formSubmitClick_2")
  if (!is.null(submit.handler)) {
    if (isTRUE(form$submit.on.failure) | res$ok)
      submit.handler(ok=res$ok,values=res$values, form=form,...)
  }
}



get.form.values = function(form=get.form(),fields=form$fields,field.names=names(fields), prefix=form$prefix, postfix=form$postfix, check.values = TRUE, show.alerts=isTRUE(form$show.alerts)) {

  values = lapply(field.names, function(name) {
    id = paste0(prefix,name,postfix)
    getInputValue(id)
  })
  #cat("get.form.values:\n ", paste0(names(values),": ",values,collapse="\n"))
  restore.point("get.form.values")


  names(values) = field.names
  if (!check.values)
    return(values)

  check = check.form.values(values, form=form, show.alerts=show.alerts)

  if (show.alerts) {
    if (!check$ok) {
      show.form.alert(form=form,msg=form$texts$submitFailure)
    } else {
      show.form.alert(form=form,msg=form$texts$submitSuccess, color=NULL)
    }
  }
  return(check)
}

check.form.values = function(values, form, fields=form$fields[field.names], field.names=names(values), show.alerts = TRUE, get.failure.msg = FALSE) {
  restore.point("check.form.values")

  li = lapply(field.names, function(name) {
    ret = check.field.value(values[[name]], fields[[name]], lang=form$lang)
    if (!ret$ok & show.alerts) {
      show.field.alert(name=name, msg=ret$msg, form=form)
    } else {
      clear.field.alert(name=name, form=form)
    }
    ret
  })
  names(li)= field.names
  values = lapply(li, function(el) el$value)
  failed.fields = field.names[sapply(li, function(el) !el$ok)]
  ok = length(failed.fields) == 0
  if (get.failure.msg) {
    failure.msg = sapply(li[failed.fields], function(el) el$msg)
    return(list(ok=ok,values=values,failed.fields=failed.fields, failure.msg=failure.msg))
  }
  return(list(ok=ok,values=values,failed.fields=failed.fields))
}


clear.form.alert = function(form=NULL,msg="", prefix=form$prefix, postfix=form$postfix, color=NULL,id = paste0(prefix,"form___Alert",postfix)) {
  show.form.alert(msg=msg, form=form, color=color, prefix=prefix,postfix=postfix, id=id)
}

show.form.alert = function(form=NULL,msg="", prefix=form$prefix, postfix=form$postfix, color="red",id = paste0(prefix,"form___Alert",postfix)) {
  restore.point("show.form.alert")

  #cat("\n*********************************")
  #cat("\nshow.form.alert:\n", msg)
  #cat("\n*********************************")

  if (is.null(msg)) msg = ""
  if (!is.null(color))
    msg = colored.html(msg, color)

  setUI(id,HTML(msg))
}


clear.field.alert = function(name=field$name,field=NULL, form=NULL, prefix=form$prefix, postfix=form$postfix, id = paste0(prefix,name,postfix,"__Alert")) {

  show.field.alert(name=name,msg="", form=form, color=NULL, prefix=prefix,postfix=postfix, id=id)
}

show.field.alert = function(name=field$name, msg="",field=NULL, prefix=form$prefix, postfix=form$postfix, form=NULL, color="red",id = paste0(prefix,name,postfix,"__Alert"), resize=NULL) {
  restore.point("show.field.alert")


  if (!is.null(color))
    msg = colored.html(msg, color)

  if (!is.null(resize)) {
    dsetUI(id,HTML(msg))
    resizeLayout(resize)
  } else {
    setUI(id,HTML(msg))
  }
}

check.field.value = function(value, field, lang="en") {
  restore.point("check.field.value")

  if (is.null(value)) value = ''
  if (!is.null(field$collapse)) {
    value = paste0(value,collapse=field$collapse)
  }

  if (isTRUE(field$type=="date")) {
    if (is(value,"Date")) return(list(ok=TRUE,msg="", value=value))
    value = str.trim(as.character(value))
    if (nchar(value)==0) {
      if (isTRUE(field$optional)) return(list(ok=TRUE,msg="", value=as.Date(NA, origin="1970-01-01")))
    }
    value = try(as.Date(value), silent=TRUE)
    if (is(value,"try-error")) value = as.Date(NA, origin="1970-01-01")
    if (is.na(value)) {
      msg = "Please enter a date in the format yyyy-mm-dd, e.g. 2014-12-24."
      return(list(ok=FALSE,msg=msg, value=value))
    }
    return(list(ok=TRUE,msg="", value=value))
  } else if (isTRUE(field$type=="numeric")) {
    num = as.numeric(value)
    if (is.na(num)) {
      if (value %in% field$na_value | isTRUE(field$optional)) {
        return(list(ok=TRUE,msg="", value=num))
      }
      msg = field.failure.msg(field, value, lang=lang)
      return(list(ok=FALSE,msg=msg, value=num))
    }

    if (!is.null(field$max)) {
      if (num>field$max) {
        msg = field.failure.msg(field, value, lang=lang)
        return(list(ok=FALSE,msg=msg, value=num))
      }
    }
    if (!is.null(field$min)) {
      if (num<field$min) {
        msg = field.failure.msg(field, value, lang=lang)
        return(list(ok=FALSE,msg=msg, value=num))
      }
    }
    return(list(ok=TRUE,msg="", value=num))
  }
  if (nchar(value)==0 & !isTRUE(field$optional)) {
    msg = field.failure.msg(field, value, lang=lang)
    return(list(ok=FALSE,msg=msg, value=value))
  }
  return(list(ok=TRUE,msg="", value=value))
}

field.failure.msg = function(field,value, use.custom = TRUE, lang="en") {
  restore.point("field.failure.msg")

  if (use.custom & !is.null(field$failure_msg))
    return(field$failure_msg)

  if (isTRUE(lang=="de")) {
  if (isTRUE(field$type=="numeric")) {
      msg = "Bitte geben Sie eine Zahl "
      if (!is.null(field[["min"]]) & !is.null(field[["max"]])) {
        msg = paste0(msg, " zwischen ", field[["min"]], " und ", field[["max"]])
      } else if (!is.null(field[["min"]])) {
        msg = paste0(msg, " größer gleich ", field[["min"]])
      } else if (!is.null(field[["max"]])) {
        msg = paste0(msg, " kleine gleich ", field[["min"]])
      }
      if (!is.null(field$na_value)) {
        msg = paste0(msg,"ein. Für keinen Eintrag geben Sie ", paste0('"',field$na_value,'"', collapse=" oder "))
      }
      msg = paste0(msg,"ein.")
      msg = mark_utf8(msg)
      return(msg)
    }

    msg = mark_utf8("Bitte machen Sie eine gültige Eingabe.")

  } else {
    if (isTRUE(field$type=="numeric")) {
      msg = "Please enter a number "
      if (!is.null(field[["min"]]) & !is.null(field[["max"]])) {
        msg = paste0(msg, " between ", field[["min"]], " and ", field[["max"]])
      } else if (!is.null(field[["min"]])) {
        msg = paste0(msg, " above or equal to ", field[["min"]])
      } else if (!is.null(field[["max"]])) {
        msg = paste0(msg, " below or equal to ", field[["min"]])
      }
      if (!is.null(field$na_value)) {
        msg = paste0(msg,". For no number enter ", paste0('"',field$na_value,'"', collapse=" or "))
      }
      msg = paste0(msg,".")
      return(msg)
    }

    msg = "Please enter a valid input."

  }

  return(msg)
}

get.lang.field = function(field, lang=NULL) {
  if (is.null(lang)) return(field)
  name = paste0("lang_",lang)
  if (!is.null(field[[name]])) return(field[[name]])
  field
}


get.lang.form = function(form, lang=NULL) {
  restore.point("get.lang.form")

  if (is.null(lang)) return(form)
  name = paste0("lang_",lang)
  if (!is.null(form[[name]])) return(form[[name]])
  form
}


fieldInput = function(name=field$name, label=lang.field$label, size=lang.field[["size"]], help=lang.field$help, help_html = lang.field$help_html, note = lang.field[["note"]], note_html=lang.field$note_html, note_title = lang.field$note_title, value=first.none.null(form$params[[name]],lang.field$value, field$value), type=field$type, readonly = isTRUE(field$readonly), min=field$min, max=field$max, step=field$step, maxchar=field$maxchar, choices=first.none.null(lang.field$choices,field$choices),choice_set = first.none.null(lang.field$choice_set,field$choice_set), choice_labels = first.none.null(lang.field$choice_labels, names(lang.field$choices)),  prefix=form$prefix, postfix=form$postfix, field=fields[[name]], fields=form$fields, field_alert = !is.false(opts$field_alert), label.left = first.none.null(field$label.left, opts$label.left, FALSE), opts=form$opts, lang=form[["lang"]], lang.field = get.lang.field(field, lang), sets = form$sets, widget.as.character = !is.false(form$widget.as.character), form=get.form(), na.is.empty=TRUE, form.control.class=!isTRUE(form$form.control.class)) {

  restore.point("fieldInput")

  res = vector("list",3)

  if (isTRUE(opts$name_as_label) & is.null(label)) {
    label=name
  }

  id = paste0(prefix,name,postfix)
  input = field[["input"]]

  if (is.null(input)) {
    if (is.null(choices) & is.null(choice_set)) {
      if (is.null(field$type)) {
        input = "text"
      } else if (field$type=="date") {
        input = "date"
      } else {
        input = "text"
      }
    } else {
      input = "selectize"
    }
  }

  if (readonly) {
    input = "text"
    if (is.null(value)) value = ""
    if (is.na(value)) value = ""
    #choices = list(value)
    #names(choices) = as.character(value)
    #choice_set = NULL
  }


  if (input == "text") {
    if (is.null(value)) value = ""
    if (is.na(value) & na.is.empty) value= ""
    if (TRUE | widget.as.character | readonly) {
      res[[1]] = textInputVector(id, label=label, value=value, readonly=readonly, size=size, label.left=label.left, form.control.class = form.control.class)
      if (!widget.as.character)
        res[[1]] = HTML(res[[1]])
    } else {
      res[[1]] = textInput(id, label, value)
    }
  } else if (input == "date") {
    if (is.null(value)) value = ""
    if (is.na(value) & na.is.empty) value= ""
    #res[[1]] = dateInput(id, label=label, value=value)
    res[[1]] = textInput(id, label=label, value=value)
  } else if (input == "selectize") {
    # choices come from a specified set
    restore.point("fieldInput.selectize")

    if (!is.null(choice_set)) {
      for (set in sets[choice_set])
        choices = c(choices, set)
    }
    if (!is.null(choice_labels))
      names(choices) = choice_labels
    li = as.list(choices)

    multiple = isTRUE(field[["multiple"]])
    if (multiple & !is.null(field$collapse)) {
      value = strsplit(value,split = field$collapse,fixed = TRUE)[[1]]
    }

    if ( (is.null(value) | isTRUE(field$optional)) & !multiple) {
      choices = c(list(""),choices)
    }
    res[[1]] = selectizeInput(id, label,choices=choices, selected=value, multiple=multiple)

  } else if (input == "ace") {
    library(shinyAce)
    if (is.null(value)) value = ""
    height = first.none.null(field[["height"]], 200)
    mode = first.none.null(field[["mode"]], "text")

    widget = aceEditor(outputId = id, value = value,mode = mode,height=height, showLineNumbers = FALSE)

    if (!is.null(label)) {
      res[[1]] = list(
        HTML(paste0('<label for="',id,'">',label,'</label>')),
        widget
      )
    } else {
      res[[1]] = widget
    }

  }

  ind = 2
  if (field_alert) {
    alert_id = paste0(id,"__Alert")
    res[[ind]] = uiOutput(alert_id)
    ind = ind+1
  }

  if (!is.null(help_html)) {
    #span(class = "help-block", ...)
    res[[ind]] = span(class = "help-block",HTML(help_html))
    ind = ind+1
  } else if (!is.null(help)) {
    res[[ind]] = helpText(help)
    ind = ind+1
  }

  if (!is.null(note_html)) {
    res[[ind]] = bsCollapse(bsCollapsePanel(title=note_title,HTML(note_html)))
  } else if (!is.null(note)) {
    res[[ind]] = bsCollapse(bsCollapsePanel(title=note_title,helpText(note)))
  }


  if (widget.as.character) {
    ret = paste0(lapply(res[1:ind],as.character),"\n", collapse="\n")
    return(ret)
  } else {
    return(res)
  }

}

form.default.values = function(form, values = NULL, sets=form[["sets"]], boolean.correction=TRUE) {
  vals = lapply(form$fields, function(field) field.default.values(form=form, field=field,sets=sets))

  replace = intersect(names(vals), names(values))

  if (length(replace)>0) {
    if (boolean.correction)
      boolean = sapply(vals[replace], function(val) is.logical(val) & !is.na(val))
    vals[replace] = values[replace]
    if (boolean.correction)
      vals[replace][boolean] =  lapply(vals[replace][boolean], as.logical)
  }
  vals
}

form.random.values = function(form, values = NULL, sets=form[["sets"]]) {
  restore.point("form.random.values")

  vals = lapply(form$fields, function(field) field.random.value(form=form, field=field,sets=sets))

  replace = intersect(names(vals), names(values))

  if (length(replace)>0) {
    vals[replace] = values[replace]
  }
  vals
}


form.schema.template = function(form) {
  li = form.default.values(form)
  schema.template(li)
}

field.random.value = function(form, field, sets = NULL) {
  #restore.point("field.random.value")

  if (!is.null(field[["value"]])) field$value


  choices = unlist(field$choices)
  choice_set = field$choice_set
  if (!is.null(choice_set)) {
    for (set in sets[intersect(choice_set,names(sets))]) {
      choices = c(choices, unlist(set))
    }
  }

  if (length(choices)>0)
    return(sample(choices,1))

  return(sample(0:1,1))
}


field.default.values = function(form, field, sets = NULL) {
  if (!is.null(field[["value"]])) field$value

  if (isTRUE(field$numeric)) return(NA_real_)

  choices = field$choices
  if (length(choices)>0) return(choices[[1]])

  choice_set = field$choice_set
  if (!is.null(choice_set)) {
    for (set in sets[intersect(choice_set,names(sets))]) {
      if (length(set)>0) return(set[[1]])
    }
  }
  return("")
}

get.form.texts = function(form, lang="en") {
  restore.point("get.form.texts")

  if (isTRUE(lang=="de")) {
    texts = list(
      submitBtnLabel = "Speichern",
      submitFailure = "Einige notwendige Angaben fehlen oder sind inkorrekt.",
      submitSuccess = ""
    )
  } else {
    texts = list(
      submitBtnLabel = "Submit",
      submitFailure = "Some required entries are missing or wrong.",
      submitSuccess = ""
    )
  }
  if (!is.null(form$texts)) {
    te = get.lang.object(form$texts,lang=lang)
    texts[names(te)] = te
  }

  texts
}

get.lang.object = function(obj, lang="en") {
  restore.point("get.lang.object")

  if (!is.null(obj[[paste0("lang_",lang)]])) {
    return(obj[[paste0("lang_",lang)]])
  } else if (!is.null(obj[["lang_en"]])) {
    return(obj[["lang_en"]])
  }
  return(obj)
}
