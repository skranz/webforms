

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
      if (isTRUE(field$type=="radio")) {
        input="radio"
      } else {
        input = "selectize"
      }
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
  } else if (input == "radio") {
    # choices come from a specified set
    restore.point("fieldInput.radio")

    if (!is.null(choice_set)) {
      for (set in sets[choice_set])
        choices = c(choices, set)
    }
    if (!is.null(choice_labels))
      names(choices) = choice_labels
    li = as.list(choices)
    mutliple = FALSE
    res[[1]] = radioButtons(id, label,choices=choices, selected=value)
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
  restore.point("get.lang.field")
  if (is.null(lang)) return(field)
  name = paste0("lang_",lang)
  if (!is.null(field[[name]])) return(field[[name]])
  field
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


# Check a vector of field values
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


determine.field.input.type = function(input=field[["input"]], choices=field$choices, choice_set=field$choice_set, readonly=isTRUE(field$readonly),type=field$type, field=NULL) {
  restore.point("determine.field.input.type")


  if (readonly)
    return("text")

  if (!is.null(input))
    return(input)

  if (!is.null(type)) return(type)

  if (is.null(choices) & is.null(choice_set)) {
      if (is.null(field$type)) {
        input = "text"
      } else if (field$type=="date") {
        input = "date"
      } else {
        input = "text"
      }
  } else {
    if (isTRUE(field$type=="radio")) {
      input="radio"
    } else {
      input = "selectize"
    }
  }
  input
}

fieldInputVector = function(name=field$name, n=length(value), label=lang.field$label, size=lang.field[["size"]], help=lang.field$help, help_html = lang.field$help_html, note = lang.field[["note"]], note_html=lang.field$note_html, note_title = lang.field$note_title, value=first.none.null(form$params[[name]],lang.field$value, field$value), type=field$type, readonly = isTRUE(field$readonly), min=field$min, max=field$max, step=field$step, maxchar=field$maxchar, choices=first.none.null(lang.field$choices,field$choices),choice_set = first.none.null(lang.field$choice_set,field$choice_set), choice_labels = first.none.null(lang.field$choice_labels, names(lang.field$choices)),  prefix=form$prefix, postfix=form$postfix, field=fields[[name]], fields=form$fields, field_alert = !is.false(opts$field_alert), label.left = first.none.null(field$label.left, opts$label.left, FALSE), opts=form$opts, lang=first.non.null(form[["lang"]],"en"), lang.field = get.lang.field(field, lang), sets = form$sets, form=get.form(), na.is.empty=TRUE, form.control.class=!isTRUE(form$form.control.class), item.separator="_-_",...) {
  restore.point("fieldInputVector")

  if (n==0) return(character(0))

  item.postfix = paste0(item.separator,1:n)
  id = paste0(prefix,name,postfix, item.postfix)

  input = determine.field.input.type(type=type, field=lang.field, choices = choices, choice_set=choice_set, readonly = readonly)

  if (readonly) {
    if (is.null(value)) value = ""
    if (is.na(value)) value = ""
  }

  if (input == "text" | input == "date") {
    if (is.null(value)) value = ""
    if (is.na(value) & na.is.empty) value= ""
    res= textInputVector(id, label=label, value=value, readonly=readonly, size=size, label.left=label.left, form.control.class = form.control.class)
  } else if (input == "checkbox") {
    res= checkBoxInputVector(id, label=label, value=value, readonly=readonly, size=size, label.left=label.left, form.control.class = form.control.class)

  } else if (input == "radio") {
    stop("vector of radio buttons not yet implemented")
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

    multiple = FALSE

    if ( (is.null(value) | isTRUE(field$optional)) & !multiple) {
      choices = c(list(""),choices)
    }
    res = selectizeInputVector(id, label,choices=choices, value=value, multiple=multiple)

  } else if (input == "ace") {
    stop("vector of ace editors not yet implemented")
  }

  if (field_alert) {
    alert_id = paste0(id,"__Alert")
    res = paste0(res, "\n", uiOutputVector(alert_id))
  }
  res
}
