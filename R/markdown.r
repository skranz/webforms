init.form.markdown = function(form, start.token = "# <--START-->") {
  restore.point("init.markdown.form")

  if (!is.null(form$file)) {
    text = readLines(form$file, warn=FALSE)
    rows = NULL
    if (!is.null(start.token)) {
      rows = which(text==start.token)
    }
    if (length(rows)>0) {
      form$md_source = text[(rows[1]+1):length(text)]
    } else {
      form$md_source = text
    }
    form$text = text
  }
  if (!is.null(form$text)) {
    form = inject.front.matter.form(form=form)
  }

  if (!is.null(form$md_source)) {
    txt = paste0(form$md_source,collapse="\n")
    form$whiskers.call.list = whiskers.call.list(txt)
    form$markdown.blocks.call.list = markdown.blocks.call.list(form$md_source)
  }

  form

}


form.ui.markdown = function(form=NULL,file=form[["file"]], text=form$md_source, parse.form=TRUE, params = form[["params"]], set.UTF8=TRUE, whiskers=TRUE, knit=isTRUE(form$knit), parent.env = parent.frame(), fragment.only=TRUE, start.token = "# <--START-->", ret.val="HTML", select.blocks=TRUE,whiskers.call.list=form$whiskers.call.list,markdown.blocks.call.list = form$markdown.blocks.call.list, ...) {
  restore.point("form.ui.markdown")

  if (!is.null(file) & is.null(text)) {
    text = readLines(file,warn = FALSE)
  } else {
    if (length(text)==1) text = sep.lines(text)
  }

  if (set.UTF8)
    Encoding(text)<-"UTF-8"

  if (parse.form & is.null(form)) {
    form = get.front.matter.form(text=text)
  }


  if (!is.null(start.token)) {
    rows = which(text==start.token)
    if (length(rows)>0) {
      text = text[(rows[1]+1):length(text)]
    }
  }

  if (select.blocks & !is.null(params)) {
    text = sep.lines(text)
    text = select.markdown.blocks(text, params, call.list = markdown.blocks.call.list)
  }
  if (whiskers) {
    params$form = form
    set.form(form)
    text = paste0(text, collapse="\n")
    text = replace.whiskers(text,params, whiskers.call.list=whiskers.call.list)
  }
  if (knit) {
    if (!is.null(form))
      set.form(form)
    if (!is.null(params)) {
      env = as.environment(params)
      parent.env(env)<-parent.env
    } else {
      env = parent.env
    }
    if (ret.val == "md") {
      return(knit(text=text,quiet=TRUE,envir=env))
    }

    html = knit.text(text=text, quiet=TRUE,envir=env, fragment.only=fragment.only)
    html = gsub("&lt;!&ndash;html_preserve&ndash;&gt;","",html, fixed=TRUE)
    html = gsub("&lt;!&ndash;/html_preserve&ndash;&gt;","",html, fixed=TRUE)

  } else {
    if (ret.val == "md") {
      return(text)
    }
    # Neccessary to make mathjax work
    #text =gsub("\\\\","\\\\\\\\",text, fixed=TRUE)
    html = markdownToHTML(text=text, fragment.only=fragment.only)
  }
  #rmarkdown::render(text=text)
  HTML(html)

}



view.form.markdown= function(file=NULL,form=NULL, params=NULL, knit=TRUE, launch.browser = rstudioapi::viewer, ui = NULL,...) {
  app = eventsApp()
  if (is.null(ui)) {
    ui = form.ui.markdown(file=file, form=form, params=params, knit=knit,...)
    form = get.form()
  }
  if (!is.null(form)) {
    add.form.handlers(form,function(...) cat("\nGreat, all values are ok!"))
  }
  app$ui = fluidPage(with_mathjax(ui))
  runEventsApp(app, launch.browser=launch.browser)
}



inject.front.matter.form = function(form, text=form[["text"]]) {
  restore.point("inject.fron.matter.form")

  fm.form = try(get.front.matter.form(form,text = text))
  if (is(fm.form,"try-error")) return(form)

  fields = setdiff(names(fm.form),names(form))
  form[fields] = fm.form[fields]
  form
}


get.front.matter.form = function(file, text = readLines(file, warn = FALSE)) {
 fm = parse_yaml_front_matter(text)
 fm$form
}



parse_yaml_front_matter <- function(input_lines) {
  partitions <- partition_yaml_front_matter(input_lines)
  if (!is.null(partitions$front_matter)) {
    front_matter <- partitions$front_matter
    if (length(front_matter) > 2) {
      front_matter <- front_matter[2:(length(front_matter)-1)]
      front_matter <- paste(front_matter, collapse="\n")
      validate_front_matter(front_matter)
      parsed_yaml <- read.yaml(text=front_matter)
      if (is.list(parsed_yaml))
        parsed_yaml
      else
        list()
    }
    else
      list()
  }
  else
    list()
}

validate_front_matter <- function(front_matter) {
  front_matter <- trim_trailing_ws(front_matter)
  if (grepl(":$", front_matter))
    stop("Invalid YAML front matter (ends with ':')", call. = FALSE)
}



partition_yaml_front_matter <- function(input_lines) {
  validate_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 &&
        (delimiters[2] - delimiters[1] > 1) &&
        grepl("^---\\s*$", input_lines[delimiters[1]])) {
      # verify that it's truly front matter (not preceded by other content)
      if (delimiters[1] == 1)
        TRUE
      else
        is_blank(input_lines[1:delimiters[1]-1])
    } else {
      FALSE
    }
  }

  # is there yaml front matter?
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {

    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]

    input_body <- c()

    if (delimiters[1] > 1)
      input_body <- c(input_body,
                      input_lines[1:delimiters[1]-1])

    if (delimiters[2] < length(input_lines))
      input_body <- c(input_body,
                      input_lines[-(1:delimiters[2])])

    list(front_matter = front_matter,
         body = input_body)
  }
  else {
    list(front_matter = NULL,
         body = input_lines)
  }
}

is_blank = function (x)
{
    if (length(x))
        all(grepl("^\\s*$", x))
    else TRUE
}

trim_trailing_ws = function (x)
{
    sub("\\s+$", "", x)
}

