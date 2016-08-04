
examples.markdown.form = function() {
  view.rmd.form("game1_input.rmd")
  rmd = readLines("game1_input.rmd")

  res = read.form.rmd(txt = rmd)

}

view.rmd.form = function(file, txt=readLines(file, warn=FALSE), prefix="form", launch.browser = rstudio::viewer) {
  restore.point("view.rmd.form")

  res = read.form.rmd(txt=txt)
  form = init.form(res$form, prefix=prefix)
  set.form(form)
  cr = compile.rmd(text=res$rmd)
  ui = render.compiled.rmd(cr, out.type = "shiny",fragment.only = TRUE)
  app = eventsApp()
  app$ui = fluidPage(withMathJax(ui))
  viewApp(app, launch.browser=launch.browser)
}


read.form.rmd = function(file, txt=readLines(file, warn=FALSE)) {
  restore.point("read.form.rmd")

  res = separate.yaml.and.rmd(txt)
  li = read.yaml(text = res$yaml)
  form = li[["form"]]
  settings = li[setdiff(names(li),"form")]
  list(form=form, rmd=res$rmd,settings=settings)

}

separate.yaml.and.rmd = function(txt) {
  res = partition_yaml_front_matter(txt)
  yaml = res$front_matter
  if (length(yaml)>=2)
    yaml = yaml[-c(1,length(yaml))]
  yaml = paste0(yaml, collapse="\n")
  rmd = rmd.between.start.end.lines(res$body)

  list(yaml = yaml, rmd=rmd)
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

