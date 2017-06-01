examples.side.by.side = function() {
  setwd("D:/libraries/investgame/investgame2")
  file="game2_result_inner.rmd"
  text = readLines(file)

  source("invest_game_2.r")

  params1 = game2(i=0.099,w=1.6)
  params2 = game2(i=0.099,w=0.5)

  le = anchorHtmlList(text, params1)
  ri = anchorHtmlList(text, params2)
  html = make.html.table(le,ri)
  viewMarkdownForm(ui=HTML(html))

  writeLines(html,"test.html")
}

init.form.side.by.side = function(form,...) {
  form$forms = lapply(form$forms, init.form)
  scens = unlist(lapply(form$forms, function(form) form$scen))
  form$scen = c(form[["scen"]], scens)

  form
}

form.ui.side.by.side = function(form, params=NULL, scen.params=NULL,...) {
  restore.point("form.ui.side.by.side")

  li = lapply(seq_along(form$forms), function(i) {
    restore.point("form.ui.side.by.side.inner")

    sform = form$forms[[i]]
    if (!is.null(scen.params)) {
      if (!is.null(sform$scen)) {
        sparams = scen.params[[sform$scen]]
      } else {
        sparams = scen.params[[i]]
      }
      sparams = copy.into.missing.fields(sparams,params)
    } else {
      sparams = params
    }
    text = sform$md_source
    anchorHtmlList(text, sparams)
  })

  html = sidebyside.table(li[[1]],li[[2]])
  HTML(html)
}

anchorHtmlList = function(text, params, knit=TRUE,...) {
  restore.point("anchorHtmlList")

  str = formMarkdownToHTML(text,params, toHTML=FALSE,...)
  li = split.into.anchor.blocks(str)
  li = lapply(li, formMarkdownToHTML, toHTML=TRUE, knit=knit, select.blocks=FALSE, whiskers=FALSE, params=params)
  li
}


sidebyside.table = function(le,ri) {
  df = data.frame(le=unlist(le),ri=unlist(ri))

  str = paste0('<tr><td valign="top" style="border-right-width:3px;	border-right-style:solid; border-right-color: #aaaaaa;	padding-right: 5px;">',df$le,'</td><td valign="top" style=" padding-left: 5px;">',df$ri,"</td></tr>")
  paste0("<table>", paste0(str, collapse="\n"),"</table>")
}

split.into.anchor.blocks = function(text) {
  if (length(text)==1)  text = sep.lines(text)

  rows = which(str.starts.with(text,"#."))
  if (length(rows)==0)
    return(list("#.000",merge.lines(text)))
  start = c(1, rows+1)
  end = c(rows-1, length(text))

  li = lapply(seq_along(start),function(i) {
    merge.lines(text[start[i]:end[i]])
  })
  names(li) = c("#.000",text[rows])
  li

}

formMarkdownToHTML =  function(text, params=NULL, parse.form=TRUE, set.UTF8=TRUE, whiskers=TRUE, knit=TRUE, parent.env = parent.frame(), fragment.only=TRUE, start.token = "# <--START-->", select.blocks=TRUE, toHTML=TRUE, form=NULL, whiskers.call.list=form$whiskers.call.list, markdown.blocks.call.list = form$markdown.blocks.call.list,...) {
  restore.point("formMarkdownToHTML")

  if (length(text)==1) text = sep.lines(text)

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
    text = select.markdown.blocks(text, params,call.list = markdown.blocks.call.list)
  }
  if (whiskers) {
    params$form = form
    set.form(form)
    text = paste0(text, collapse="\n")
    text = webforms.replace.whiskers(text,params, add.params=TRUE, whiskers.call.list=whiskers.call.list)
  }

  if (!toHTML) return(text)

  if (knit) {
    if (!is.null(form))
      set.form(form)
    if (!is.null(params)) {
      env = as.environment(params)
      parent.env(env)<-parent.env
      env$params = params
    } else {
      env = parent.env
    }

    html = knit.text(text=text, quiet=TRUE,envir=env, fragment.only=fragment.only)
    html = gsub("&lt;!&ndash;html_preserve&ndash;&gt;","",html, fixed=TRUE)
    html = gsub("&lt;!&ndash;/html_preserve&ndash;&gt;","",html, fixed=TRUE)
    #html =gsub("\\\\","\\\\\\\\",html, fixed=TRUE)

  } else  {
    # Neccessary to make mathjax work
    #text =gsub("\\\\","\\\\\\\\",text, fixed=TRUE)
    html = markdownToHTML(text=text, fragment.only=fragment.only)
  }
  #rmarkdown::render(text=text)
  HTML(html)

}
