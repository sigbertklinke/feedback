library("shiny")
library("shinydashboard")
library("colorspace")

defaults <- function (input, ...) { 
  inp  <- list(...)
  for (arg in names(inp)) {
    if (!is.null(input[[arg]])) inp[[arg]] <- input[[arg]]
  }
  return(inp)
}

ui <- dashboardPage(
  dashboardHeader(title="Feedback", titleWidth=300),
  dashboardSidebar(width=300,
      uiOutput("codeUI"),
      uiOutput("variableUI"),
      uiOutput("groupUI"),
      sidebarMenu(menuItemOutput("options")),
      uiOutput("tool"),
      uiOutput("copyright")
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1"), width=12) #, background="red")
    )
  )
)

drawPercentLine <- function(x, usr) {
  h99 <- usr[4]-0.01*diff(usr[3:4])
  h98 <- usr[4]-0.02*diff(usr[3:4])
  lines(c(usr[1], x), c(h99, h99), col="red")
  lines(c(x, x), c(h99, h98), col="red")
}

plotBarchart <- function(tab, main="Undefined", sub='', fontsize=1, parallel=F, sel=vector("numeric"), legendright=F) {
  par(mfrow=c(1,1))
  if (length(dim(tab))==2) {  # eliminate NA-lines
    tab <- tab[rowSums(is.na(tab))<ncol(tab),]    
  }
  ylim <- c(0, 1.2*max(tab, na.rm=TRUE))
  if (length(dim(tab))<2) { # simple barchart 
    col <- rep("grey", length(tab))
    if (length(sel)>0) col[sel] <- "red"
    mp <- barplot(tab, col=col, main=main,sub=sub, axes=F, ylim=ylim,
                  xlab="Score",
                  cex.names=fontsize, cex.axis=fontsize,
                  cex.lab=fontsize, cex.main=fontsize,
                  cex.sub=fontsize, col.sub="red")
    if (length(sel)>0) {
  #    browser()
      usr <- par("usr")
      for (i in seq(length(sel))) {
        drawPercentLine (mp[sel[i]], usr)
        cdf <- 100*sum(tab[1:sel[i]])
        text(mp[sel[i]], usr[4]-0.05*diff(usr[3:4]), sprintf('%3.0f%%', cdf), col="red")
      }
    }
    graphics::box()
  } else{
    gry <- sequential_hcl(nrow(tab), h = 260, c = 0, l = c(30, 85), power = 1.5)
    if (parallel) {
      par(mfrow=c(1, nrow(tab)))
      for (i in 1:nrow(tab)) {
        col <- rep(gry[i], length(tab))
        if (length(sel)>0) col[sel] <- "red"
        mp <- barplot(tab[i,], col=col,  axes=F, ylim=ylim, 
                      xlab="Score",
                      cex.names=fontsize, cex.axis=fontsize,
                      cex.lab=fontsize, cex.main=fontsize,
                      cex.sub=fontsize, col.sub="red") 
        if (length(sel)>0) {
          usr <- par("usr")
          for (j in seq(length(sel))) {
            drawPercentLine (mp[sel[j]], usr)
            cdf <- 100*sum(tab[i,1:sel[j]])
            text(mp[sel[j]], usr[4]-0.05*diff(usr[3:4]), sprintf('%3.0f%%', cdf), col="red")
          }
        }
        if (i==1) legend(if(legendright) "topright" else "topleft", legend=rownames(tab), fill=gry, cex=fontsize)
        graphics::box()
      }
    	mtext(bquote(bold(.(main))), side = 3, line = -3, outer = TRUE, cex=fontsize)
    	mtext(sub, side = 1, line = -1, outer = TRUE, cex=fontsize, col="red")
    } else {
      col <- matrix(gry, nrow=nrow(tab), ncol=ncol(tab))
      if (length(sel)>0) col[,sel] <- "red"
      mp  <- barplot(tab, beside=T, col=col, main=main, sub=sub, axes=F, ylim=ylim,
                     xlab="Score",
                     cex.names=fontsize, cex.axis=fontsize,
                     cex.lab=fontsize, cex.main=fontsize,
                     cex.sub=fontsize, col.sub="red")
      usr <- par("usr")
      if (length(sel)>0) {
        #browser()
        for (i in 1:nrow(tab)) {
          for (j in seq(length(sel))) {
            drawPercentLine (mp[i,sel[j]], usr)
            cdf <- 100*sum(tab[i,1:sel[j]]) 
            text(mp[i,sel[j]], usr[4]-i*0.05*diff(usr[3:4]), sprintf('%3.0f%%', cdf), col="red")
          }
        }
      }
      graphics::box()
      legend(if(legendright) "topright" else "topleft", legend=rownames(tab), fill=gry, cex=fontsize)
      graphics::box()
    }
  }
}

server <- function(input, output, session) {

  data <- reactiveValues(file=NULL, vars=NULL, groups=NULL, codes=NULL)
  
	init <- observe({
		query <- parseQueryString(session$clientData$url_search)
		fn    <- if (length(query)) paste0(query$data, '.RDS') else 'feedback.RDS'
		file <- readRDS(fn)
		if (!('feedback' %in% class(file)) || (file$.VERSION!='0.3')) stop("Invalid feedback object")
		# statistical data
		data$file   <- file
		data$vars   <- file$.VAR
		data$codes  <- c('.', file$.CODE)
		data$groups <- file$.GROUP
	})

  output$codeUI  <- renderUI({
    if (is.null(data$codes) || (length(data$codes)==0)) return(NULL)
    selectizeInput("code","Code(s)", choices = data$codes, width = '98%', 
    							  options=list(maxOptions=length(data$codes)))
  })

  output$variableUI <- renderUI({
    if (is.null(data$vars) || (length(data$codes)==0)) return(NULL)
    selectInput("variable", "Score(s)", selectize = FALSE,
                  choices = data$vars, multiple=FALSE, size = 3,
                  width = '98%')
  })
  
  output$groupUI <- renderUI({
    if (is.null(data$groups) || (length(data$groups)<2)) return(NULL)
  	list(selectInput("group", "Gruppe", selectize = TRUE,
  								    choices = data$groups, multiple=FALSE,
  								    width = '98%'),
  		   checkboxInput("parallel", "Parallele Darstellung", value = FALSE))
  })
  
  output$options <- renderUI({
    menuItem("Optionen",
             sliderInput("fontsize", "Schriftgrösse", min=0.8, max=2, value=1.25, step=0.05),
             checkboxInput("legend", "Legende rechts", FALSE)
            )
  })

  output$tool <- renderUI({
    helpText(gettext("Feedback tool v0.3"))
  })

  output$copyright <- renderUI({
    helpText("(C) 2019- Sigbert Klinke, Humboldt-Universität zu Berlin, Lehrstuhl für Statistik")
  })

  output$plot1 <- renderPlot({
  	inp  <- defaults(input, variable=data$vars[1], group='.',
  										code='.', percentrank='F', fontsize=1.25, parallel=F,	legend=F)
    if (is.null(inp$group)) inp$group = '.'
  	tab <- data$file[[inp$variable]][[inp$group]]
  	selind <- which(data$file$.ID==inp$code)
  	sel    <- unique(which(levels(data$file[[inp$var]]$.SCORE)==data$file[[inp$var]]$.SCORE[selind]))
  	sub    <- if (inp$code=='.') '' else inp$code
  	if (data$file[[inp$var]]$.CHART=='barchart') {
    	plotBarchart(tab, main=inp$variable, sub=sub, fontsize=inp$fontsize, 
    	             parallel=inp$parallel, sel=sel, legendright=inp$legend) 
  	}
  })
}

shinyApp(ui, server)
