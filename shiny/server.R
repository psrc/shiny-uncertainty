function(input, output, session) {
  vars <- reactiveValues(grps = NULL,
                         gdirlist = NULL)
  
  observeEvent(input$rs_enterButton, {
    vars$grps <- input$rs_num_group
  })

  output$rs_helptext <- renderUI({
    if (is.null(vars$grps)) return(NULL)
    helpText("Select one or more runs for each group")
  })
  
  # should differences between groups be considered
  output$rs_group_diff_ui <- renderUI({
    if (is.null(vars$grps)) return(NULL)
    checkboxInput("rs_group_diff", 
                  label = "Find the difference between groups",
                  value = TRUE)

  })
  
  output$rs_group_select_ui <- renderUI({
    if (is.null(vars$grps)) {
      return(NULL)
    } else {
      lapply(1:vars$grps, function(i) {
        selectizeInput(paste0("group",i),
                       h6(paste("Group", i)),
                       choices = allruns,
                       multiple = TRUE)
      })
    }
  })
  
  output$rs_fazes_ui <- renderUI({
    if (is.null(vars$grps)) return(NULL)
    numericInput("rs_fazes", 
                 label = h6("Select number of FAZes to plot"), 
                 min = 1,
                 value = 20)
  })
  
  output$rs_submitButton_ui <- renderUI({
    if (is.null(vars$grps)) return(NULL)
    actionButton("rs_submitButton", label = "Submit")
  })
  
  observeEvent(input$rs_submitButton, {
    vars$gdirlist <- lapply(1:vars$grps, function(i) input[[paste0("group", i)]]) %>% setNames(paste("Group", 1:vars$grps))
  })
  
 
# Compile data ------------------------------------------------------------


  alldata <- eventReactive(input$rs_submitButton, {
    nlargest <- input$rs_fazes
    dif.between.groups <- input$rs_group_diff
    runs <- vars$gdirlist
    
    # add minutes to run names if there are multiple runs with the same number
    add.minutes <- unlist(runs) %>% lapply(function(x) str_split(x, "/") %>% unlist %>% .[10] %>% str_extract("^([a-z_0-9]+)")) %>% duplicated() %>% any()
                                           
    all.data <- NULL
    i <- 1
    # Load data for each indicator and run, and compute the max difference by faz
    for (ind in indicator.names) {
      ind.name <- tolower(indicator.names[i])
      for (igroup in seq_along(runs)) { 
        ind.data.all <- NULL
        for (run.name in runs[[igroup]]) { 
          # simplify run.name
          rname <- basename(run.name)
          # load data
          rdir <- file.path(run.name, 'indicators') 
          ind.data <- fread(file.path(rdir, paste0('faz__table__', ind.name, '.csv')))[,c('faz_id', paste0(ind.name, '_2040')), with=FALSE]
          indcolname <- strsplit(rname, "[.]")[[1]][1]
          if (add.minutes)
            indcolname <- paste(indcolname, strsplit(strsplit(rname, "[.]")[[1]][2], "_")[[1]][6], sep="_")
          colnames(ind.data)[2] <- indcolname
          ind.data.all <- if(is.null(ind.data.all)) ind.data else cbind(ind.data.all, ind.data[,indcolname, with=FALSE])
        }
        mrdata <- melt(ind.data.all, id.vars='faz_id')
        mrdata[, group := igroup]
        all.data[[ind]] <- rbind(all.data[[ind]], mrdata)
      }
      
      # add difference between min and max by faz
      if(dif.between.groups) {
        all.data[[ind]][ , dif := diff(range(value)), by=faz_id]
      } else all.data[[ind]][ , dif := diff(range(value)), by= .(faz_id, group)]
      # obtain the nlargest fazes with max differences among all groups
      udif <- unique(all.data[[ind]][, .(faz_id, dif, group)])
      udif.max <- udif[, .(max.dif=max(dif)), by=faz_id]
      fidx <- order(udif.max$max.dif, decreasing=TRUE)
      selected.zones <- udif.max[fidx, faz_id][1:nlargest]
      all.data[[ind]] <- subset(all.data[[ind]], faz_id %in% selected.zones)
      # this sets the order of fazes on the x axis 
      all.data[[ind]][, faz_id := factor(faz_id, levels=selected.zones)]
      # add lower and upper bound for drawing vertical lines
      all.data[[ind]][, lower := min(value), by=.(faz_id, group)]
      all.data[[ind]][,upper := max(value), by=.(faz_id, group)]
      i <- i+1
    }
    return(all.data)
  })
  
 
  plotdata <- reactive({
    alldata <- alldata()
   
    pd <- position_dodge(.4) # how far apart are the groups
    g <- list()
    i <- 1
    for (ind in indicator.names) {
      g[[ind]] <- ggplot(alldata[[ind]], aes(x=faz_id, y=value, color=variable, group=group)) + 
        geom_linerange(aes(ymax=upper, ymin=lower, group=group), position=pd, linetype=2, colour="grey") +
        geom_point(position=pd, shape=1) + xlab('') + ylab('') + 
        labs(title=indicator.names[i]) +
        scale_colour_discrete(name = '') + 
        theme(
          legend.position=c(1,1),
          legend.justification=c(1,1), legend.key=element_blank(),
          legend.key.size = unit(0.012, "npc"), 
          plot.title=element_text(size=11, hjust=0, face="bold"), 
          axis.title.x = element_text(size=10),
          legend.background = element_rect(fill="gray90"),
          text = element_text(family="Segoe UI")
          )
      i <- i+1
    }
    return(g)
  })
  
# Plots ------------------------------------------------------------------- 
  

  output$plot_households <- renderPlotly({
   g <- plotdata()
   g2 <- ggplotly(g[['Households']])
  })
  
  output$plot_employment <- renderPlotly({
   g <- plotdata()
   g2 <- ggplotly(g[['Employment']])
  })


} # end server function