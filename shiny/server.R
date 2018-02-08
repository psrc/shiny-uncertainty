function(input, output, session) {
  
## Confidence Intervals ----------------------------------------------------  
  
  output$ci_select_run_ui <- renderUI({
    selectizeInput("ci_select_run",
                   label = "Select run",
                   choices = bm.runs,
                   multiple = FALSE)
  })
  
  observe({
    if ("All" %in% input$ci_select_county) {
      selected_choices <- setdiff(cnty.choices, "All")
      updateSelectInput(session, "ci_select_county", selected = selected_choices)
    }
  })
  
  ci.data <- eventReactive(input$ci_submitButton, {
    tbl <- NULL
    filenames <- list.files(input$ci_select_run, pattern = "^\\d+(_\\w+)+_\\w+\\.txt")
    
    # loop & read each file
    for (f in filenames) {
      t <- NULL
      t <- read.table(file.path(input$ci_select_run, f), header = TRUE) %>% as.data.table()
      t[, `:=` (year = str_extract(f, "\\d+"), attribute = str_extract(f, "([a-z]*)\\.") %>% str_extract("[a-z]+"), 
                geog = str_extract(f, "[a-z]+_[a-z]+\\.txt") %>% str_extract("^[a-z]+"))]
      ifelse(is.null(tbl), tbl <- t, tbl <- rbind(tbl, t))
    }

    # melt & recast columns: source data
    dt <- melt.data.table(tbl, 
                          id.vars = c("id", "year", "attribute", "geog", "median"), 
                          measure.vars = colnames(tbl)[3:6], 
                          variable.name = "bound",
                          value.name = "estimate") 
    dt[,  `:=` (cinterval = str_extract(bound, "\\d+$"), bound2 = str_extract(bound, "[a-z]+"))]
    d <- dcast.data.table(dt, id + year + attribute + geog + cinterval + median ~ bound2, value.var = "estimate")
    
    # filter and join lookup table
    if (input$ci_select_geog == 'rgs') {
      d1 <- d[year == input$ci_select_year & geog == input$ci_select_geog & cinterval == input$ci_select_ci]
      d2 <- merge(d1, rgs.lu, by.x = "id", by.y = "fips_rgs_id")
      setnames(d2,"fips_rgs_name","name")
    } else if (input$ci_select_geog == 'city') {
      d1 <- d[year == input$ci_select_year & geog == input$ci_select_geog & cinterval == input$ci_select_ci]
      d2 <- merge(d1, cities.lu , by.x = "id", by.y = "city_id") 
      setnames(d2,"city_name","name")
    } else {
      d2 <- d[year == input$ci_select_year & geog == input$ci_select_geog & cinterval == input$ci_select_ci]
    }
    return(d2)
  })
  
  ci.plotdata <- eventReactive(input$ci_submitButton, {
    ci.data <- ci.data()  
    # browser()

    g <- list()
    i <- 1
    ind.names <- indicator.names %>% tolower
    for (ind in ind.names) {
      d <-  ci.data[attribute == ind & county_name %in% input$ci_select_county,
                    ][, id := as.factor(id)
                      ][, name := as.factor(name)]
      
      g[[ind]] <- ggplot(d, aes(y= reorder(name, median), x= median, xmax=upper, xmin=lower)) + 
        geom_errorbarh(aes(xmax=upper, xmin=lower), height = .5, colour="grey") +
        geom_point(shape = 20, size = .5) +
        labs(title=indicator.names[i], x = "", y = "") +
        scale_y_discrete() +
        scale_x_continuous(labels = comma, breaks = pretty_breaks(n=8)) +
        theme(
          legend.position=c(1,1),
          legend.justification=c(1,1), legend.key=element_blank(),
          legend.key.size = unit(0.012, "npc"), 
          plot.title=element_text(size=11, hjust=0, face="bold"), 
          axis.title.x = element_text(size=10),
          axis.ticks.length = unit(.45, "cm"),
          axis.ticks.y= element_line(colour = "white"),
          axis.ticks.x= element_line(colour = "white"),
          legend.background = element_rect(fill="gray90"),
          text = element_text(family="Segoe UI")
        )
      i <- i + 1
    }
    return(g)
  })
  
  output$ci_plot_hh <- renderPlotly({
    g <- ci.plotdata()
    g2 <- ggplotly(g[['households']])
  })
  
  output$ci_plot_emp <- renderPlotly({
    g <- ci.plotdata()
    g2 <- ggplotly(g[['employment']])
  })
  

  
## Random Seed -------------------------------------------------------------
  
  
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
                  label = "Include difference between groups",
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
   # browser()
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