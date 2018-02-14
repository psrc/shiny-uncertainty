function(input, output, session) {
  
## Confidence Intervals ----------------------------------------------------  
  
  output$ci_select_ci_dir_ui <- renderUI({
    selectizeInput("ci_select_ci_dir",
                   label = HTML("CI Directory<br/><font color = Gray size = 2>Select one or more directories"),
                   choices = bm.runs,
                   multiple = TRUE)
  })
  
  # output$ci_select_corrruns_ui <- renderUI({
  #   selectizeInput("ci_select_corrruns",
  #                  label = "Runs",
  #                  choices = corres.runs(),
  #                  multiple = TRUE)
  # })
  
  observe({
    if ("All" %in% input$ci_select_county) {
      selected_choices <- setdiff(cnty.choices, "All")
      updateSelectInput(session, "ci_select_county", selected = selected_choices)
    }
  })
  
  # # read cache_directory.txt in each selected bm_ dir as additional run options
  # corres.runs <- eventReactive(input$ci_submitButton, {
  #   runs <- input$ci_select_ci_dir
  #   
  #   # find corresponding runs
  #   filename <- "cache_directories.txt"
  #   corr.runs <- NULL
  #   for (r in seq_along(runs)) {
  #     t <- read.table(file.path(runs[r], filename), col.names = TRUE, stringsAsFactors=F) %>% as.data.table
  #     rs <- t[[colnames(t)]] %>% lapply(function(x) basename(x)) %>% unlist
  #     ifelse(is.null(corr.runs), corr.runs <- rs, corr.runs <- c(corr.runs, rs))
  #   }
  #   
  #   # subset allruns with corresponding runs
  #   sub.runs <- NULL
  #   for (i in seq_along(corr.runs)) {
  #     s <- map(allruns, ~ .[corr.runs[i]]) %>% discard(is.na) 
  #     ifelse(is.null(sub.runs), sub.runs <- s, sub.runs <- append(sub.runs, s))
  #   }
  #   sub.runs <- flatten_chr(sub.runs)
  # })
  
  ci.data <- eventReactive(input$ci_submitButton, {
    runs <- input$ci_select_ci_dir
    tbl <- NULL
  
    # loop through ci directories
    for (r in runs) {
      filenames <- list.files(r, pattern = "^\\d+(_\\w+)+_\\w+\\.txt")
      # loop & read each file
      for (f in filenames) {
        t <- NULL
        t <- read.table(file.path(r, f), header = TRUE) %>% as.data.table()
        t[, `:=` (year = str_extract(f, "\\d+"), 
                  attribute = str_extract(f, "([a-z]*)\\.") %>% str_extract("[a-z]+"),
                  geog = str_extract(f, "[a-z]+_[a-z]+\\.txt") %>% str_extract("^[a-z]+"),
                  cidir = basename(r))]
        ifelse(is.null(tbl), tbl <- t, tbl <- rbind(tbl, t))
      }
    }

    # melt & recast columns: source data
    dt <- melt.data.table(tbl, 
                          id.vars = c("id", "year", "attribute", "geog", "median", "cidir"), 
                          measure.vars = colnames(tbl)[3:6], 
                          variable.name = "bound",
                          value.name = "estimate") 
    dt[,  `:=` (cinterval = str_extract(bound, "\\d+$"), bound2 = str_extract(bound, "[a-z]+"))]
    d <- dcast.data.table(dt, id + year + attribute + geog + cinterval + median + cidir ~ bound2, value.var = "estimate")
    
    # filter and join lookup table
    if (input$ci_select_geog == 'rgs') {
      d1 <- d[year == input$ci_select_year & geog == input$ci_select_geog & cinterval == input$ci_select_ci]
      d2 <- merge(d1, rgs.lu, by.x = "id", by.y = "fips_rgs_id") # display geography codes
      # setnames(d2,"fips_rgs_name","name") # display geography names
      setnames(d2,"fips_rgs_label","name")
      # d2 <- d[year == input$ci_select_year & geog == input$ci_select_geog & cinterval == input$ci_select_ci]
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
    pd <- position_dodge(.7) 

    g <- list()
    i <- 1
    ind.names <- indicator.names %>% tolower
    for (ind in ind.names) {
      d <-  ci.data[attribute == ind & county_name %in% input$ci_select_county,
                    ][, id := as.factor(id)
                      ][, name := as.factor(name)]
      
      g[[ind]] <- ggplot() + 
        geom_point(data = d, 
                   aes(x = reorder(name, median), y = median, colour = cidir), 
                   position = pd, 
                   shape = 21, 
                   fill = "gray90", 
                   size = 1) +
        geom_errorbar(data = d, 
                      aes(x = reorder(name, median), y = median, colour = cidir, ymax=upper, ymin=lower),
                      position = pd, 
                      height = .07) +
        labs(title=indicator.names[i], x = "", y = "") +
        scale_x_discrete() +
        scale_y_continuous(labels = comma, breaks = pretty_breaks(n=8)) +
        coord_flip()+
        theme(
          legend.key.size = unit(0.012, "npc"), 
          plot.title=element_text(size=11, hjust=0, face="bold"), 
          axis.title.x = element_text(size=10),
          axis.ticks.length = unit(.45, "cm"),
          axis.ticks.y= element_line(colour = "white"),
          axis.ticks.x= element_line(colour = "white"),
          legend.background = element_rect(fill="gray90"),
          legend.title = element_blank(),
          text = element_text(family="Segoe UI")
        )
      i <- i + 1
    }
    return(g)
  })
  
  # Currently only Emp nums applicable
  policy <- eventReactive(input$ci_submitButton, {
    cols <- grep("Emp\\d{2}$", names(pol.num), value = TRUE)
    cols2 <- c("fips_rgs_id", "fips_rgs_name", "county_name", "area_name")
    p <- pol.num[, c(cols2, cols), with = FALSE
                 ][, `:=` (attribute = cols, label = "RGS Policy Number", name = as.factor(fips_rgs_id))]
    setnames(p, cols, "policy_est")
    p1 <- p[county_name %in% isolate(input$ci_select_county), ]
    return(p1)
  })
  
  output$ci_plot_hh <- renderPlotly({
    g <- ci.plotdata()
    g2 <- ggplotly(g[['households']])
  })
  
  output$ci_plot_emp <- renderPlotly({
    g <- ci.plotdata()
    p <- policy()
    if (isolate(input$ci_select_geog) == 'rgs') {
      g2 <- ggplotly(g[['employment']] + 
                       geom_point(data = p, 
                                  aes(x = name, y = policy_est, fill = label),
                                  colour = "grey32",
                                  position = position_dodge(.7), 
                                  shape = 0, 
                                  size = 1) +
                       scale_fill_discrete()
      )
    } else {
      g2 <- ggplotly(g[['employment']])
    }
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