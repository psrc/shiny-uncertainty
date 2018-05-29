function(input, output, session) {


# Generic Functions -------------------------------------------------------


  capitalize <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }

  
# Actions -----------------------------------------------------------------  
  
  
  observeEvent(input$ci_link_to_About, {
    newvalue <- "About"
    updateNavbarPage(session, "inNavbarPage", newvalue)
  })
  
  observeEvent(input$ci_aapc_link_to_About, {
    newvalue <- "About"
    updateNavbarPage(session, "inNavbarPage", newvalue)
  })  
  
  
    
## Baseyear Data (2014) ----------------------------------------------------  

  
  baseyr.data <- reactive({
    run <- "run_78.run_2018_02_28_14_03" # hardcoding this run for baseyr data b/c 1) rgs_id correct & 2) has rgs pop indicator
    # run <- "run_2.run_2018_02_27_14_54" # this run does not have rgs pop indicator
    dir <- map(allruns, run) %>% purrr::discard(is.null)

    geographies <- c("fips_rgs", "city")
    all.indicator.names <- c(indicator.names, "Population") %>% tolower
    grep.ind.pat <- paste(all.indicator.names, collapse = "|")

    dt <- NULL
    # create regional geog file and bind to dt
    reg.files <- list.files(file.path(dir, "indicators"), paste0(geographies[1], "(__\\w+)*\\.csv"))

    for (rf in reg.files) {
      t <- read.csv(file.path(dir, "indicators", rf), header = TRUE) %>% as.data.table
      t0 <- t[, c(paste0(geographies[1], "_id"), grep("2014", colnames(t), value = TRUE)), with = FALSE
              ][,`:=` (geog = "rgs", attribute = str_extract(rf, grep.ind.pat), year = str_extract(colnames(t)[2], "\\d+"))]
      setnames(t0, colnames(t0)[1:2], c("id", "baseyr"))
      ifelse(is.null(dt), dt <- t0, dt <- rbind(dt, t0))
    }
    # create city file and bind to dt
    for (f in all.indicator.names) {
      t2 <- read.csv(file.path(dir, "indicators", paste0(geographies[2], "__table__", f, ".csv"))) %>% as.data.table
      t3 <- t2[, c(paste0(geographies[2], "_id"), grep("2014", colnames(t2), value = TRUE)), with = FALSE
               ][,`:=` (geog = "city", attribute = f, year = str_extract(colnames(t2)[2], "\\d+"))]
      setnames(t3, colnames(t3)[1:2], c("id", "baseyr"))
      dt <- rbind(dt, t3)
    }
    return(dt)
  })


## Capacity Data -----------------------------------------------------------

  # cap.data <- reactive({
  #   c <- melt.data.table(cap, id.vars = colnames(cap)[1:4], measure.vars = colnames(cap)[5:6], variable.name = "attribute", value.name = "capacity")
  # }) 

  
## Confidence Intervals ----------------------------------------------------  
  
  output$ci_select_ci_dir_ui <- renderUI({
    selectizeInput("ci_select_ci_dir",
                   label = HTML("CI Directory<br/><font color = Gray size = 2>Select one or more directories"),
                   choices = bm.runs,
                   multiple = TRUE)
  })
  
  output$ci_select_helptext_ui <- renderUI({
    if (input$ci_preview_submitButton) {
      helpText("Adjust the following criteria as necessary and click 'Enter'")
    } else {
      return(NULL)
    }
  })
  
  output$ci_select_year_ui <- renderUI({
    if (input$ci_preview_submitButton) {
      if (is.null(ci.data.preview())) return(NULL)
      years <- unique(ci.data.preview()$year)
      selectInput("ci_select_year",
                  label = "Year",
                  choices = years,
                  selected = 2050)
    } else {
      return(NULL)
    }
  })
  
  output$ci_select_geog_ui <- renderUI({
    if (input$ci_preview_submitButton) {
      selectInput("ci_select_geog",
                  label = "Geography",
                  choices = c("City" = "city",
                              #"TAZ" = "taz",
                              #"FAZ" = "faz",
                              "Regional Geography" = "rgs"),
                  selected = "rgs")
    } else {
      return(NULL)
    }
  })
  
  output$ci_select_ci_ui <- renderUI({
    if (input$ci_preview_submitButton) {
      radioButtons("ci_select_ci",
                   label = h6("Confidence Interval"),
                   choices = list("80%" = 80, "95%" = 95))
    } else {
      return(NULL)
    }
    
  })
  
  output$ci_select_county_ui <- renderUI({
    if (input$ci_preview_submitButton) {
      selectInput("ci_select_county",
                  label = h6("Filter by County"),
                  choices = cnty.choices,
                  selected = "All",
                  multiple = TRUE
      )
    } else {
      return(NULL)
    }
 
  })
  
  output$ci_submitButton_ui <- renderUI({
    if (input$ci_preview_submitButton) {
      actionButton("ci_submitButton",
                   label = "Enter")
    } else {
      return(NULL)
    }
   
  })
  
  ci.data.preview <- eventReactive(input$ci_preview_submitButton, {
    # read runs
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
    return(tbl)
  })
  
  observe({
    if ("All" %in% input$ci_select_county) {
      selected_choices <- setdiff(cnty.choices, "All")
      updateSelectInput(session, "ci_select_county", selected = selected_choices)
    }
  })
  
  # ci.cap.data <- eventReactive(input$ci_submitButton, {
  #   c <- cap.data()
  # 
  #   if (input$ci_select_geog == 'rgs') {
  #     c1 <- c[, lapply(.SD, sum), by = list(fips_rgs_id, attribute), .SDcols = "capacity"]
  #     c2 <- merge(c1, rgs.lu, by = "fips_rgs_id")
  #     c2[, name := fips_rgs_name]
  #   } else if (input$ci_select_geog == 'city') {
  #     c2 <- merge(c, cities.lu, by = c("city_id", "county_id", "city_name"))
  #     c2[, name := city_name]
  #   }
  #   return(c2)
  # })
  # 
  # ci.cap.data.filter <- eventReactive(input$ci_submitButton, {
  #   ci.cap.data()[county_name %in% isolate(input$ci_select_county), ]
  # })
  
  ci.baseyr.data <- eventReactive(input$ci_submitButton, {
    baseyr <- baseyr.data()[geog == input$ci_select_geog, ]
 
    if (input$ci_select_geog == 'rgs') {
      d2 <- merge(baseyr, rgs.lu, by.x = "id", by.y = "fips_rgs_id")
      setnames(d2,"fips_rgs_name","name") 
      d2[, name := factor(name, levels = rgs.lvl)]
    } else if (input$ci_select_geog == 'city') {
      d2 <- merge(baseyr, cities.lu , by.x = "id", by.y = "city_id") 
      setnames(d2,"city_name","name")
      d2[, name := factor(name)]
    }
    return(d2)
  })
  
  ci.baseyr.data.filter <- eventReactive(input$ci_submitButton, {
    ci.baseyr.data()[county_name %in% isolate(input$ci_select_county), ]
  })
  
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
      # d1 <- d[year == input$ci_select_year & geog == input$ci_select_geog]
      d2 <- merge(d1, rgs.lu, by.x = "id", by.y = "fips_rgs_id") 
      setnames(d2,"fips_rgs_name","name") 
      d2[, name := factor(name, levels = rgs.lvl)]
    } else if (input$ci_select_geog == 'city') {
      d1 <- d[year == input$ci_select_year & geog == input$ci_select_geog & cinterval == input$ci_select_ci]
      d2 <- merge(d1, cities.lu , by.x = "id", by.y = "city_id") 
      setnames(d2,"city_name","name")
      d2[, name := factor(name)]
      # d2[, name := factor(name, levels = cities.lvl)]
    } else {
      d2 <- d[year == input$ci_select_year & geog == input$ci_select_geog & cinterval == input$ci_select_ci]
      d2[, name := factor(name)]
    }
    return(d2)
  })
  
  ci.plotdata <- eventReactive(input$ci_submitButton, {
    ci.data <- ci.data()  
    pd <- position_dodge(.9) 

    g <- list()
    i <- 1
    all.indicator.names <- c(indicator.names, "Population")
    ind.names <- all.indicator.names %>% tolower
    for (ind in ind.names) {
      d <-  ci.data[attribute == ind & county_name %in% input$ci_select_county,][, id := as.factor(id)]
      
      g[[ind]] <- ggplot() + 
        geom_point(data = d, 
                   aes(x = name, y = median, colour = cidir), 
                   position = pd, 
                   shape = 21, 
                   fill = "gray90", 
                   size = 1) +
        geom_errorbar(data = d, 
                      aes(x = name, y = median, colour = cidir, ymax=upper, ymin=lower),
                      position = pd, 
                      height = .07) +
        labs(title=all.indicator.names[i], x = "", y = "") +
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
          legend.background = element_rect(fill="white"),
          legend.title = element_blank(),
          text = element_text(family="Segoe UI")
        )
      i <- i + 1
    }
    return(g)
  })
  
  # Regional Growth Strategy Policy Numbers
  policy <- eventReactive(input$ci_submitButton, {
    cols <- grep("[Pop|Emp]\\d{2}$", names(pol.num), value = TRUE)
    cols.vector <- setNames(c("population", "employment"), cols)
    cols2 <- c("fips_rgs_id", "fips_rgs_name", "county_name", "area_name")
    p <- pol.num[, c(cols2, cols), with = FALSE]
    dt <- melt.data.table(p, id.vars = cols2, measure.vars = cols, variable.name = "cols", value.name = "policy_est")
    dt$attribute <- cols.vector[dt$cols]
    dt[, `:=` (label = "RGS Policy Number", name = factor(fips_rgs_name, levels = rgs.lvl))]
    p1 <- dt[county_name %in% isolate(input$ci_select_county), ]
    return(p1)
  })
  
  # City Target Numbers (unofficial)
  policy.city <- eventReactive(input$ci_submitButton, {
    cols <- grep("[Pop|Emp]2050$", names(pol.num.city), value = TRUE)
    cols.vector <- setNames(c("population", "employment"), cols)
    cols2 <- c("RG", "County", "Juris", "county_name")
    p <- pol.num.city[, c(cols2, cols), with = FALSE]
    dt <- melt.data.table(p, id.vars = cols2, measure.vars = cols, variable.name = "cols", value.name = "policy_est")
    dt$attribute <- cols.vector[dt$cols]
    dt[, `:=` (label = "Target Number", name = factor(Juris))]
    p1 <- dt[county_name %in% isolate(input$ci_select_county), ]
    return(p1)
  })
  
  # Switch to appropriate policy number dataset
  policy.df <- eventReactive(input$ci_submitButton, {
    policy <- policy()
    policy.city <- policy.city()
    if (isolate(input$ci_select_geog) == 'rgs') {
      p <- policy
    } else if (isolate(input$ci_select_geog) == 'city') {
      p <- policy.city
    }
    return(p)
  })
  
  ci.ggplot.add.policy <- function(data) {
    p <- data
    geom_point(data = p,
               aes(x = name, y = policy_est, colour = "2050 RGS Policy"),
               fill = "black",
               stroke = 0,
               position = position_dodge(.9),
               shape = 22,
               size = 1.5,
               show.legend = TRUE) 
  }
  
  ci.ggplot.add.baseyr <- function(data) {
    b <- data
    geom_point(data = b,
               aes(x = name, y = baseyr, colour = "2014 Base Year"),
               fill = "grey53",
               stroke = 0,
               position = position_dodge(.9),
               shape = 21,
               size = 1.3,
               show.legend = TRUE)
  } 
  
  ci.ggplot.add.capdata <- function(data) {
    c <- data
    geom_point(data = c,
               aes(x = name, y = capacity, colour = "Total Capacity"),
               # fill = "grey53",
               stroke = .45,
               position = position_dodge(.9),
               shape = 3,
               size = 1.3,
               show.legend = TRUE)
  }
  
  output$ci_plot_hh <- renderPlotly({
    g <- ci.plotdata()
    baseyr <- ci.baseyr.data.filter()
    # cap <- ci.cap.data.filter()
    
    if (isolate(input$ci_select_geog) == 'rgs'|isolate(input$ci_select_geog) == 'city') {
      b <- baseyr[attribute == 'households',]
      # c <- cap[attribute == 'households',]
      
      g2 <- ggplotly(g[['households']] +
                       ci.ggplot.add.baseyr(b) #+
                       # ci.ggplot.add.capdata(c)
                       )
                       
    } else {
      g2 <- ggplotly(g[['households']])
    }
  })
  
  output$ci_plot_emp <- renderPlotly({
    # data <- ci.data()
    g <- ci.plotdata()
    policy.df <- policy.df()
    baseyr <- ci.baseyr.data.filter()
    # cap <- ci.cap.data.filter()
    # browser()

    if (isolate(input$ci_select_geog) == 'rgs'|isolate(input$ci_select_geog) == 'city') {
      b <- baseyr[attribute == 'employment', ]
      p <- policy.df[attribute == 'employment',]
      # c <- cap[attribute == 'employment',]
      
      g2 <- ggplotly(g[['employment']] +
                       ci.ggplot.add.policy(p) +
                       ci.ggplot.add.baseyr(b) #+ 
                       # ci.ggplot.add.capdata(c)
                       )
                       
    } else {
      g2 <- ggplotly(g[['employment']])
    }
  })
  
  output$ci_plot_pop <- renderPlotly({
    g <- ci.plotdata()
    policy.df <- policy.df()
    baseyr <- ci.baseyr.data.filter()
    if (isolate(input$ci_select_geog) == 'rgs' | isolate(input$ci_select_geog) == 'city') {
      b <- baseyr[attribute == 'population', ]
      p <-  policy.df[attribute == 'population',]
      
      g2 <- ggplotly(g[['population']] +
                       ci.ggplot.add.policy(p) +
                       ci.ggplot.add.baseyr(b))
      
    } else {
      g2 <- ggplotly(g[['population']])
    }
  })
  

## CI AAPC -----------------------------------------------------------------

  
  aapc.plot.basic <- function(data, attribute_value) {
    pd <- position_dodge(.9)
    d <- data[attribute == attribute_value]
    g <- ggplot() +
      geom_point(data = d,
                 aes(x = name, y = aapc_median, colour = cidir),
                 position = pd,
                 shape = 21,
                 fill = "gray90",
                 size = 1) +
      geom_linerange(data = d,
                     aes(x = name, y = aapc_median, colour = cidir, ymax = aapc_upper, ymin = aapc_lower),
                     position = pd,
                     height = .00) +
      labs(title=capitalize(attribute_value), x = "", y = "Percent (%)") +
      scale_x_discrete() +
      scale_y_continuous(labels = comma, breaks = pretty_breaks(n=15)) +
      coord_flip()+
      theme(
        legend.key.size = unit(0.012, "npc"),
        plot.title=element_text(size=11, hjust=0, face="bold"),
        axis.title.x = element_text(size=10),
        axis.ticks.length = unit(.45, "cm"),
        axis.ticks.y= element_line(colour = "white"),
        axis.ticks.x= element_line(colour = "white"),
        legend.background = element_rect(fill="white"),
        legend.title = element_blank(),
        text = element_text(family="Segoe UI")
      )
  }

  aapc.plot.plus <- function(data, attribute_value) {
    pd <- position_dodge(.9)
    d <- data[attribute == attribute_value]
    g <- aapc.plot.basic(data, attribute_value) +
    
      geom_point(data = d,
                 aes(x = name, y = aapc_policy, colour = "2050 RGS Policy"),
                 fill = "black",
                 stroke = 0,
                 position = pd,
                 shape = 22,
                 size = 1.3,
                 show.legend = TRUE)
  }

  output$aapc_select_ci_dir_ui <- renderUI({
    selectizeInput("aapc_select_ci_dir",
                   label = HTML("CI Directory<br/><font color = Gray size = 2>Select one or more directories"),
                   choices = bm.runs,
                   multiple = TRUE)
  })

  observe({
    if ("All" %in% input$aapc_select_county) {
      aapc_selected_choices <- setdiff(cnty.choices, "All")
      updateSelectInput(session, "aapc_select_county", selected = aapc_selected_choices)
    }
  })

  a.policy <- eventReactive(input$aapc_submitButton, {
    baseyr <- baseyr.data()
    cols <- grep("[Pop|Emp]\\d{2}$", names(pol.num), value = TRUE)
    cols2 <-  grep("[Pop|Emp]\\d{4}$", names(pol.num.city), value = TRUE)

    reg <- pol.num[, c("fips_rgs_id", cols), with = FALSE][, `:=` (geog = "rgs")]
    dt.rg <- melt.data.table(reg, id.vars = c("fips_rgs_id", "geog"), measure.vars = c(cols), variable.name = "cols", value.name = "policy")
    dr <- dt.rg[, attribute := ifelse(cols == cols[1], "population", "employment")]
    setnames(dr, colnames(dr)[1], "id")

    city <- pol.num.city[, c("CityID", cols2), with = FALSE][, `:=` (geog = "city")]
    dt.city <- melt.data.table(city , id.vars = c("CityID", "geog"), measure.vars = c(cols2), variable.name = "cols", value.name = "policy")
    dc <- dt.city[, attribute := ifelse(cols == cols[1], "population", "employment")]
    setnames(dc, colnames(dc)[1], "id")

    dt <- rbind(dr, dc)
    dt[,.(id, geog, attribute, policy)]
  })

  a.ci.data <- eventReactive(input$aapc_submitButton, {
    runs <- input$aapc_select_ci_dir
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
    return(tbl)
  })

  calc.aapc <- reactive({
    if (is.null(a.ci.data()) | is.null(baseyr.data()) | is.null(input$aapc_select_geog) | is.null(input$aapc_select_year)) return(NULL)
    ci <- a.ci.data()
    baseyr <- baseyr.data()
    pol <- a.policy()
   
    ci <- ci[!(geog %in% c("faz", "zone")), ]
    baseyr <- baseyr[, c(1:4)]

    dt0 <- merge(ci, baseyr, by = c("id", "attribute", "geog"), all.x = TRUE)
    dt <- merge(dt0, pol, by = c("id", "attribute", "geog"), all.x = TRUE)
    dt <- dt[, `:=` (baseyr = ifelse(is.na(baseyr), 0, baseyr), policy = ifelse(is.na(policy ), 0, policy ))]
    stat.cols <- as.vector(outer(c("lower", "upper"), c("80", "95"), paste, sep = "_"))
    cols <- c("median", stat.cols)
    aapc.cols <- paste0("aapc_", cols)
    # check.dt1 <- dt[, (aapc.cols) := lapply(.SD, function(x) round(((x/baseyr)^(1/(as.numeric(year)-2014))-1)*100, 2)), .SDcols = cols
    #           ]
    # browser()
    dt1 <- dt[, (aapc.cols) := lapply(.SD, function(x) round(((x/baseyr)^(1/(as.numeric(year)-2014))-1)*100, 2)), .SDcols = cols
              ][, aapc_policy := round(((policy/baseyr)^(1/(2050-2014))-1)*100, 2)
                ][, c("id", "year", "attribute", "geog", "cidir", aapc.cols, "aapc_policy"), with = FALSE]
    dt2 <- melt.data.table(dt1,
                           id.vars = c("id", "year","attribute", "geog", "cidir", "aapc_median", "aapc_policy"),
                           measure.vars = setdiff(aapc.cols, c("aapc_median", "aapc_policy")),
                           variable.name = "cols",
                           value.name = "aapc")

    dt3 <- dt2[,  `:=` (cinterval = str_extract(cols, "\\d+$"), bound = str_extract(cols, "^aapc_([a-z]*)"))]
    d <- dcast.data.table(dt3, id + year + attribute + geog + cidir  + cinterval + aapc_median + + aapc_policy  ~ bound, value.var = "aapc")
  })

  a.plot.data <- eventReactive(input$aapc_submitButton, {
    if (is.null(calc.aapc())) return(NULL)
    calc.aapc <- calc.aapc()

    d <- calc.aapc[year == input$aapc_select_year & geog == input$aapc_select_geog & cinterval == input$aapc_select_ci,]

    # filter and join lookup table
    if (input$aapc_select_geog == 'rgs') {
      d2 <- merge(d, rgs.lu, by.x = "id", by.y = "fips_rgs_id")
      setnames(d2,"fips_rgs_name","name")
      d2[, name := factor(name, levels = rgs.lvl)]
    } else if (input$aapc_select_geog == 'city'){ #
      d2 <- merge(d, cities.lu , by.x = "id", by.y = "city_id")
      setnames(d2,"city_name","name")
      d2[, name := factor(name)]
    }
    return(d2)
  })

  a.plot.data.filter <- eventReactive(input$aapc_submitButton, {
    a.plot.data()[county_name %in% isolate(input$aapc_select_county),]
  })

  output$aapc_plot_pop <- renderPlotly({
    a <- a.plot.data.filter()
    r <- nrow(a[attribute == "population"]) %>% as.numeric
    p <- aapc.plot.plus(a, "population") +
      geom_segment(aes(y = 1.2, yend = 1.2, x = 0, colour = "Regional AAPC 2014-50"), 
                   xend = r + 1,
                   linetype = 3,
                   show.legend = TRUE)
    ggplotly(p)
  })

  output$aapc_plot_hh <- renderPlotly({
    a <- a.plot.data.filter()
    p <- aapc.plot.basic(a, "households")
    ggplotly(p)
  })

  output$aapc_plot_emp <- renderPlotly({
    a <- a.plot.data.filter()
    r <- nrow(a[attribute == "employment"]) %>% as.numeric
    p <- aapc.plot.plus(a, "employment") +
      geom_segment(aes(y = 1.4, yend = 1.4, x = 0, colour = "Regional AAPC 2014-50"),
                   xend = r + 1,
                   linetype = 3,
                   show.legend = TRUE)
    ggplotly(p)
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