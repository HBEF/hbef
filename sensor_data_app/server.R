shinyServer(function(input, output, session) {

    #load and configure data
    sensor_data <- reactive({
        var <- input$SENSORVAR2
        site <- input$SITES2
        grab_add <- input$GRAB_ADD2
        grab_var <- input$SOLUTES2
        date <- c(ymd('2000-01-01'), ymd('2030-01-01'))

        sensor_data <- get_sensor_data(var, site, date)

        #add grab sample overlay 
        if(!is.null(grab_add) && grab_add == "Overlay grab samples"){

            new_name <- paste0(grab_var, '_g')
            join <- dataCurrent %>%
                filter(site == !!site) %>%
                mutate(datetime = ymd_hms(paste(date, timeEST, sep = ' '))) %>%
                select(datetime, !!grab_var) %>%
                rename(!!new_name := !!grab_var)

            sensor_data <- full_join(sensor_data, join, by = 'datetime')

        }
            return(sensor_data)
    })
    
    # sensor_data <- sensor_data %>%
    #   debounce(100)

    #Create plot title
    output$TITLE4 <- renderText({
        
        var <- input$SENSORVAR2
        site <- input$SITES2
        water_or_date <- input$wateryearOrRange2
        water_year <- input$WATERYEAR2
        date <- input$DATE2 
        date <- round_date_first(date)
        
        
        if(water_or_date == 'wateryr'){
            title <- glue('{v} for site {w} in water year {y}',
                          v = var, 
                          w = site,
                          y = water_year)
        } else{
            title <- glue('{v} for site {w}',
                          v = var,
                          w = site)
        }
        
        return(title)
    })

    #select the same grab sample var as sensor var as default  
    observe({
        input$GRAB_ADD2
        sensor_var <- isolate(input$SENSORVAR2)
        
        grab_select <- sensor_grab_map[names(sensor_grab_map) == sensor_var]
        
        if(!length(grab_select) == 0){
            updateSelectInput(session = session, inputId = 'SOLUTES2', selected = grab_select)
        }
        
    })

    #Create main graph 
    output$GRAPGH4 <- renderDygraph({

        date <- input$DATE2
        water_or_date <- input$wateryearOrRange2
        water_year <- input$WATERYEAR2
        grab_var <- isolate(input$SOLUTES2)
        var <- isolate(input$SENSORVAR2)
        grab_add <- isolate(input$GRAB_ADD2)
        sensor_data <- sensor_data()
        ymin <- input$YLIMlo2 
        ymax <- input$YLIMhi2
        add_line <- input$OPTIONS2

        if(!is.null(add_line) && add_line == 'Add line to sensor data'){
            line_width <- NULL
            points <- FALSE
        } else{
            points <- TRUE
            line_width <- 0
        }

        #Round dates from slider bar to the first of the month 
        date <- round_date_first(date)

      #Create water year dates 
      if(water_or_date == 'wateryr'){
          yrstart = as.POSIXct(paste0(water_year, '-06-01'))
          yrend = as.POSIXct(paste0(as.numeric(water_year) + 1, '-05-31'))
          date = c(yrstart, yrend)
          xlabel <- paste("Water Year ", water_year)
      } else {
          dates = date
          xlabel = paste("Dates ", date[1], " to ", date[2])
      }

      #Filter sensor data to selected dates 
      sensor_data <- sensor_data %>%
          filter(datetime >= date[1],
                 datetime <= date[2])
      
      #Create empty graph when no data is available
      if(nrow(sensor_data) == 0){
        
          datelims = as.POSIXct(date)
          dateseq = seq(datelims[1], datelims[2], by='day')
          emptydat = xts(rep(0, length.out=length(dateseq)),
                         order.by=dateseq, tzone='UTC')
          dg = dygraph(emptydat, group='group2') %>%
              dyOptions(useDataTimezone=TRUE, 
                        drawPoints=FALSE,
                        colors='transparent', 
                        retainDateWindow=TRUE,
                        drawGrid = FALSE) %>%
              dyAxis('y', label='', labelWidth=16, labelHeight=10, rangePad=10)
        
      } else{

          #Create overlay with grab samples
          if(!is.null(grab_add) && grab_add == "Overlay grab samples"){

              #Pad dataset to selected dates 
              datebounds = as.POSIXct(date)
              row1 = data.frame(datebounds[1], NA, NA)
              colnames(row1) = colnames(sensor_data)
              rown = data.frame(datebounds[1], NA, NA)
              colnames(rown) = colnames(sensor_data)
              df_padded = rbind(row1, sensor_data, rown)

              sensor_data_xts <- xts(df_padded[,-1], order.by = df_padded$datetime)

              xlabel = paste("Dates ", date[1], " to ", date[2])
              new_name <- paste0(grab_var, '_g')
              
              #Assemble y lable 
              sensor_name <- unname(sensor_name_map[names(sensor_name_map) == var])
              sensor_unit <- unname(sensor_unit_map[names(sensor_name_map) == var])
              
              if(sensor_unit == ''){
                  y_lab_name <- glue('{n} and {gv} grab',
                                     n = sensor_name,
                                     gv = grab_var)
              } else{
                  y_lab_name <- glue('{n} ({u}) and {gv} grab',
                                   n = sensor_name,
                                   u = sensor_unit,
                                   gv = grab_var)
              }
              grab_lable <- paste(grab_var, ' (grab)')

              dg <- dygraph(sensor_data_xts, group='group2') %>%
                  dySeries(new_name, drawPoints = TRUE, label = grab_lable, pointSize = 3) %>%
                  dySeries(var, drawPoints = points, label = sensor_name, strokeWidth=line_width) %>%
                  dyAxis("x", label = xlabel, valueRange = date) %>%
                  dyAxis("y", label = y_lab_name, independentTicks=TRUE, valueRange = c(ymin, ymax)) %>%
                  dyOptions(drawGrid = FALSE,
                            connectSeparatedPoints=TRUE,
                            includeZero = TRUE,
                            colors = c('#800080', '#000000'))
          } else{

            #Just sensor samples 
            sensor_data <- pad_ts(sensor_data, date)

            sensor_data_xts <- xts(sensor_data[,-1], order.by = sensor_data$datetime)
            
            names(sensor_data_xts) <- var

            xlabel = paste("Dates ", date[1], " to ", date[2])
            
            #Assemble y lable 
            sensor_name <- unname(sensor_name_map[names(sensor_name_map) == var])
            sensor_unit <- unname(sensor_unit_map[names(sensor_name_map) == var])
            
            if(sensor_unit == ''){
              y_lab_name <- glue('{n}',
                                 n = sensor_name)
            } else{
              y_lab_name <- glue('{n} ({u})',
                                 n = sensor_name,
                                 u = sensor_unit)
            }

            dg <- dygraph(sensor_data_xts, group='group2') %>%
                dySeries(var, drawPoints = points, label = sensor_name, strokeWidth = line_width) %>%
                dyAxis("x", label = xlabel, valueRange = date) %>%
                dyAxis("y", label = y_lab_name, independentTicks=TRUE, valueRange = c(ymin, ymax)) %>%
                dyOptions(drawGrid = FALSE,
                          connectSeparatedPoints = TRUE,
                          includeZero = TRUE,
                          colors = 'black')
          }
      }
    })
})
