suppressPackageStartupMessages({
    library(V8)
    library(feather)
    library(plyr)
    library(data.table)
    # library(dtplyr)
    library(shiny)
    library(shinydashboard)
    library(plotly)
    library(dygraphs)
    # library(DBI)
    library(ggthemes)
    # library(ggplot2)
    # library(colorspace)
    library(jsonlite)
    library(lubridate)
    library(xts)
    library(leaflet)
    library(tidyverse)
    library(glue)
    library(shinyjs)
    # library(googlesheets4)
    library(DT)
    library(errors)
    # library(rhandsontable)
    # library(shiny.router)
})

# boundary_file_loc = '~/git/hbef/hbef_misc/macrosheds_hbef/data/general/shed_boundary/shed_boundary.shp'
# x = sf::st_read(boundary_file_loc)
# x = filter(x, site_code %in% paste0('w', 1:9))
# sf::st_write(x, boundary_file_loc)

#TODO:
#most of the variables created in this script can be automatically generated.
    #those that can't should be read from a config file or spreadsheet eventually.
#attend to trailing comments within this script

## 0. setup ####

# conf <- jsonlite::fromJSON('config.json')

# #uncomment and run this (without saving script) to allow pushing of app to our shinyapps account
# rsconnect::setAccountInfo(name = 'cuahsi',
#                           token = conf$shinyapps_cuahsi_token,
#                           secret = conf$shinyapps_cuahsi_secret)
#
# #uncomment and run this (without saving script) to deploy app
# rsconnect::deployApp('/home/mike/git/macrosheds/portal',
#                      appName = 'macrosheds',
#                      account = 'cuahsi')

#options(dplyr.summarise.inform = FALSE)

#for local testing (comment all before pushing live)
# setwd('~/git/macrosheds/portal')
# setwd('~/desktop/macrosheds/portal')
# options(shiny.trace = TRUE) #see every communication between ui and server
# options(shiny.reactlog = TRUE) #see map of reactivity by running reactlogShow()
# options(shiny.error='recover') #enter debugger when error occurs
# options(shiny.fullstacktrace=TRUE) #see stack traces for all errors (incl. dplyr)
# options(shiny.sanitize.errors = TRUE) #hide errors in the app

source('helpers.R') #maybe package these or put them in a namespace called "ms"
source('function_aliases.R')

#load global datasets
# googlesheets4::gs4_auth(path = '../data_acquisition/googlesheet_service_accnt.json')
#                         use_oob = TRUE)
load_portal_config(from_where = 'local')

sites_with_Q <- sm(read_csv('data/general/sites_with_discharge.csv')) %>%
    select(-network) %>%
    tidyr::unite(col = 'nds',
                 domain, site_code,
                 remove = TRUE) %>%
    pull(nds)

site_data <- filter(site_data,
                    as.logical(in_workflow),
                    paste(domain, site_code, sep = '_') %in% sites_with_Q |
                        site_type == 'rain_gauge',
                    domain == 'hbef')

#TODO: allow duplicate site_codes
# if(any(duplicated(site_data$site_code))) stop('site_codes must be unique, even across domains')
## 1. nSiteNVar page setup ####

#establish color scheme for nSiteNVar plots
linecolors <- c('#1f49c7', '#006600', '#4d0099') #from royalblue (4169e1), green, purple
pchemcolors <- c('#7b97ea', '#00b300', '#8000ff') #lighter shades of linecolors

## 2. populate nSiteNVar defaults, which determine data shown when user lands ####

default_network <- 'lter'
default_domain <- 'hbef'
network_domain_default_sites <- site_data %>%
    group_by(network, domain) %>%
    summarize(site_code = first(site_code),
              pretty_domain = first(pretty_domain),
              pretty_network = first(pretty_network),
              .groups = 'drop') %>%
    select(pretty_network, network, pretty_domain, domain,
           default_site = site_code)

default_sitelist <- get_sitelist(domain = default_domain,
                                 # network = default_network, #TODO: observe network level within portal?
                                 type = c('stream_gauge', 'stream_sampling_point'))

default_site <- get_default_site(domain = default_domain)
                                 # network = default_network)

basedata <- list(
    Q = ms_read_portalsite(domain = default_domain,
                           site_code = default_site,
                           prodname = 'discharge'),
    chem = ms_read_portalsite(domain = default_domain,
                              site_code = default_site,
                              prodname = 'stream_chemistry'),
    # flux = ms_read_portalsite(domain = default_domain,
    #                           site_code = default_site,
    #                           prodname = 'stream_flux_inst_scaled'),
    P = ms_read_portalsite(domain = default_domain,
                           site_code = default_site,
                           prodname = 'precipitation')
    # pchem = ms_read_portalsite(domain = default_domain,
    #                            site_code = default_site,
    #                            prodname = 'precip_chemistry')
    # pflux = ms_read_portalsite(domain = default_domain,
    #                            site_code = default_site,
    #                            prodname = 'precip_flux_inst_scaled')
)

#date range for date selector
dtrng <- as.Date(range(basedata$chem$datetime,
                       na.rm = TRUE))

## 3. populate nSiteNVar options for all selection widgets ####

domains_pretty <- network_domain_default_sites$domain
names(domains_pretty) <- network_domain_default_sites$pretty_domain

fluxvars <- variables %>%
    filter(as.logical(flux_convertible)) %>%
    pull(variable_code)

chemvars <- filter(variables,
                   variable_type %in% c('chem_discrete', 'chem_mix', 'gas'))
    # filter(variable_code %in% fluxvars) #might need this back temporarily

chemvars_display <- generate_dropdown_varlist(chemvars)

pchemvars = list( #TODO: program this list. dig into pchem files by domain and
                  #extract all available variable names. pchemvars_display should
                  #only reflect the available vars for the sites that are selected
    hbef=c('pH', 'spCond', 'Ca', 'Mg', 'K', 'Na', 'TMAl', 'OMAl', 'Al_ICP',
        'NH4', 'SO4', 'NO3', 'Cl', 'PO4', 'DOC', 'TDN', 'DON', 'SiO2', 'Mn', 'Fe',
        'F', 'cationCharge', 'anionCharge', 'theoryCond', 'ionError', 'ionBalance'),
    hjandrews=c('alk', 'Ca', 'Cl', 'spCond', 'DOC', 'K', 'Mg', 'Na', 'NH3_N',
        'NO3_N', 'pH', 'PO4_P', 'SiO2', 'SO4_S', 'suspSed', 'TDN', 'TDP', 'TKN',
        'UTKN', 'UTN', 'UTP'))

pchemvars_display <- generate_dropdown_varlist(chemvars,
                                               filter_set = Reduce(union,
                                                                   pchemvars))

conc_vars <- variables %>%
    filter(variable_type %in% c('chem_discrete', 'gas')) %>% #TODO: allow the 4 gas variables to be displayed in ppx OR x/L, xM, xeq
    pull(variable_code)

#these are the available selections for the unit conversion menus
conc_units <- c('ng/L', '\u03BCg/L'='ug/L', 'mg/L', 'g/L', 'nM', '\u03BCM'='uM', 'mM', 'M',
                'neq/L', '\u03BCeq/L'='ueq/L', 'meq/L', 'eq/L') #TODO: add ppt, ppm, ppb to this list (see TODO above)
flux_units <- c('Mg/ha/d', 'kg/ha/d', 'g/ha/d', 'mg/ha/d')

#map conc/flux display options to internal IDs for conc/flux metrics
conc_flux_names <- c('Concentration' = 'Concentration',
                     # '_x' = 'Flux',
                     'Flux' = 'Flux',
                     'VWC' = 'VWC')
# '_y' = 'VWC')

# names(conc_flux_names)[2] <- paste('Flux (interpolated)',
#                                    enc2native('\U2753'))
# names(conc_flux_names)[3] <- paste('Flux (VWC)',
# names(conc_flux_names)[3] <- paste('Volume-Weighted Concentration',
#                                    enc2native('\U2753'))

# sites_with_P <- sites_by_var('precipitation')
# sites_with_Q <- sites_by_var('discharge')
# sites_with_pchem <- sites_by_var('precip_chemistry')

chemvars_display_subset <- filter_dropdown_varlist(basedata$chem)
#pchemvars_display_subset <- filter_dropdown_varlist(basedata$pchem)

## 4. biplot page setup ####

biplot_options <- chemvars_display_subset

biplot_data_types <- c('Stream Chemistry', 'Stream Chemistry Flux', 'Discharge',
                       'Watershed Characteristics', 'Precipitation',
                       'Precipitation Chemistry', 'Precipitation Chemistry Flux')

flux_units_bi <- c('Mg/ha/d', 'kg/ha/d', 'g/ha/d', 'mg/ha/d',
                   'Mg/ha/year', 'kg/ha/year','g/ha/year', 'mg/ha/year'
                   #'Mg/year', 'kg/year', 'g/year', 'mg/year'
)

conc_units_bi <- c('ng/L', 'ug/L', 'mg/L', 'g/L')

discharge_units_bi <- c('mm/year', 'm^3', 'mm/d')

ws_trait_types <- variables %>%
    filter(variable_type == 'ws_char') %>%
    pull(variable_subtype) %>%
    unique()

ws_traits <- generate_dropdown_varlist_ws(variables)

ws_traits_names <- unlist(ws_traits)

