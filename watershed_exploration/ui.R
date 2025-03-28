
source("helpers.R")
source('ui/nSiteNVar_ui.R')
source('ui/report_ui.R')
source('ui/map_ui.R')


ui = fluidPage(
# shinyUI(fluidPage(

    #screen shouldn't go gray when plots are updating.
    # tags$style(type="text/css", ".recalculating { opacity: 1.0; }" ),
    tags$head(tags$style(HTML(
        "@import url('https://fonts.googleapis.com/css?family=Montserrat');"))),
    tags$head(tags$link(rel="stylesheet", type="text/css", href="app.css")),
    # useShinyjs(),
    # extendShinyjs(script = 'js/general.js',
    #               functions = c()),

    dashboardPage(
        dashboardHeader(disable=TRUE),
        dashboardSidebar(width='40%',
            div(class='sidebar-sub',
                # HTML('<input type="text" id="MAPDATA" style="display: none">'),
                tabsetPanel(id='left_tabs',
                    map_tab,
                    report_tab
                )
            ),
            div(style='width: 36px; display: inline-block; float: right',
                actionLink('COLLAPSE_SIDEBAR', label='', icon=icon('arrows-h'),
                    class='sidebar-toggle', `data-toggle`='offcanvas',
                    style='margin: 6px')
            )
        ),
        dashboardBody(
            useShinyjs(),
            extendShinyjs(script = 'js/general.js',
                          functions = c()),
            # tags$head(
            #     tags$link(rel='stylesheet', type='text/css', href='style.css')
            # ),
            tabsetPanel(id='right_tabs',
                # summary_biplot_tab,
                # oneSiteNVar_tab,
                nSiteNVar_tab
                # site_comparison_tab
            )
        )
    )
)
# ))
