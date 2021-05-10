report_tab = tabPanel('What is HBWatER', value='report',
    br(),
    div(style="text-align:center",
        HTML('<a href="watershed_report_full.pdf" class="btn btn-primary" role="button">View Full Report</a>')
    ),
    br(),
    div(style="width: 100%; height: 100%",
        HTML(paste0('<embed style="min-height:100vh" width="100%" ',
                    'src="watershed_report_page1.pdf#toolbar=0&navpanes=0&scrollbar=0""></embed>'))
    )
)
