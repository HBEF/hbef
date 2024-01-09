#in case we ever end up using the SP and SW sites, which are included with
#the core hbef sites in the field and lab notes that Tammy uploads...
#i dunno, the reason this was made is now pretty irrelevant

library(tidyverse)

setwd('/home/mike/git/hbef/shiny/restricted_QAQC/field_and_lab_note_collections')
fs = list.files()
dir.create('~/Desktop/aa')

for(f in fs){
    d = readxl::read_xlsx(f, sheet = 3, col_names = FALSE, col_types = 'text',
                          .name_repair = "unique_quiet") %>%
        as.matrix()

    d_flow_ = d[18:32, c(1:4, 6:10)] %>%
        as_tibble() %>%
        rename(site = 1, date = 2, pHmetrohm = 3, ANCMet = 4, pH = 5,
               spCond = 6, fieldCode = 7, notes = 8, archived = 9) %>%
        filter(if_any(everything(), ~! is.na(.)))

    cat(f)
    cat('\n')
    cat(grep('S[PW]', d_flow_$site, value = T), sep = '\n')
    readLines(n=1)
}
