load("Centralizator.Rdata"); load("PRISMA.Rdata")

# Screeningul articolelor dupa rezumat ####
rezult <- screen_abstracts(x = tabel.surse)
#rezult <- screen_abstracts(x = rezult)
temp <- rezult %>%
  filter(screened_abstracts == "selected")
del.abstract <- nrow(rezult) - nrow(temp); tabel.surse <- temp

# Salvarea datelor finale ####
PRISMA.template$n[which(PRISMA.template$data == "records_excluded")] <- del.abstract
PRISMA.template$n[which(PRISMA.template$data == "dbr_sought_reports")] <- nrow(temp)
tabel.surse <- read_bibliography("Final DB.bib", return_df = T) # Preventie sa nu suprascriem baza de lucru
save(tabel.surse, file = "Centralizator.Rdata"); save(PRISMA.template, file = "PRISMA.Rdata")
write_bibliography(tabel.surse, filename = "Final DB.bib", format = "bib")

# Preventie PRISMA sa sincronizam informatiile
PRISMA.template$n[which(PRISMA.template$data == "database_results")] <- 6874
PRISMA.template$n[which(PRISMA.template$data == "register_results")] <- 345
PRISMA.template$n[which(PRISMA.template$data == "duplicates")] <- 2539
PRISMA.template$n[which(PRISMA.template$data == "excluded_automatic")] <- 4511
PRISMA.template$n[which(PRISMA.template$data == "excluded_other")] <- 85
PRISMA.template$n[which(PRISMA.template$data == "records_screened")] <- 84
PRISMA.template$n[which(PRISMA.template$data == "records_excluded")] <- 46
PRISMA.template$n[which(PRISMA.template$data == "dbr_sought_reports")] <- 38
PRISMA.template$n[which(PRISMA.template$data == "dbr_notretrieved_reports")] <- 8
PRISMA.template$n[which(PRISMA.template$data == "dbr_assessed")] <- 30

# Desenarea si afisarea diagramei PRISMA ####
PRISMA <- PRISMA_flowdiagram(PRISMA_data(PRISMA.template),
                             interactive = T, previous = F, other = F,
                             fontsize = 10, font = "Arial",
                             title_colour = "DarkOrange",         # Culoarea titului sectiunii - Baze de date
                             greybox_colour = "DarkOliveGreen",   # Culoarea intregii sectiuni - Alte surse
                             #main_colour = "Red",                # Culoarea bordurilor - Baze de date
                             arrow_colour = "SteelBlue",          # Culoarea sagetii
                             arrow_head = "vee",                  # Tipul varfului sagetii
                             #arrow_tail = "none",                # Tipul cozii sagetii
                             side_boxes = T)
PRISMA; PRISMA_save(PRISMA, overwrite = T, filename = "PRISMA.png", filetype = "PNG")

