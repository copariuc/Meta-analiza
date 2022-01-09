
load("Centralizator.Rdata"); load("PRISMA.Rdata")
# Cautarea inregistrarilor duplicat
gasite <- find_duplicates(data = tabel.surse,
                          match_variable = "doi",
                          match_function = "exact")
tabel.surse <- extract_unique_references(tabel.surse, gasite)
tabel.surse <- screen_duplicates(x = tabel.surse)
# Screeningul articolelor dupa rezumat
rezult <- screen_abstracts(x = tabel.surse)
#rezult <- screen_abstracts(x = rezult)
temp <- rezult %>%
  filter(screened_abstracts == "selected")
del.abstract <- nrow(rezult) - nrow(temp); tabel.surse <- temp

# Salvarea datelor finale
PRISMA.template$n[which(PRISMA.template$data == "records_excluded")] <- del.abstract
PRISMA.template$n[which(PRISMA.template$data == "dbr_sought_reports")] <- nrow(temp)
save(tabel.surse, file = "Centralizator.Rdata")
save(PRISMA.template, file = "PRISMA.Rdata")
write_bibliography(tabel.surse, filename = "Final DB.bib", format = "bib")
# Desenarea si afisarea diagramei
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
