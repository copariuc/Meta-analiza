# Instalarea pachetelor necesare
if(!require(devtools)) install.packages("devtools")
devtools::install_github("nealhaddaway/citationchaser")
devtools::install_github("nealhaddaway/citationchaser", ref="DrMattG-patch-vlookup")
# Vezi si https://estech.shinyapps.io/citationchaser/

# Incarcarea librariilor necesare
library(citationchaser)

### Cautarile Forward si Backward ####
load("Centralizator.Rdata"); load("PRISMA.Rdata")
FwBwSearch <- function(rec = 1) {
  # Se realizeaza cautarea Fw si Bw
  forward <- get_refs(tabel.surse$doi[rec], type = "doi", get_records = "citations",
                      token = "introduceti_aici_tokenul")
  backward <- get_refs(tabel.surse$doi[rec], type = "doi", get_records = "references",
                       token = "introduceti_aici_tokenul")
  return(list(fw = forward$display, bw = backward$display))
}
# Crearea tabelelor de referinte pentru primul articol
rez <- FwBwSearch(rec = 1); fw.search <- rez$fw; bw.search <- rez$bw

# Crearea tabelelor de referinte pentru celelalte articole - ITERATIV
rez <- FwBwSearch(rec = 129); fw.search <- rbind(fw.search, rez$fw, fill=T); bw.search <- rbind(bw.search, rez$bw, fill=T)
# Salvarea surselor FW si BW
save(fw.search, file = "FW Search.Rdata"); save(bw.search, file = "BW Search.Rdata");rm(rez)

# Cautarea inregistrarilor duplicat
fw.gasite <- find_duplicates(data = fw.search,
                          match_variable = "doi",
                          match_function = "exact")
bw.gasite <- find_duplicates(data = bw.search,
                             match_variable = "doi",
                             match_function = "exact")
fw.gasite <- extract_unique_references(fw.search, fw.gasite)
bw.gasite <- extract_unique_references(bw.search, bw.gasite)
fw.search <- fw.gasite[,1:9]; bw.search <- bw.gasite[,1:9]
fw.rezult <- screen_duplicates(x = fw.search)
bw.rezult <- screen_duplicates(x = bw.search)
# Salvarea surselor FW si BW
save(fw.rezult, file = "FW Search.Rdata"); save(bw.rezult, file = "BW Search.Rdata")

### Screeningul articolelor dupa topic
fw.rezult <- screen_topics(x = fw.rezult)
fw.rezult <- fw.rezult$raw %>%
  filter(screened_topics == "selected")
bw.rezult <- screen_topics(x = bw.rezult)
bw.rezult <- bw.rezult$raw %>%
  filter(screened_topics == "selected")
# Salvarea surselor FW si BW
save(fw.rezult, file = "FW Search.Rdata"); save(bw.rezult, file = "BW Search.Rdata")

### Unirea tabelelor centralizatoare si salvarea datelor
# tabel.surse <- rbind(tabel.surse, search.result)
PRISMA.template$n[which(PRISMA.template$data == "records_screened")] <- nrow(tabel.surse)
save(tabel.surse, file = "Centralizator.Rdata")
save(PRISMA.template, file = "PRISMA.Rdata")

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
