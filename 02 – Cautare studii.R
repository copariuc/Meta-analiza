# Instalarea pachetelor necesare
if(!require(devtools)) install.packages("devtools")
devtools::install_github("nealhaddaway/citationchaser")
devtools::install_github("nealhaddaway/citationchaser", ref="DrMattG-patch-vlookup")
# Vezi si https://estech.shinyapps.io/citationchaser/

# Incarcarea librariilor necesare
library(citationchaser)

# Cautarile Forward si Backward si includerea articolelor ####
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
rez <- FwBwSearch(rec = 2); fw.search <- rbind(fw.search, rez$fw, fill=T); bw.search <- rbind(bw.search, rez$bw, fill=T)
# Salvarea surselor FW si BW
total.fw <- nrow(fw.search); total.bw <- nrow(bw.search)
PRISMA.template$n[which(PRISMA.template$data == "database_results")] <- total.db + total.bw + total.fw
save(fw.search, file = "FW Search.Rdata"); save(bw.search, file = "BW Search.Rdata");rm(rez)

# Cautarea inregistrarilor duplicat ####
fw.gasite <- find_duplicates(data = fw.search,
                          match_variable = "doi",
                          match_function = "exact")
bw.gasite <- find_duplicates(data = bw.search,
                             match_variable = "doi",
                             match_function = "exact")
fw.gasite <- extract_unique_references(fw.search, fw.gasite)
fw.duplicate <- sum(fw.gasite$n_duplicates) - nrow(fw.gasite)
bw.gasite <- extract_unique_references(bw.search, bw.gasite)
bw.duplicate <- sum(bw.gasite$n_duplicates) - nrow(bw.gasite)
# Scanare suplimentara a duplicatelor si generarea tabelului de analiza
fw.rezult <- screen_duplicates(x = fw.gasite)
bw.rezult <- screen_duplicates(x = bw.gasite)
# Salvarea surselor FW si BW
fw.duplicate <- fw.duplicate + nrow(fw.gasite) - nrow(fw.rezult)
bw.duplicate <- bw.duplicate + nrow(bw.gasite) - nrow(bw.rezult)
PRISMA.template$n[which(PRISMA.template$data == "duplicates")] <- duplicate + fw.duplicate + bw.duplicate
save(fw.rezult, file = "FW Search.Rdata"); save(bw.rezult, file = "BW Search.Rdata")

# Screeningul articolelor dupa topic ####
fw.rezult <- screen_topics(x = fw.rezult); temp <- fw.rezult
fw.rezult <- fw.rezult$raw %>%
  filter(screened_topics == "selected")
fw.del.topics <- nrow(temp) - nrow(fw.rezult$raw)
bw.rezult <- screen_topics(x = bw.rezult); temp <- bw.rezult
bw.rezult <- bw.rezult$raw %>%
  filter(screened_topics == "selected")
bw.del.topics <- nrow(temp) - nrow(bw.rezult$raw)
# Salvarea surselor FW si BW
PRISMA.template$n[which(PRISMA.template$data == "excluded_automatic")] <- del.topics + fw.del.topics + bw.del.topics
save(fw.rezult, file = "FW Search.Rdata"); save(bw.rezult, file = "BW Search.Rdata")

# Screeningul articolelor dupa titlu ####
fw.rezult <- screen_titles(x = fw.rezult); temp <- fw.rezult
fw.rezult <- fw.rezult %>%
  filter(screened_titles == "selected")
fw.del.title <- nrow(temp) - nrow(fw.rezult$raw)
bw.rezult <- screen_titles(x = bw.rezult); temp <- bw.rezult
bw.rezult <- bw.rezult %>%
  filter(screened_titles == "selected")
bw.del.title <- nrow(temp) - nrow(bw.rezult$raw)
# Salvarea surselor FW si BW
PRISMA.template$n[which(PRISMA.template$data == "excluded_other")] <- del.titles + fw.del.title + bw.del.title
save(fw.rezult, file = "FW Search.Rdata"); save(bw.rezult, file = "BW Search.Rdata")

### Unirea tabelelor centralizatoare si salvarea datelor
tabel.surse <- rbind(tabel.surse, fw.rezult, bw.rezult)
PRISMA.template$n[which(PRISMA.template$data == "records_screened")] <- nrow(tabel.surse)
save(tabel.surse, file = "Centralizator.Rdata"); save(PRISMA.template, file = "PRISMA.Rdata")
