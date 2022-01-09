# Instalarea pachetelor necesare
if(!require(bib2df)) install.packages("bib2df")
if(!require(dplyr)) install.packages("dplyr")
if(!require(flextable)) install.packages("flextable")
if(!require(writexl)) install.packages("writexl")
if(!require(revtools)) install.packages("revtools")
if(!require(PRISMA2020)) install.packages("PRISMA2020")

# Incarcarea librariilor necesare
library(bib2df); library(dplyr); library(PRISMA2020)
library(revtools); library(flextable); library(writexl)

### Construirea si aranjarea tabelului centralizator ####
tabel.surse <- data.frame(ID = NA, Type = NA, Authors = NA, Year = NA, Title = NA,
                          Journal = NA, Abstract = NA, Keywords = NA, Search = NA, 
                          Disp = NA, Analyst = NA, DOI = NA)
temp <- bib2df("Elsevier DB.bib")
temp <- bib2df("Wiley DB.bib")
temp <- bib2df("WoS DB.bib")
temp <- bib2df("Scopus DB.bib")

# Extragerea datelor - ITERATIV
rec <- 1
while(rec <= nrow(temp)) {
  sursa <- temp %>%
    filter(BIBTEXKEY == temp$BIBTEXKEY[rec])
  id <- sursa$BIBTEXKEY
  type <- sursa$CATEGORY
  authors <- paste(unlist(sursa$AUTHOR), collapse="; ")
  year <- sursa$YEAR
  title <- gsub("[{|}]", "", sursa$TITLE)
  journal <- sursa$JOURNAL
  abstract <- sursa$ABSTRACT
  keywords <- sursa$KEYWORDS
  #db <- paste("Elsevier \n", date())
  #db <- paste("Wiley \n", date())
  #db <- paste("WoS \n", date())
  db <- paste("Scopus \n", date());  keywords <- sursa$KEY
  dispos <- "NO"
  analist <- paste("COD \n", date())
  doi <- sursa$DOI
  # Actualizarea tabelului centralizator - ITERATIV
  tabel.surse <- rbind(tabel.surse, 
                       c(id, type, authors, year, title, journal,  
                         abstract, keywords, db, dispos, analist, doi))
  rec <- rec + 1   # Trecerea la urmatoarea sursa
}

# Stergerea primei inregistrari si a obiectelor inutile - LA FINAL
if (is.na(tabel.surse[1,])) tabel.surse <- tabel.surse[-1,]
rm(sursa, temp, abstract, analist, authors, db, dispos, doi, id, journal,
   keywords, rec, title, type, year)
save(tabel.surse, file = "Centralizator.Rdata")

### Crearea structurii PRISMA ####
PRISMA.template <- read.csv(system.file("extdata", "PRISMA.csv", package = "PRISMA2020"))
# Incarcarea informatiilor in sablonul PRISMA - TOTALUL SURSELOR
PRISMA.template <- PRISMA.template %>%
  mutate(boxtext = case_when(data == "identification"~"Identification", T~boxtext))
PRISMA.template$n[which(PRISMA.template$data == "database_results")] <- nrow(tabel.surse)

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

### Cautarea inregistrarilor duplicat ####
gasite <- find_duplicates(data = tabel.surse,
                          match_variable = "DOI",
                          match_function = "exact")
gasite <- extract_unique_references(tabel.surse, gasite)
duplicate <- sum(gasite$n_duplicates) - nrow(gasite); tabel.surse <- gasite
colnames(tabel.surse) <- c(names(tabel.surse[1:12]), "Duplicates")
# Scanare suplimentara a duplicatelor
rezult <- screen_duplicates(x = tabel.surse)
duplicate <- duplicate + nrow(tabel.surse) - nrow(rezult)
PRISMA.template$n[which(PRISMA.template$data == "duplicates")] <- duplicate

### Screeningul articolelor dupa topic ####
rezult <- screen_topics(x = rezult)
temp <- rezult$raw %>%
  filter(screened_topics == "selected")
del.topics <- nrow(rezult$raw) - nrow(temp)
PRISMA.template$n[which(PRISMA.template$data == "excluded_automatic")] <- del.topics

### Screeningul articolelor dupa titlu ####
rezult <- screen_titles(x = temp)
temp <- rezult %>%
  filter(screened_titles == "selected")
del.titles <- nrow(rezult) - nrow(temp)
PRISMA.template$n[which(PRISMA.template$data == "excluded_other")] <- del.titles

# Salvarea fisierului Excel si HTML
tabel.surse <- temp %>%
  select( -screened_topics, - screened_titles, -duplicates, -matches, -display, -topic)
write_xlsx(tabel.surse, path = "Centralizator.xlsx")
tabel <- flextable(data = tabel.surse) %>% theme_box
tabel
tabel %>% save_as_html(path = "Centralizator.html")
save(tabel.surse, file = "Centralizator.Rdata")
save(PRISMA.template, file = "PRISMA.Rdata")
rm(gasite, rezult, tabel, temp, del.titles, del.topics, duplicate)
