print("Source 01_read_acorn_data.R")

# Read in ACORN data dictionary ----
print("Start to read data dictionnary")

path_data_dictionary_file <- input$file_data_dic[[1, 'datapath']]
data_dictionary <- list()
data_dictionary$variables <- read_excel(path_data_dictionary_file, sheet = "variables")
data_dictionary$test.res <- read_excel(path_data_dictionary_file, sheet = "test.results")
data_dictionary$local.spec <- read_excel(path_data_dictionary_file, sheet = "spec.types")
data_dictionary$local.orgs <- read_excel(path_data_dictionary_file, sheet = "organisms")

print("Data dictionnary read")



# Read in ACORN lab data ----
print("Start to read lab data")

path_lab_file <- input$file_lab_data[[1, 'datapath']]
extension_file_lab_data <- tools::file_ext(path_lab_file)

if (extension_file_lab_data == "dbf" | extension_file_lab_data == "DBF")  amr.loc <- foreign::read.dbf(path_lab_file, as.is = TRUE)
if (extension_file_lab_data == "sqlite") {
  dta <- DBI::dbConnect(RSQLite::SQLite(), path_lab_file)
  amr.loc <- as.data.frame(dbReadTable(dta, "Isolates"))
}
  
if (extension_file_lab_data == "csv")  amr.loc <- read_csv(path_lab_file, guess_max = 10000)
if (extension_file_lab_data == "txt")  amr.loc <- read_tsv(path_lab_file, guess_max = 10000)
if (extension_file_lab_data %in% c("xls", "xlsx")) amr.loc <- read_excel(path_lab_file, guess_max = 10000)
print("Lab data read")


# Read in lab code and AST breakpoint data ----
print("Start to read lab codes")

path_lab_code_file <- input$file_lab_codes[[1, 'datapath']]
read_lab_code <- function(sheet) read_excel(path_lab_code_file, sheet = sheet, 
                                            col_types = c("text", "text", "text", "text", "text", "numeric", "numeric", "text", "text"), na = "NA")

lab_code <- list(
  whonet.spec = read_excel(path_lab_code_file, sheet = "spectypes.whonet"),
  orgs.antibio = read_excel(path_lab_code_file, sheet = "orgs.antibio"),
  whonet.orgs = read_excel(path_lab_code_file, sheet = "orgs.whonet"),
  acorn.ast.groups = read_excel(path_lab_code_file, sheet = "acorn.ast.groups"),
  ast.aci = read_lab_code(sheet = "aci"),  # Gram negatives - Acinetobacter
  ast.col = read_lab_code(sheet = "col"),   # Enterobacteriaceae (all)
  ast.hin = read_lab_code(sheet = "hin"),  # Haemophilus influenzae
  ast.ngo = read_lab_code(sheet = "ngo"),  # Neisseria gonorrhoeae
  ast.nmen = read_lab_code(sheet = "nmen"),  # Neisseria meningitidis
  ast.pae = read_lab_code(sheet = "pae"),  # Pseudomonas aeruginosa
  ast.sal = read_lab_code(sheet = "sal"),  # Salmonella sp (all)
  ast.shi = read_lab_code(sheet = "shi"),  # Shigella sp
  ast.ent = read_lab_code(sheet = "ent"),  # Gram positives - Enterococcus sp (all)
  ast.sau = read_lab_code(sheet = "sau"),  # Staphylococcus aureus
  ast.spn = read_lab_code(sheet = "spn"),  # Streptococcus pneumoniae
  notes = read_excel(path_lab_code_file, sheet = "notes")
)

print("Lab codes read")

# Read in ODK data ----
print(paste0(dim(input$file_odk_data)[1], " ODK files were added"))
print("Start to read ODK data")

odk_01 <- read.csv(input$file_odk_data[[1, 'datapath']], stringsAsFactors = FALSE) %>% mutate_all(as.character)
odk_02 <- read.csv(input$file_odk_data[[2, 'datapath']], stringsAsFactors = FALSE) %>% mutate_all(as.character)
odk_03 <- read.csv(input$file_odk_data[[3, 'datapath']], stringsAsFactors = FALSE) %>% mutate_all(as.character)
odk_04 <- read.csv(input$file_odk_data[[4, 'datapath']], stringsAsFactors = FALSE) %>% mutate_all(as.character)
odk_05 <- read.csv(input$file_odk_data[[5, 'datapath']], stringsAsFactors = FALSE) %>% mutate_all(as.character)

if("BRTHDTC" %in% names(odk_01))  f01 <- odk_01
if("BRTHDTC" %in% names(odk_02))  f01 <- odk_02
if("BRTHDTC" %in% names(odk_03))  f01 <- odk_03
if("BRTHDTC" %in% names(odk_04))  f01 <- odk_04
if("BRTHDTC" %in% names(odk_05))  f01 <- odk_05

if("NO_NUMEPISODE" %in% names(odk_01))  f02 <- odk_01
if("NO_NUMEPISODE" %in% names(odk_02))  f02 <- odk_02
if("NO_NUMEPISODE" %in% names(odk_03))  f02 <- odk_03
if("NO_NUMEPISODE" %in% names(odk_04))  f02 <- odk_04
if("NO_NUMEPISODE" %in% names(odk_05))  f02 <- odk_05

if("NO_REPEATEPISODE_INDEX" %in% names(odk_01))  f02rep <- odk_01
if("NO_REPEATEPISODE_INDEX" %in% names(odk_02))  f02rep <- odk_02
if("NO_REPEATEPISODE_INDEX" %in% names(odk_03))  f02rep <- odk_03
if("NO_REPEATEPISODE_INDEX" %in% names(odk_04))  f02rep <- odk_04
if("NO_REPEATEPISODE_INDEX" %in% names(odk_05))  f02rep <- odk_05

if("D28_DATE" %in% names(odk_01))  f03 <- odk_01
if("D28_DATE" %in% names(odk_02))  f03 <- odk_02
if("D28_DATE" %in% names(odk_03))  f03 <- odk_03
if("D28_DATE" %in% names(odk_04))  f03 <- odk_04
if("D28_DATE" %in% names(odk_05))  f03 <- odk_05

if("WARD_BEDS" %in% names(odk_01))  f04 <- odk_01
if("WARD_BEDS" %in% names(odk_02))  f04 <- odk_02
if("WARD_BEDS" %in% names(odk_03))  f04 <- odk_03
if("WARD_BEDS" %in% names(odk_04))  f04 <- odk_04
if("WARD_BEDS" %in% names(odk_05))  f04 <- odk_05


# Log R console
if(! exists("f01")) print("(!!!) ERROR F01 not read")
if(! exists("f02")) print("(!!!) ERROR F02 not read")
if(! exists("f02rep")) print("(!!!) ERROR F02rep not read")
if(! exists("f03")) print("(!!!) ERROR F03 not read")
if(! exists("f04")) print("(!!!) ERROR F04 not read")
