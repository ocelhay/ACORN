# Read in ACORN data dictionary ----
print("Okay, start to read data dictionnary")  # Log R console
path_data_dictionary_file <- input$file_data_dic[[1, 'datapath']]

data_dictionary <- list()
# data_dictionary$input.file <- read_excel(path_data_dictionary_file, sheet = "lab.filename",range = "B1:C2")
data_dictionary$variables <- read_excel(path_data_dictionary_file, sheet = "variables")
# data_dictionary$date.format <- read_excel(path_data_dictionary_file, sheet = "lab.filename",range = "D1:D2")
data_dictionary$test.res <- read_excel(path_data_dictionary_file, sheet = "test.results")
data_dictionary$local.spec <- read_excel(path_data_dictionary_file, sheet = "spec.types")
data_dictionary$local.orgs <- read_excel(path_data_dictionary_file, sheet = "organisms")

print("Okay, import data dictionnary") # Log R console



# Read in ACORN lab data
print("Okay, start to read ACORN lab data")  # Log R console

if(input$whonet_file == "WHONET file") {
  amr.loc <- read.dbf(path_lab_file, as.is = T)
}

if(input$whonet_file == "Not WHONET file") {
  path_lab_file <- input$file_lab_data[[1, 'datapath']]
  extension_file_lab_codes <- file_ext(path_lab_file)
  
  if (extension_file_lab_codes == "csv") { amr.loc <- read_csv(path_lab_file, guess_max = 10000) } else if
  (extension_file_lab_codes == "txt") { amr.loc <- read_tsv(path_lab_file, guess_max = 10000) } else if
  (extension_file_lab_codes %in% c("xls", "xlsx")) { amr.loc <- read_excel(path_lab_file, guess_max = 10000) }
}

print(paste0("Info: extension file of ACORN lab data is ", extension_file_lab_codes))  # Log R console
print("Okay, import ACORN lab data")  # Log R console


# Read in lab code and AST breakpoint data ----
print("Okay, start to read lab codes")  # Log R console

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

shiny_lab_code_notes <<- lab_code$notes

print("Okay, lab codes read")  # Log R console

# Read in ODK data ----
print("Okay, start to read ODK data")  # Log R console
print(paste0("Info: you have added ", dim(input$file_odk_data)[1], " files."))

odk_01 <- read.csv(input$file_odk_data[[1, 'datapath']], stringsAsFactors = FALSE) %>% mutate_all(as.character)
odk_02 <- read.csv(input$file_odk_data[[2, 'datapath']], stringsAsFactors = FALSE) %>% mutate_all(as.character)
odk_03 <- read.csv(input$file_odk_data[[3, 'datapath']], stringsAsFactors = FALSE) %>% mutate_all(as.character)
odk_04 <- read.csv(input$file_odk_data[[4, 'datapath']], stringsAsFactors = FALSE) %>% mutate_all(as.character)
odk_05 <- read.csv(input$file_odk_data[[5, 'datapath']], stringsAsFactors = FALSE) %>% mutate_all(as.character)

if(names(odk_01)[11] == "BRTHDTC")  f01 <- odk_01
if(names(odk_02)[11] == "BRTHDTC")  f01 <- odk_02
if(names(odk_03)[11] == "BRTHDTC")  f01 <- odk_03
if(names(odk_04)[11] == "BRTHDTC")  f01 <- odk_04
if(names(odk_05)[11] == "BRTHDTC")  f01 <- odk_05

if(names(odk_01)[10] == "NO_NUMEPISODE")  f02 <- odk_01
if(names(odk_02)[10] == "NO_NUMEPISODE")  f02 <- odk_02
if(names(odk_03)[10] == "NO_NUMEPISODE")  f02 <- odk_03
if(names(odk_04)[10] == "NO_NUMEPISODE")  f02 <- odk_04
if(names(odk_05)[10] == "NO_NUMEPISODE")  f02 <- odk_05

if(names(odk_01)[1] == "NO_REPEATEPISODE_INDEX")  f02rep <- odk_01
if(names(odk_02)[1] == "NO_REPEATEPISODE_INDEX")  f02rep <- odk_02
if(names(odk_03)[1] == "NO_REPEATEPISODE_INDEX")  f02rep <- odk_03
if(names(odk_04)[1] == "NO_REPEATEPISODE_INDEX")  f02rep <- odk_04
if(names(odk_05)[1] == "NO_REPEATEPISODE_INDEX")  f02rep <- odk_05

if(names(odk_01)[10] == "D28_DATE")  f03 <- odk_01
if(names(odk_02)[10] == "D28_DATE")  f03 <- odk_02
if(names(odk_03)[10] == "D28_DATE")  f03 <- odk_03
if(names(odk_04)[10] == "D28_DATE")  f03 <- odk_04
if(names(odk_05)[10] == "D28_DATE")  f03 <- odk_05

if(names(odk_01)[10] == "WARD")  f04 <- odk_01
if(names(odk_02)[10] == "WARD")  f04 <- odk_02
if(names(odk_03)[10] == "WARD")  f04 <- odk_03
if(names(odk_04)[10] == "WARD")  f04 <- odk_04
if(names(odk_05)[10] == "WARD")  f04 <- odk_05

# Log R console
ifelse(exists("f01"), print("Okay, F01 data"), print("(!)ERROR(!) F01 not found"))
ifelse(exists("f02"), print("Okay, F02 data"), print("(!)ERROR(!) F02 not found"))
ifelse(exists("f02rep"), print("Okay, F02 rep data"), print("(!)ERROR(!) F02 rep not found"))
ifelse(exists("f03"), print("Okay, F03 data"), print("(!)ERROR F03(!) not found"))
ifelse(exists("f04"), print("Okay, F04 data"), print("(!)ERROR F04(!) not found"))
