options(stringsAsFactors=F)
library(dplyr)
library(data.table)
library(sas7bdat)

path = "~/Desktop/Liu_Labwork/cancer/cancer_cost/original/RE__Cost_of_cancer_care_in_the_US/Year 2018 Data SAS Files/"
filename = dir(path, pattern = "bdat")

##Overview of CPT: https://www.medicalbillingandcoding.org/intro-to-cpt/#:~:text=Category%20I%20CPT%20codes%20are%20numeric%2C%20and%20are%20five%20digits,Pathology%20and%20Laboratory%2C%20and%20Medicine.##

##CPT I group: ref = https://www.aapc.com/codes/cpt-codes-range/## Date of retrieval: 01/28/2021##
cpt = data.frame()
temp = data.frame("category" = "A", "alias" = "CPT_I_group1", "code" = as.character(c(paste("00", 100:999, sep = ""), paste("0", 1000:1999, sep = "")))) ##Anesthesia##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "S", "alias" = "CPT_I_group2", "code" = as.character(10004:69990)) ##Surgery##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "R", "alias" = "CPT_I_group3", "code" = as.character(70010:79999)) ##Radiology Procedures##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "P", "alias" = "CPT_I_group4", "code" = as.character(c( paste0("000", 1:9, "U"), paste0("00", 10:99, "U"), paste0("0", 100:247, "U"), 80000:89398))) ##Pathology and Laboratory Procedures##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "M", "alias" = "CPT_I_group5", "code" = as.character(setdiff(90281:99607, 99091:99499)) ) ##Medicine services and procedures##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "E", "alias" = "CPT_I_group6", "code" = as.character(99091:99499) ) ##Evaluation and management##
cpt = rbind(cpt, temp)

##HCPCS II group: ref = https://www.aapc.com/codes/hcpcs-codes-range/## Date of retrieval: 01/28/2021##
for (g in c("A", "B", "C", "E", "G", "H", "J", "K", "L", "M", "P",  "R")){
	temp = data.frame("category" = "NP", "alias" = paste("HCPCS_II_group", g, sep =""), "code" = as.character(c(paste(g, "000", 1:9, sep = ""), paste(g, "00", 10:99, sep = ""),
                                                                paste(g, "0", 100:999, sep = ""), paste(g, 1000:9999, sep = ""))))
	cpt = rbind(cpt, temp)
}

##A = Ambulance and Other Transport Services and Supplies, Medical And Surgical Supplies, Administrative, Miscellaneous and Investigational##
##B = Enteral and Parenteral Therapy##
##C = Other Therapeutic Procedures, Outpatient PPS##
##D = Dental services##
##E = Durable Medical Equipment##
##G = Procedures / Professional Services##
##H = Alcohol and Drug Abuse Treatment##
##J = Drugs Administered Other than Oral Method, Chemotherapy Drugs##
##K = Durable medical equipment (DME) Medicare administrative contractors (MACs), Components, Accessories and Supplies##
##L = Orthotic Procedures and services, Prosthetic Procedures##
##M = Miscellaneous Medical Services, Screening Procedures, Other Services, Episode of Care##
##P = Pathology and Laboratory Services##
##Q = Temporary Codes##
##R = Diagnostic Radiology Services##
##S = Temporary National Codes (Non-Medicare)##
##T = National Codes Established for State Medicaid Agencies##
##U = Coronavirus Diagnostic Panel##
##V = Vision Services, Hearing Services##

cpt$category = as.character(cpt$category)
cpt$code = as.character(cpt$code)
colnames(cpt)[3] = "procedure_code"
write.table(cpt, "~/Desktop/Liu_Labwork/cancer/cancer_cost/cpt_group_mapping.txt", 
            col.names = TRUE, row.names = FALSE, sep = "\t")

category_list = c("P", "A", "S", "R", "M", "E", "NP")

##Analysis for figure 1##
result_cost = as.data.frame(matrix(NA, length(filename), 1 + length(category_list)))
result_count = as.data.frame(matrix(NA, length(filename), 1+ length(category_list)))
##Analysis##
for (i in 1:length(filename)){
  tissue = strsplit(filename[i], "_")[[1]][2]
  
  result_cost[i, 1] = tissue
  result_count[i, 1] = tissue
  df = as.data.frame(read.sas7bdat(paste(path, "/", filename[i], sep = "")))
  df$PROC1 = as.character(df$PROC1)
  
  for (j in 1:length(category_list)){
    df_ss = df[df$PROC1 %in% (cpt %>% filter(category == category_list[j]))$procedure_code, ]
    result_cost[i, j+1] = sum(df_ss$totcost_proc1)
    result_count[i, j+1] = sum(df_ss$COUNT)
  }
}
colnames(result_count) = c("tissue", category_list)
colnames(result_cost) = c("tissue", category_list)




##Analysis for Figure 2##
df = data.frame()
for (i in 1:length(filename)){
	df_temp = read.sas7bdat(paste(path, "/", filename[i], sep = ""))
	df = rbind(df, df_temp)	
}
df = as.data.frame(df %>% group_by(PROC1) %>% summarise(Total_count = sum(COUNT)))

merge = merge(df, cpt, by.x = "PROC1", by.y = "code")


##CPT I and HCPCS I description: ref = https://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/PhysicianFeeSched/PFS-Relative-Value-Files-Items/RVU18A## Date of retrieval: 01/28/2021##
descx = as.data.frame(fread("/gpfs/scratch/cxk502/cancer_cost/2018/RVUA/rvu18a/CPT_mapping.csv", header = TRUE, sep = ","))
descx = distinct(descx %>% select(HCPCS, DESCRIPTION))
descx = descx[descx$HCPCS %in% merge$PROC1, ]
descx = rbind(descx, data.frame("HCPCS" = setdiff(merge$PROC1, descx$HCPCS), "DESCRIPTION" = "NA"))

merge = merge(merge, descx, by.x = "PROC1", by.y = "HCPCS")
merge = merge[order(-merge$Total_count), ]
merge = merge %>% select(category, PROC1, DESCRIPTION, Total_count)
colnames(merge) = c("CATEGORY", "PROCEDURE_CODE", "DESCRIPTION", "COUNT")

write.table(merge, "/gpfs/scratch/cxk502/cancer_cost/2018/code_merged_count.txt", col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)

###############################################################################################################################################################################################################
