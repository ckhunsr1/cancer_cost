options(stringsAsFactors=F)
.libPaths("/storage/home/cxk502/work/R_library/")
library(dplyr)
library(data.table)

path = "/storage/home/cxk502/scratch/cancer_cost/2018"
filename = dir(path, pattern = "cost")

##Overview of CPT: https://www.medicalbillingandcoding.org/intro-to-cpt/#:~:text=Category%20I%20CPT%20codes%20are%20numeric%2C%20and%20are%20five%20digits,Pathology%20and%20Laboratory%2C%20and%20Medicine.##

##CPT I group: ref = https://www.aapc.com/codes/cpt-codes-range/## Date of retrieval: 01/28/2021##
cpt = data.frame()
temp = data.frame("category" = "CPT_I_group1", "code" = c(paste("00", 100:999, sep = ""), paste("0", 1000:9999, sep = ""))) ##Anesthesia##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_I_group2", "code" = 10000:69999) ##Surgery##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_I_group3", "code" = 70000:79999) ##Radiology Procedures##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_I_group4", "code" = c(paste("000", 1:9, "U", sep = ""), paste("00", 10:99, "U", sep = ""), 
								paste("0", 100:241, "U", sep = ""), 80000:89999)) ##Pathology and Laboratory Procedures##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_I_group5", "code" = 90000:99999) ##Evaluation and Management##
cpt = rbind(cpt, temp)


##CPT II group: ref = https://www.aapc.com/codes/cpt-codes-range/## Date of retrieval: 01/28/2021##
temp = data.frame("category" = "CPT_II_group1", "code" = c(paste("000", 1:9, "F", sep = ""), paste("00", 10:15, "F", sep = ""))) ##Composite Measures##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_II_group2", "code" = paste("0", 500:584, "F", sep = "")) ##Patient Management##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_II_group3", "code" = paste(1000:1505, "F", sep = "")) ##Patient History##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_II_group4", "code" = paste(2000:2060, "F", sep = "")) ##Physical Examination##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_II_group5", "code" = paste(3006:3776, "F", sep = "")) ##Diagnostic/Screening Processes or Results##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_II_group6", "code" = paste(4000:4563, "F", sep = "")) ##Therapeutic, Preventive or Other Interventions##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_II_group7", "code" = paste(5005:5250, "F", sep = "")) ##Follow-up or Other Outcomes##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_II_group8", "code" = paste(6005:6150, "F", sep = "")) ##Patient Safety##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_II_group9", "code" = paste(7010:7025, "F", sep = "")) ##Structural Measures##
cpt = rbind(cpt, temp)
temp = data.frame("category" = "CPT_II_group10", "code" = paste(9001:9007, "F", sep = "")) ##Non-Measure Category II Codes##

##CPT III group: ref = https://www.aapc.com/codes/cpt-codes-range/## Date of retrieval: 01/28/2021##
temp = data.frame("category" = "CPT_III", "code" = c(paste("000", 1:9, "T", sep = ""), paste("00", 10:99, "T", sep = ""), paste("0", 100:999, "T", sep = "")))
cpt = rbind(cpt, temp)

##HCPCS II group: ref = https://www.aapc.com/codes/hcpcs-codes-range/## Date of retrieval: 01/28/2021##
for (g in c("A", "B", "C", "D", "E", "G", "H", "J", "K", "L", "M", "P", "Q", "R", "S", "T", "U", "V")){
	temp = data.frame("category" = paste("HCPCS_II_group", g, sep =""), "code" = c(paste(g, "000", 1:9, sep = ""), paste(g, "00", 10:99, sep = ""),
                                                                paste(g, "0", 100:999, sep = ""), paste(g, 1000:9999, sep = "")))
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

##Identify top 50 CPT codes according to data from 15 cancer types##
df = data.frame()
for (i in 1:length(filename)){
	df_temp = read.table(paste(path, "/", filename[i], sep = ""), header = TRUE, sep = ",")
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
