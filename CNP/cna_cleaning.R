#load data
cna <- read.csv('cna_df.csv', header = T, sep = ',',row.names = 1)
txt <- read.table('TCGA.giScores.wgd.txt', sep = '\t', header = T)
cns <- readRDS('Signature_Compendium_v5_Cosine-0.74_Exposures_newNames.rds')

#samples of esophageal cancer
esca <- txt$patient[txt$cancer_type == 'ESCA']

#copy number signatures
dim(cns)
cns <- cns[rownames(cns) %in% esca,]
dim(cns)

colnames(cna) <- rownames(cns)
cna[1:10,1:10]

#NAs values
sum(is.na(cna))
cna <- na.omit(cna)
dim(cna)
sum(is.na(cna))
cna <- t(cna)
cna[1:10,1:10]

# cnp with s1
dim(cna)
cna_s1 <- cbind(cna,cns[,1])
dim(cna_s1)
write.csv(cna_s1, file = 'cna_s1.csv')

# cnp with s3
dim(cna)
cna_s3 <- cbind(cna,cns[,2])
dim(cna_s3)
write.csv(cna_s3, file = 'cna_s3.csv')
