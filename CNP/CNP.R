#load data
b <- readRDS('combined.ascat.segments.smoothednormals.rds')
txt <- read.table('TCGA.giScores.wgd.txt', sep = '\t', header = T)
cns <- readRDS('Signature_Compendium_v5_Cosine-0.74_Exposures_newNames.rds')
chr <- read.csv('df_chr.csv', header = T, row.names = 1, check.names = F)
pos <- read.csv('pos_450.csv', header = T, row.names = 1, check.names = F)

pos_chr <- cbind(chr,pos)
colnames(pos_chr) <- c('chr','pos')
#samples of esophageal cancer
esca <- txt$patient[txt$cancer_type == 'ESCA']

#copy number signatures
dim(cns)
cns <- cns[rownames(cns) %in% esca,]
dim(cns)

#copy number profiles
dim(b)
b <- b[b$sample %in% rownames(cns),] 
dim(b)
length(unique(b$sample))

#samples
sam <- rownames(cns)
print(sam)

i <- sam[1]
final_cna <- c()
for (i in sam){
  print(paste0('Starting sample: ',i))
  dat <- b[b$sample == i,]
  vec <- c()
  #for (p in 1:nrow(pos_chr)){
  for (p in 1:100){
    chr <- pos_chr[p,'chr']
    pos <- pos_chr[p,'pos']
    values <- which(chr == dat[,'chromosome'] & pos >= dat[,'start'] & pos <= dat[,'end'])
    if (sum(values) == 0){
      vec <- c(vec, NA)
    } else{vec <- c(vec, dat[values,'segVal'])}
  }
  final_cna<- cbind(final_cna,vec)
  print(cat('The dimension of final matrix: ',dim(final_cna)))
}
final_cna <- as.data.frame(final_cna)
colnames(final_cna) <- sam
