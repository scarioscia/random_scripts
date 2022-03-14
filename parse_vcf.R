## Quick script to isolate the pathogenic reads from an annotated vcf file and 
## identify the genes involved (can be applied to other information)  
library(data.table) 
library(dplyr) # for pipe, filter, str_detect
library(stringr) # to separate column by | 

# read in full file, despite variable number of fields at first 
all_content <- readLines("/Users/saracarioscia/Downloads/sample.vcf")
# skip until we get to the fixed area header
variant_rows <- all_content[-c(1:grep("#CHROM", all_content))]
# read back in the fixed lines as a table 
variants <- read.table(textConnection(variant_rows))

# get only pathogenic variants 
variants_dt <- variants %>% as.data.table()
pathogenic_variants <- variants_dt %>% 
  filter_all(any_vars(str_detect(., pattern = "PATHOGENIC")))
# write it back as a csv 
write.csv(pathogenic_variants, "~/Downloads/pathogenic_variants.csv", row.names = FALSE, quote = FALSE)


# Format is `|GENE=gene_name;`. Try to grab the content between GENE= and ; 
for (i in 1:nrow(pathogenic_variants)) {
  # go through each row of the pathogenic variants
  row <- pathogenic_variants[i,]
  # grab the content after GENE=
  gene <- str_match(row, "GENE=\\s*(.*?)\\s*;")
  # other rows are grabbed; we want those that are not NA 
  gene <- gene[,2][!is.na(gene[,2])]
  # assign the gene to the column in pathogenic_variants 
  pathogenic_variants$genes[i] <- gene
}


# using those pathogenic variants, grab the genes they're associated with 
# separate the information 
ncols <- max(stringr::str_count(pathogenic_variants$V8, "|")) + 1
colmn <- paste("col ", 1:ncols)
pathogenic_variants_genes <- pathogenic_variants %>% separate(V8, sep = "|", into = colmn, remove = FALSE)
