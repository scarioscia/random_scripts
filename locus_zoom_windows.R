library(data.table)
library(ggplot2)
library(RColorBrewer)
library(LDlinkR)

# Read in the GWAS summary stats
# discovery_gwas_df <- 
fread("/scratch16/rmccoy22/scarios1/sandbox/maternal_meiotic_aneuploidy_gwas_scratch/lmm_gwas_maternal_meiotic_aneuploidy_by_mother_discovery_total_colnames.tsv.gz", 
sep="\t")
# discovery_gwas_df[, CHROM := tstrsplit(snp_id, ":", keep = 1)] # set chromosome column if not present in file 
# discovery_gwas_filt_df <- unique(discovery_gwas_df, by = "snp_id")
# gwas_df <- discovery_gwas_filt_df

#discovery_gwas_df <- 
fread("/scratch16/rmccoy22/scarios1/natera_aneuploidy/analysis/quantgen/results/intermediate_files/maternal_meiotic_aneuploidy_by_mother_summary_stats_cpra.tsv")

# get LD for sites within the region of interest 
get_ld_matrix <- function(chrom, pos, lower_window_size, upper_window_size) {
  # Define region
  region_start <- pos - lower_window_size
  region_end <- pos + upper_window_size
  region <- paste0(chrom, ":", region_start, "-", region_end)
  
  # Use LDproxy for LD calculations (or use PLINK with VCF files if available)
  ld_result <- LDproxy(snp = paste0(chrom, ":", pos), pop = "EUR", r2d = "r2", token = "273d8c43276e")
  return(ld_result)
}

# Lead variant on grch37 as that's what LD matrix builds on 
lead_variant <- "chr14:60893425:T:C"

chrom <- strsplit(lead_variant, ":")[[1]][1]
pos <- as.numeric(strsplit(lead_variant, ":")[[1]][2])
lower_window_size <- 1e5
upper_window_size <- 1e5

# Extract relevant data - if using position rather than RSID
#region_data <- gwas_df[CHROM == chrom & pos >= (pos - lower_window_size) & pos <= (pos + upper_window_size)]
# Get chr_pos without ref:alt to match format of ld_matrix
#region_data$chr_pos <- sub("^([^:]+:[^:]+):.*", "\\1", region_data$snp_id)

# Get LD matrix
ld_matrix <- get_ld_matrix(chrom, pos, lower_window_size, upper_window_size)
# create new column for genomic position 
ld_matrix$pos <- sub("^[^:]+:", "", ld_matrix$Coord)

# Extract from GWAS sites present in ld_matrix
sumstats_region <- discovery_gwas_df[discovery_gwas_df$SNP %in% ld_matrix$RS_Number,]

# Merge LD data with GWAS results
merged_data <- merge(sumstats_region, ld_matrix, by.x = "SNP", by.y = "RS_Number", all.x = TRUE)

# Find out the windows to get the genes manually without printing them 
linked_snps <- merged_data[merged_data$R2 > 0.8,]
window_min_linked <- as.numeric(min(linked_snps$pos))
window_max_linked <- as.numeric(max(linked_snps$pos))

rsid_min <- linked_snps[linked_snps$pos == window_min_linked]$SNP
rsid_max <- linked_snps[linked_snps$pos == window_max_linked]$SNP

# Plot 
ggplot(merged_data, aes(x = as.numeric(pos), y = -log10(P), color = R2)) +
  geom_point(size = 2) +
  scale_color_gradientn(colors = brewer.pal(9, "RdYlBu")) +
  geom_hline(yintercept = -log10(5e-8), linetype = "dashed", color = "black") +
  geom_vline(xintercept = window_min_linked, linetype = "solid", color = "black") + 
  geom_vline(xintercept = window_max_linked, linetype = "solid", color = "black") + 
  theme_minimal() +
  labs(x = paste0("Chromosome ", chrom, " Position (Mbp)"), y = "-log10(P-value)", color = "LD (r²)")

# Find length of haplotype 
num_kb <- (window_max_linked - window_min_linked) / 1000




