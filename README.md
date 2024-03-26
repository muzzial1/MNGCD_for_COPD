## An integrative network-based approach to identify driving gene communities in chronic obstructive pulmonary disease (COPD)
Supplementary repository to collect the set of scritps to analyze a set of multi-layer networks using multi-network gene community detection (MNGCD) method, based on the partition of maps of information algorithm, to identify subnetworks of strongly interacting genes ('gene communities’), which were then characterized for enrichment with annotated biological functions or sets of regulons. The analysis was applied to establish an integrative network-based approach to identify driving gene communities in chronic obstructive pulmonary disease (COPD).

# Scripts execution flow
The execution environment is a Unix HPC with slurm parallel execution queue manager, Infomap and R are required. Here we report a description of the executed code to run Infomap and the enrichment analysis of the identified gene communities (scripts avalailable). 
/**
1. Run the following bash scripts in the following order after changing in each script the line for j in (ls -1d output_*vero)  replacing the term output_*vero with the name of the file to analyse:
* ./execallt.sh generates 100 partitions using Infomap:
  - it executes perparcopy2.sh, which in turn runs stability_sb2.R 20 times in parallel.
  - stability_sb2.R calls myscript_varifile_copy_cal.sh, which executes Infomap 100 times with different seeds for each iteration.
  - Seeds also vary across the 20 invocations of perparcopy2.sh.
* ./execallt_cons.sh calculates the consensus on the 100 partitions:
  - It executes perparconsensi_v2.sh, which runs stabilityperconsenso_v2.R 20 times in parallel.
  - stabilityperconsenso_v2.R calculates the consensus as follows:
  - A for loop is executed, starting with the 100 partitions generated by Infomap.
  - In each iteration, generoperconsenso_cal.sh is called.
  - The process continues until all generated partitions are identical, producing the consensus matrix.
* ./execallt_arr.sh  (with argument h or c3 or c5 or BTM, depending on the gene-set collection to use for the enrichment of communities)
* ./execallt_pval.sh  (with argument h or c3 or c5 or BTM, depending on the gene-set collection to use for the enrichment of communities)
* ./alltabs.sh (with argument h or c3 or c5 or BTM, depending on the gene-set collection to use for the enrichment of communities)
*/

2. In the folder  final_result in the folder prova_nmi/output_nameofthemultinetwork   there is the final result (functions enriched in each community with their rate) for the gene set collection selected.
