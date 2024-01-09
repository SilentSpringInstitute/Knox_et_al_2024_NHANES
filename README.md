# Knox_et_al_2024_NHANES
R code for: "Using U.S. National Biomonitoring Data to Assess the Impacts of California’s Prop 65"

Kristin E. Knox,1* Megan R. Schwarzman,2 Ruthann A. Rudel,1 Claudia Polsky,3 and Robin E. Dodson1
1 Silent Spring Institute, Newton, MA 02460 USA
2 School of Public Health, University of California, Berkeley, CA 94720 USA 
3 School of Law, University of California, Berkeley, CA 94720 USA
*Email: knox@silentspring.org

The following scripts were run before we went to the National Center Health Statistics (NCHS) Restricted Data Center (RDC):
1. "RDC - Make 95 percentile cut-offs_publish.R" uses public NHANES data to calculate the 95th percentile concentration for each of the analytes included in our analysis.  It is a necessary precursor to:
2. "RDC - Create Public Dataset_publish.R" - this script creates the public data file that we provided to the RDC. Since once of the variables included in our original analysis was a 0/1 indicator for whether a concentration was "high" (defined as greater than the 95th percentile value), this script must be run after "RDC - Make 95 percentile cut-offs_publish.R" which calculates the cuf-off values.

Once we created the public dataset, we provided it to the RDC. Our RDC analyst then merged in the restricted data variables by SEQN: exam year, indicator for CA/non-CA, actual PSU (T_VPSU) and strata (T_VSTRAT) variables.

"RDC_publish.R" is the final version of the script that was run at the RDC. This script produces all output that is used in the paper.

The remaining scripts formats the output from the RDC analysis for publication, as follows:
"Make Paper Tables_publish.R" selects/formats the RDC output that is shown in tables 2-5, with the exception of the observation counts, which are computed in "Count_the_Ns_publish.R" from the public data.

"make figure 1_publish.R" pulls the necessary RDC output and creates figure 1 in the paper.

"make public data figures_publish.R" pulls and formats the public NHANES data that is used to create figures 2-4 and "make final paper figures_publish.R" takes this data and makes the actual figures 2-4.

"Creatinine sensitivity check_publish.R" is the sensitivity check that makes sure the regression results for urine metabolites are not materially altered by the includion or exclusion of ln(creatinine) on the RHS. These results are shown in the supplemental.
