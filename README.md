# Masterthesis_CSS_2022
Syntax for SAOM analysis of Naked Emperor nominations of the RECENS dataset

The files in the repository present the syntax used for analysis for the Master thesis in Computational Social Science:
'The Naked Emperor - False popularity in the context of secondary school status competition'.

Abstract
Individuals are known to occasionally feign support for the status quo to avoid social repercussion, causing illusions of majority approval. Indeed, some adolescents, termed “Naked Emperors” in this study, have a high-status reputation in the group while their peers privately perceive that status as undeserved; a problem that risks the legitimization of undesired behaviour, such as bullying. An explanation for false popularity could be that these high-status adolescents are influential and maintain their reputation by intimidating others, limiting the willingness of classmates to openly share their true opinion, by fear of being victimized or receiving reputational damage. This framework is tested by Stochastic Actor Oriented Modelling (SAOM) of panel data of nine Hungarian classrooms of the RECENS dataset (2016). The study provides no convincing evidence that the proposed mechanism can explain the phenomenon. Results show that perceived anti-social behavioral characteristics strongly lead to Naked Emperor nomination, but this could also result from violation of classroom popularity norms, since pro-social classroom norms are likely to dominate in the classrooms of the sample. This study made a valuable first step towards understanding network dynamics of perceived undeserved high-status and contributes to the literature gap considering status competition and false popularity in secondary schools. 

Analysis
The analysis was performed in R (R Core Team, v4.0.4, 2021), with the help of the packages tidyverse (Wickham et al., v1.3.1, 2019), sna (Butts, v2.6, 2016) igraph (Csardi & Nepusz, v1.2.11, 2006), RSiena (Ripley et al., v3.0.1, 2021) and MultiSiena (Ripley et al., v1.2.9, 2021). Tables were generated with the help of packages kableExtra (Zhu, v1.3.4,2021). Plots were generated with igraph (Csardi & Nepusz, v1.2.11, 2006) and ggplot2 (Wickham, v3.3.5, 2016). 

For syntax 3,4,5,6,7 parts of the following example codes were used from the RSiena webpage (https://www.stats.ox.ac.uk/~snijders/siena/):
basicRSiena.r: https://www.stats.ox.ac.uk/~snijders/siena/basicRSiena.r
Rscript03SienaRunModel.R: https://www.stats.ox.ac.uk/~snijders/siena/Rscript03SienaRunModel.R
RscriptMultipleGroups_meta.R: https://www.stats.ox.ac.uk/~snijders/siena/RscriptMultipleGroups_meta.R
sienaGOF_vdB.R: https://www.stats.ox.ac.uk/~snijders/siena/sienaGOF_vdB.R

For Syntax 8 parts of the following example codes were used from the RSiena webpage.
RscriptsienaBayes.r : https://www.stats.ox.ac.uk/~snijders/siena/RscriptsienaBayes.r
RscriptsienaBayes_3.r: https://www.stats.ox.ac.uk/~snijders/siena/RscriptsienaBayes_3.r

Plots stemming from the Bayesian Multilevel random coefficients were generated with the help of the following code from the RSiena webpage, as can be found in syntax 9:
BayesPlots.r: https://www.stats.ox.ac.uk/~snijders/siena/BayesPlots.r

References:
Butts, Carter T. (2016). “sna: Tools for Social Network Analysis.” R package version 2.6.

Csardi, G., & Nepusz, T. (2006). The igraph software package for complex network research. 
In InterJournal: Vol. Complex Systems (p. 1695). https://igraph.org

R Core Team (2021). R: A language and environment for statistical computing. R Foundation 
for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Ruth M. Ripley, Tom A.B. Snijders, Zsófia Boda, András Vörös, and Paulina Preciado,
(2021). Manual for SIENA version 4.0 (version December 15, 2021). Oxford: University of Oxford, Department of Statistics; Nufield College. http://www.stats.ox.ac.uk/ snij-ders/siena/.

Wickham, H. (2016). Ggplot2: Elegant graphics for data analysis (2nd ed.) [PDF]. Springer 
International Publishing.

Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L., François, R., Grolemund, 
G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Pedersen, T., Miller, E., Bache, S., 
Müller, K., Ooms, J., Robinson, D., Seidel, D., Spinu, V., … Yutani, H. (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686.

Zhu, Hao. 2021. kableExtra: Construct Complex Table with Kable and Pipe Syntax.

Procedure:
0.	Calculation of Jaccard indices of Naked Emperor networks of all classrooms with and without joiners and leavers for selection of classrooms (not shown in syntax).
1.	Syntax 1:
    a.	Generating of Siena objects and variable lists, QAP per classroom per wave:
        i.	Per classroom
                            1.	Loading and cleaning of data
                            2.	Merging or dichotomizing networks
                            3.	Selection of students present in last 3 waves
                            4.	Data imputation from previous wave for absent students 
                            5.	Making Siena objects from variables
                            6.	QAP-test for Status discrepancy and NE network
                            7.	Creation of variable list (to use outside of RSiena)
2.	Syntax 2: 
    a.	Description of variables
        i.	Load variable lists
        ii.	Comparing sample and full dataset for output table
                            1.	Calculations for description of full dataset per wave
                                a.	Sum of dropouts, N, median age, gender by proportion, location, school type,                                      ethnicity, participation rate, behaviours
                            2.	Calculations for description of sample per wave
                                a.	N, median age, gender by proportion, location, school type, ethnicity,                                            participation rate, behaviours
       iii.	Calculations to describe dependent network: Naked Emperors
                            1.	Functions for network description per classroom and Hamming and Jaccard indices
                            2.	Make igraph objects of NE networks
                            3.	Calculations:
                                 a.	Network descriptives, Jaccard and Hamming distances, Isolates, Girls per                                         classroom
                            4.	Network Visualization
                                 a.	Make plot function, Make layouts, Plots are made for each classroom and wave                                     and exported
        iv.	Calculations to describe independent networks per wave
                            1.	Function for description
                            2.	Networks are transformed into igraph objects
                            3.	Network descriptions per independent network
                                  a.	Relational Aggression, antisocial and social behavior are dichotomized in                                       this process
                            4.	Calculations
                                  a.	Density, reciprocity, transitivity, degrees
                            b.	Means and SD of all classrooms
3.	Syntax 3,4,5,6,7: MoM and Meta-analysis per model
          a.	Functions to:
                          i.	Make Data
                                      1.	Data model including effects
                                      2.	Algorithms
                          ii.	Utility function:
                                      1.	To repeat MoM estimation till model is converged or deemed unfeasible.
                         iii.	Analyse data
                                      1.	To launch the MoM modelling, per classroom
          b.	SAOM
                        i.	Repeated with adaptation of algorithm until all models are converged (here it was 
                        deemed unfeasible for classroom 1200 and 7100 to reach convergence in MoM estimation).
                        ii.	Convergence check
                        iii.	Check of large Standard Errors
                                      1.	Large and variance > 0.1?
                        iv.	SAOM repeated with fixed values for parameters with large standard errors and                                   variance > 0.1
                        v.	Convergence check
                        vi.	Check of large Standard Errors
          c.	Goodness of fit estimations
                        i.	Function for Geodesic distribution
                        ii.	Calculation of GOF for indegrees, outdegrees, triads and Geodesic distribution:                                 p-values and plots.
          d.	Table of SAOM results
          e.	Meta-analysis
                         i.	Meta-analysis of list with SAOM results for all classrooms with siena08
                         ii.	SE’s random?
                         iii.	Extract estimates for meta-analysis of the mean,  Fisher test (output not used                                   in paper), heterogeneity test (output not used in paper) and overall tests with                                extraction of p-value.
4.	Syntax 8: 
        a.	Bayesian multilevel random coefficients
                        i.	Heterogeneity test of group object
                                1.	Create multi-group model with effects, but disregarding interactions, and                                       algorithm
                                2.	Estimation with siena07 MoM
                                3.	Time-test with SienaTimeTest
                                4.	Note high values of chi-squared test to decide on varying parameters in                                         BMRC.
                        ii.	BMRC modelling
                                1.	Include interaction effects to model
                                2.	Declare which parameters are considered as random
                                3.	Define prior mu’s and variance
                                4.	Create algorithm for initial group estimation with MoM
                                5.	Group estimation with MoM (unconditional)
                                      a.	Non rate parameters low and converged?
                                6.	Test estimation with Bayesian multigroup analysis sienaBayes
                                          a.	No high rate parameters?
                                7.	Complete run with BMRC
                                          a.	Check trace plots for convergence
                                8.	Repeated run, prolonging BMRC
                                          a.	Check trace plots for convergence
                                9.	Combination of the two analysis
                                          a.	Better convergence for trace plots?
                        iii.	Results
                                1.	Trace plots
                                2.	Summary of results
                                3.	Density plots
                                4.	Mean MDS
                                5.	Global parameters

Extra: Syntax 9: script stemming from: https://www.stats.ox.ac.uk/~snijders/siena/BayesPlots.r
 to source before generating plots in Syntax 8

