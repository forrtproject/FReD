### EXPLORER ###

about_page <- glue::glue("

## FORRT Replication Database Explorer {packageVersion('FReD')}

**Last Update:** {get_last_modified('fred_explorer')}

**Data citation:** {create_citation()}

**Data and Materials:** <a href='https://osf.io/9r62x/' target='_blank'>https://osf.io/9r62x/</a>

**Contribute:** Please send an e-mail to lukas.roeseler(at)uni-muenster.de

**License:** _Data:_ CC-By Attribution 4.0 International, _Code:_ MIT License

**Acknowledgements:** We thank all researchers who have invested resources in conducting replication research, researchers who have submitted their replication studies, and researchers who used the Replication Recipe Post-Completion template to register their results. FORRT Replication Database is supported through the University of Bamberg's Interne Forschungsförderung, by the University of Münster, by the Nederlandse Organisatie voor Wetenschappelijk's (NWO) Open Science Fund, and by the Leuphana University Lüneburg.

**Important note:** This is work in progress. Please beware that there might be bugs or errors in the dataset. If you spot any, please let us know <a href='mailto:lukas.roeseler@uni-muenster.de' target='_blank'>by email</a>, or on <a href='https://github.com/forrtproject/FReD/issues' target='_blank'>GitHub</a>.

_Thanks to our funders:_
<img src='ub.png' height='100' /> <img src='um.png' height='50' /> <img src='nwo.png' height='100' />

")





info <- HTML(paste("<h3>Welcome to the FORRT Replication Database!"
                   , "<h4><br/><br/><b>What is FReD?</b><br/> <i>Science appears to be the human enterprise that is most systematic in its attempt to eliminate error in the search for knowledge </i>(Hoyningen-Huene, 2013, p. 89).
                   </br></br>Still, if - or how well - most of our findings replicate, is unknown. The FORRT Replication Database is a crowdsourced effort to include unpublished and published replication results to estimate and track the replicability along various fields and provide researchers with a way to assess replicability of crucial studies in a quick and transparent way. Check out the <a href=https://osf.io/f3w26 target='_blank'>Call for Results</a> if you would like to contribute."
                   , "<br/><br/><b>What are your benefits of joining us?</b><br/> You are very welcome to contribute data from your replication studies! In return, (apart from being rewarded by the good feeling of helping research on replicability to improve) we will list you as a contributor of the FReD (CRediT: Resources). Please use the <a href=https://www.soscisurvey.de/replicate target='_blank'>submission portal</a> to submit replication results.
                        <br/><br/>Unpublished datasets as well as data from classroom experiments are also highly appreciated. Get in touch if you have any questions about the submission portal."
                   , "<br/><br/><b>How to use this website</b><br/> Here in our ShinyApp, you can explore replicability for all or filtered entries. Click on the other tabs and filter your results on the left side.
                        <br/><br/>In the <a href=https://osf.io/9r62x/ target='_blank'>OSF project</a>, you can find further information and files on our project. There, you also can send us a contribution request to the project.
                        <br/><br/>For questions or comments, please check out the FAQs on this website, or send an e-mail to lukas.roeseler(at)uni-muenster.de"
                   , "<br/><br/>"
                   , sep = ""))

welcome_title <- HTML("Welcome to the FORRT Replication Database!")
welcome_text <- HTML("<br /><p style=\'color:black;\'>The FReD is a collection of crowdsourced findings from replication studies (i.e., studies that investigated one or more previously tested hypotheses using new data). The aims of this project are:
</br>(1) to document replication attempts across many areas of science and make replications findable and
</br>(2) to provide meta-scientists with a database for research on replicability.
</br></br>This is the FReD <b>Explorer</b>. You can browse, search, and summarize replication results of the coded FReD entries. You can go to the online version of the FReD <b>Annotator</b> <a href=https://forrt-replications.shinyapps.io/fred_annotator/ target='_blank'>here</a>.
</br></br>We advise researchers to carefully investigate replication findings before making judgments about the robustness of research findings. Note that to allow be able to present effect sizes from hundreds of different studies and designs in one common metric, several assumptions are made about the dataset some of which are wrong. How effect sizes are handled is described in depth in the <a href=https://doi.org/10.5334/jopd.101 target='_blank'>FReD dataset publication</a>.
                     </br></br>You can find more information on the overarching project at the <a href=https://forrt.org/replication-hub target='_blank'>FORRT Replication Hub</a>. If you would like to contribute replication findings, please check out our <a href=https://osf.io/f3w26 target='_blank'>call for results</a> or send us an e-mail.</p><br/>")


dataset_explanation <- shiny::HTML(paste("<h4><b>FReD Dataset</b>"
                                         , "<h5><br/>This is the entire FORRT Replication Database Dataset. It currently contains "
                                         , nrow(df)
                                         , " findings. The most recent version can be downloaded via the <a href=https://osf.io/9r62x/ target='_blank'>OSF project</a>."
                                         , "<br/><br/>"
                                         , sep = ""))

dataset_headline <- HTML(paste("<h4><b>Dataset</b><h6>"
                               , sep = ""))

variables_headline <- HTML(paste("<h4><b>Variables</b><h5>"
                                 , sep = ""))

dataset_info <- HTML(paste("<h4><b>Replication Rate</b>"
                           , "<h5><br/>There are currently "
                           , nrow(df)
                           , " replication findings entered into the database. By default, only validated and coded findings are selected. Of all findings, "
                           , length(unique(df$ref_r))
                           , " are independent (i.e., use different samples/stem from different studies). Note that the following analyses treat all studies as independent. Apart from the table and bar chart, only studies for which sample sizes and effect sizes are available (for original study and replication) are considered here. The others can be viewed in the Dataset."
                           , " In total, "
                           , length(unique(df$ref_o))
                           , " different original studies have been replicated."
                           # , "According to the original researchers' assessments, there have been "
                           # , sum(red$pc05 == "informative failure to replicate", na.rm = TRUE)
                           # , " informative failures to replicate and "
                           # , sum(red$pc05 == "success", na.rm = TRUE)
                           # , " successes. "
                           # , sum(red$pc05 == "inconclusive", na.rm = TRUE)
                           # , " replications have yielded inconclusive results and "
                           # , sum(red$pc05 == "practical failure to replicate", na.rm = TRUE)
                           # , " were practical failures to replicate."
                           , "<br/><br/><h6>"
                           , sep = ""))

forest_info <- HTML(paste("<h4><b>Study Overview</b>"
                          , "<h5><br/>Currently, "
                          , nrow(df)
                          , " replication findings are entered into the database. These stem from "
                          , length(unique(df$ref_o))
                          , " independent original studies. This is an overview of these studies."
                          , "<br/><br/><h6>"
                          , sep = ""))

scatterplot_title <- HTML(paste("
                                 <h4><br/><br/><b>Scatterplot of Original and Replication Effect Sizes</b><br/>"
                                , "<br/><br/>"
                                , sep = ""))

scatterplot_explanation <- HTML(paste("
                                 <h5><i>Note. </i>This plot is based on the code used for the main plot of Open Science Collaboration (2015). Here you can see for each replication study the original effect and the replication effect. Significant replication effects (p < .05) are highlighted in blue. If all studies were perfectly replicable, the dots would be on the solid grey line. If no study was replicable, the dots would be at the dashed line (= null effects). Hover over the plot to see the exact effect sizes and the study. Clicking on rows in the table above this plot will highlight the selected studies. If there are registered replication reports (RRRs) among the selected study, you will see 'columns' of effect sizes because all studies from a RRR have the same 'original effect size' but replication effect sizes vary."
                                      , "<br/><br/>"
                                      , sep = ""))

barplot2_title <- HTML(paste("
                                 <h4><br/><br/><b>A More Nuanced Interpretation of Replication Effects</b><br/>"
                             , "<br/><br/>"
                             , sep = ""))

barplot2_explanation <- HTML(paste("
                                 <h5><i>Note. </i><a href='https://doi.org/10.15626/MP.2018.843 target='_blank'>LeBel et al. (2019)</a> have suggested a more nuanced interpretation of replication results for cases where the original study found an effect. Whether or not the replication effect is significant, too, is indicated by signal/no-signal. Whether the replication effect is smaller, larger, or the same size is also indicated. We also included cases where the original study was not significant or no information about the original study's significance was available in grey."
                                   , "<br/><br/>"
                                   , sep = ""))

zcurve_title <- HTML(paste("
                                 <h4><br/><br/><b>Z-Curve Analysis</b><br/> "
                           , "<br/><br/>"
                           , sep = ""))

zcurve_explanation <- HTML(paste("
                                 <h5><i>Note. </i>Z-curve (<a href='http://dx.doi.org/10.15626/MP.2021.2720' target='_blank'>Bartos & Schimmack, 2020</a>) can be used to estimate replicability of a set of studies. Note that it assumes that observations are independent, which is not the case here, and that it uses only significant results. We recommend not using z-curve on less than 200 tests.<br/>Observed discovery rate refers to the proportion of significant (p < .05) studies. Expected discovery rate is the proportion of studies that you would expect to be significant if you ran replications of <i>all</i> studies with the same sample size. Expected replicability rate is the proportion of studies that you would expect to be significant if you ran replications of <i>all significant</i> studies with the same sample size. You can compare the discovery and replicability rates with the actual replicability presented at the top of this page. We recommend running bootstraps to get confidence intervals for z-curve's estimates but refrain from doing so as it takes much time."
                                 , "<br/><br/>"
                                 , sep = ""))



correlates_info <- HTML(paste("
                                 <h4><br/><br/><b>Correlates of Replicability</b><br/> "
                              , "<h4>Has replicability increased over time? How do I know if a published finding is replicable? Are there differences betweeen research fields? Using the dataset, we can test what moderators are correlated with certain replication outcomes.</br></br><b><font color=\"#ff0000\">Please keep in mind that these analyses are preliminary for at least two reasons: They include non-validated data and some variables may not have been coded yet for a large proportion of the dataset</font></b>"
                              , sep = ""))

moderators_info <- HTML(paste("
                                 <h4><br/><br/><b>Moderators of Replication Effect Sizes</b><br/> "
                              , "<h4>Here you can explore the influence of several moderators on the replication effect size. </br></br><b><font color=\"#ff0000\">Please keep in mind that these analyses are preliminary for at least two reasons: They include non-validated data and some variables may not have been coded yet for a large proportion of the dataset.<br/><br/></b></font><h5>"
                              , sep = ""))


correlates_decade <- HTML(paste("
                                 <h4><br/><br/><b>Replicability over time</b><br/> "
                                , "For the following analysis, replication findings have been aggregated for each <i>decade</i> of the original finding's publication year."
                                , sep = ""))

correlates_journal <- HTML(paste("
                                 <h4><br/><br/><b>Replicability by Journal</b><br/> "
                                 , "For the following analysis, replication findings have been aggregated for each <i>journal where the original finding was published</i>. This can serve as a shortcut to comparing replicability by research area. Note that mixed findings can be a mix of many succesful and one inconclusive or failed replication or vice versa."
                                 , sep = ""))



rc_info  <- HTML(paste("
                                 <h4><br/><br/><b>References-Checker</b><br/> "
                       , "<h4>Paste your entire lists of references or DOIs here. In order to identify replication studies, there need to be DOIs. Please note that not all studies entered in ReD feature a DOI or that some papers may even have no or more than one DOI. Finally, ReD does not contain <i>all</i> replications. That means, if there are no replications listed in ReD, this does not mean that nobody has ever attempted to replicate the entered studies.<h6>"
                       , sep = ""))


checker_info <- HTML(paste("<h4><br/><br/><b>Replicability Checker</b><br/> "
                           , "<br/><br/><h4>Filter the database via the table's search function and it will return a summary of the state of research regarding the searched entries at the bottom of the page."
                           , "<h6>"
                           , sep = ""))

references_headline <- HTML(paste("<h4><br/><br/><b>References</b><br/> "
                                  , "<br/><br/><h4>Currently, a large proportion of the replication studies stems from the CurateScience database. We added data from CORE, RPP, the OSF Registries, and individual submissions. A synthesis of ReD with FORRT's replications and reversals is coming soon. If you are aware of replications not listed here, please write us an e-mail or add them <a href = 'https://docs.google.com/spreadsheets/d/1x68oW2H_Xrdv44fIeycl4fegsmQgCa60GxeZZ_hAR90/edit?pli=1#gid=1463805480' target='_blank'>here.</a> "
                                  , sep = ""))



references_list <- HTML(paste(
  "<br/><br/>- Curate Science Database: https://web.archive.org/web/20220128104303mp_/https://curatescience.org/app/replications"
  , "FORRT Replications and Reversals: https://docs.google.com/spreadsheets/d/1IY56xdDmoU9VasdE0sry2xvsGFkWPrqbKXAs1Rw-9LA/edit#gid=0"
  , "Bartoš, F., & Schimmack, U. (2020). Z-Curve.2.0: Estimating Replication Rates and Discovery Rates. Advance online publication. https://doi.org/10.31234/osf.io/urgtn"
  , "Hoyningen-Huene, P. (2013). Systematicity: The nature of science. Oxford studies in philosophy of science. Oxford Univ. Press."
  , "LeBel, E. P., Vanpaemel, W., Cheung, I., & Campbell, L. (2019). A brief guide to evaluate replications. Meta-Psychology, 3."
  , "Open Science Collaboration (2015). Psychology: Estimating the reproducibility of psychological science. Science (New York, N.Y.), 349(6251), aac4716. https://doi.org/10.1126/science.aac4716"
  ,  sep = "<br/><br/>- "))


references_redpublications <- HTML(paste("<br/><br/><br/><h4><b>Publications Using FReD</b><h5>"
                                         , sep = ""))

references_list_redpublications <- HTML(paste(
  "- Röseler, L., Kaiser, L., Doetsch, C., Klett, N., Seida, C., Schütz, A., Aczel, B., Adelina, N., Agostini, V., Alarie, S., Albayrak-Aydemir, N., Aldoh, A., Al-Hoorie, A. H., Azevedo, F., Baker, B. J., Barth, C. L., Beitner, J., Brick, C., Brohmer, H., Chandrashekar, S. P., Chung, K. L., Cockcroft, J. P., Cummins, J., Diveica, V., Dumbalska, T., Efendic, E., Elsherif, M., Evans, T., Feldman, G., Fillon, A., Förster, N., Frese, J., Genschow, O., Giannouli, V., Gjoneska, B., Gnambs, T., Gourdon-Kanhukamwe, A., Graham, C. J., Hartmann, H., Haviva, C., Herderich, A., Hilbert, L. P., Holgado, D., Hussey, I., Ilchovska, Z. G., Kalandadze, T., Karhulahti, V.-M., Kasseckert, L., Klingelhöfer-Jens, M., Koppold, A., Korbmacher, M., Kulke, L., Kuper, N., LaPlume, A., Leech, G., Lohkamp, F., Lou, N. M., Lynott, D., Maier, M., Meier, M., Montefinese, M., Moreau, D., Mrkva, K., Nemcova, M., Oomen, D., Packheiser, J., Pandey, S., Papenmeier, F., Paruzel-Czachura, M., Pavlov, Y. G., Pavlović, Z., Pennington, C. R., Pittelkow, M.-M., Plomp, W., Plonski, P. E., Pronizius, E., Pua, A. A., Pypno-Blajda, K., Rausch, M., Rebholz, T. R., Richert, E., Röer, J. P., Ross, R., Schmidt, K., Skvortsova, A., Sperl, M. F. J., Tan, A. W. M., Thürmer, J. L., Tołopiło, A., Vanpaemel, W., Vaughn, L. A., Verheyen, S., Wallrich, L., Weber, L., Wolska, J. K., Zaneva, M., & Zhang, Y. (2024). The Replication Database: Documenting the Replicability of Psychological Science. Journal of Open Psychology Data, 12: 8, pp. 1–23. DOI: https://doi.org/10.5334/jopd.101"
  , "Röseler, L. (2023). Predicting Replication Rates with Z-Curve: A Brief Exploratory Validation Study Using the FORRT Replication Database. Retrieved from https://osf.io/t7nwk"
  , "Röseler, L., Kaiser, L., Doetsch, C. A., Klett, N., Seida, C., Schütz, A., … Zhang, Y., Mr. (2024, April 11). The Replication Database: Documenting the Replicability of Psychological Science. https://doi.org/10.31222/osf.io/me2ub"
  ,  sep = "<br/><br/>- "))


packages_headline <- HTML(paste("<br/><br/><br/><h4><b>R-packages used for this App</b></h5>"
                                , sep = ""))

packages_list <- HTML(paste("<br/><br/>- ", names(sessionInfo()[["otherPkgs"]]),  sep = ""))

breaks <- HTML(paste("<br/><br/>",  sep = ""))
