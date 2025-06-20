## Changelog ---------------------------------------------------------------
changelog <- HTML(paste("<h3><b>Changelog</b></h3><h5>"
                        , "<i>This is a list of changes for the FReD-website and the dataset.</i>"

                        # TEMPLATE FOR NEW VERSIONS
                        # , "</br></br><b>dd.mm.yyyy</b>"
                        # , "</br><i>App version: Version 0.4.xx</i>"
                        # , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        # , "</br>- ..."

                        , "</br></br><b>20.06.2025</b>"
                        , "</br><i>Fixed several issues. For future changes, please see the Github project: https://github.com/forrtproject/FReD</i>"

                        , "</br></br><b>17.04.2024</b>"
                        , "</br><i>App version: Version 0.4.6</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Corrected computation of variable 'result' (sign of the effect size was ignored when evaluating success)."
                        , "</br>- Corrected computation of variable 'consistent' (NAs were erroneously counted as significant effects)."
                        , "</br>- Removed global dataset download link from Dataset tab and added OSF-link (website_text.R)."
                        , "</br>- Removed global dataset download link from Dataset tab (ui.R)."
                        , "</br>- Added options for download of the filtered datatable in the Dataset tab (server.R)."

                        , "</br></br><b>08.02.2024</b>"
                        , "</br><i>App version: Version 0.4.5</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Added option to choose Data Replicada or OpenMKT studies in the Replicability Tracker."
                        , "</br>- ."

                        , "</br></br><b>06.02.2024</b>"
                        , "</br><i>App version: Version 0.4.4</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Updated author list (ReD and Reversals lists have now been merged to FReD)"
                        , "</br>- Removed the contributions list due to the new underlying table that is available here: https://docs.google.com/spreadsheets/d/1x68oW2H_Xrdv44fIeycl4fegsmQgCa60GxeZZ_hAR90/edit?pli=1#gid=1234999412 (Contributors FReD)."
                        , "</br>- Added the option to browse Many Labs 2 and SSRP studies."
                        , "</br>- Added 'FORRT' to Replication Database throughout the text."
                        , "</br>- Added information on funding (About tab)."
                        , "</br>- Corrected an error that prevented the FReD logo to be displayed (About tab at the bottom of the page)"
                        , "</br>- Corrected an error in the power calculation where power was only calculated if the original sample size instead of the replication sample size was available."

                        , "</br></br><b>02.02.2024</b>"
                        , "</br><i>App version: Version 0.4.3</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Added information on funding."
                        , "</br>- Added FORRT to the name."
                        , "</br>- Corrected a header name."

                        , "</br></br><b>05.01.2024</b>"
                        , "</br><i>App version: Version 0.4.22</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Included FORRT entries and Additional Studies (i.e., studies with effect sizes not yet coded) in the correlates and reference checker tabs."
                        , "</br>- v 0.4.21: Reference checker is now also specifying the number of publications besides the number of replication findings."
                        , "</br>- v 0.4.22: Added the option to include uncoded entries in the replicability tracker."
                        , "</br>- v 0.4.22: Removed 'corrected ERR' from z-curve."

                        , "</br></br><b>06.12.2023</b>"
                        , "</br><i>App version: Version 0.4.1</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Fixed filtering function via datatable in the tab Replicability Tracker. The reversed order of the table led to errors."
                        , "</br>- Added dynamic pointsize to the scatterplot in the Replicability Tracker."

                        , "</br></br><b>24.11.2023</b>"
                        , "</br><i>App version: Version 0.4.0</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Added FORRT-logos."
                        , "</br>- Overhauled References Checker to work via DOIs as DOIs have been assigned to a large proportion of the database."
                        , "</br>- Users can now filter data in the replication tracker via the Datatable. Selection of rows is no longer possible in the table."
                        # , "</br>- Added corrected ERR to Z-Curve (https://osf.io/preprints/metaarxiv/ewb2t)."

                        , "</br></br><b>23.11.2023</b>"
                        , "</br><i>App version: Version 0.0.3.21</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Increased forest plot height."
                        , "</br>- Moved downloadfunctions from Summarizer and removed the Summarizer tab."


                        , "</br></br><b>26.10.2023</b>"
                        , "</br><i>App version: Version 0.0.3.1</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Added new variables (discipline and effect)"
                        , "</br>- Added >1K unvalidated entries from FORRT. These are already included in the downloadable dataset and the correlates (highlighted as 'not yet coded')"

                        , "</br></br><b>17.10.2023</b>"
                        , "</br><i>App version: Version 0.3.1</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Added first version of a dynamic 'Summarizer'"


                        , "</br></br><b>16.10.2023</b>"
                        , "</br><i>App version: Version 0.3.0</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Corrected text about the number of independent replication findings (used a deprecated ID variable and is now using the reference)."
                        , "</br>- Changed the order of the table under Replicability Tracker so that the newest entries are displayed on top"
                        , "</br>- Changed the color of mixed results in the Correlates tab to yellowish instead of blue so that inconclusive and mixed are different colors."
                        , "</br>- Removed faulty rows from the aggregated data used for the Correlates tab (e.g., where a journal was named 'no signal')"
                        , "</br>- Formatted references and added a section for publications using ReD"

                        , "</br></br><b>02.10.2023</b>"
                        , "</br><i>App version: Version 0.2.91</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Changed mail adress."


                        , "</br></br><b>19.04.2023</b>"
                        , "</br><i>App version: Version 0.2.9</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Added function to check which elements from a list of references are included among the original studies."
                        , "</br>- Updated Replicability Tracker info text."

                        , "</br></br><b>18.04.2023</b>"
                        , "</br><i>App version: Version 0.2.8</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Corrected info text in the correlates tab."
                        , "</br>- Increased height of the Replicability by Journal plot."

                        , "</br></br><b>17.04.2023</b>"
                        , "</br><i>App version: Version 0.2.7</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Updated contact info."
                        , "</br>- Updated replication tracker text to include number of original studies."

                        , "</br></br><b>13.04.2023</b>"
                        , "</br><i>App version: Version 0.2.6</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Added replication power to moderator analyses."

                        , "</br></br><b>12.04.2023</b>"
                        , "</br><i>App version: Version 0.2.5</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Updated moderator analyses (alpha)."

                        , "</br></br><b>11.04.2023</b>"
                        , "</br><i>App version: Version 0.2.4</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Updated FAQ."

                        , "</br></br><b>06.04.2023</b>"
                        , "</br><i>App version: Version 0.2.3</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Updated contact info."
                        , "</br>- Updated FAQ."

                        , "</br></br><b>05.04.2023</b>"
                        , "</br><i>App version: Version 0.2.2</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Changed \'Effect Sizes Comparison\' tab to \'Study Overview\' tab to be centered around original studies' references instead of descriptions. Filtering for this tab is currently disabled."
                        , "</br>- Changed correlates of replicability to be centered around original studies instead of replication findings to prevent \'overweighing\' through studies that were replicated a large number of times (e.g., RRRs)."

                        , "</br></br><b>03.04.2023</b>"
                        , "</br><i>App version: Version 0.2.1</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/z5u9b)</i>"
                        , "</br>- Updated FAQs (data structure)."
                        , "</br>- Added preliminary moderator analyses."
                        , "</br>- Updated file link."
                        , "</br>- Corrected number-parsing for correlates of replicability (decade)."

                        , "</br></br><b>30.03.2023</b>"
                        , "</br><i>App version: Version 0.1.2</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/dysqm)</i>"
                        , "</br>- Updated FAQs."

                        , "</br></br><b>28.03.2023</b>"
                        , "</br><i>App version: Version 0.1.2</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/dysqm)</i>"
                        , "</br>- Excluded direct downloading of the submission portal."
                        , "</br>- Added some flexibility to computing effect sizes."
                        , "</br>- Changed appearance of selected points in the scatterplot."
                        , "</br>- Added observed replication rate to z-curve."

                        , "</br></br><b>21.03.2023</b>"
                        , "</br><i>App version: Version 0.1.1</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/dysqm)</i>"
                        , "</br>- Minor changes to wording"
                        , "</br>- Corrected error that led to id-variable being dropped"
                        , "</br>- Added FAQ on overlap with FORRT's R&R"

                        , "</br>- Reworked info text and added link to the submission portal"
                        , "</br></br><b>20.03.2023</b>"
                        , "</br><i>App version: Version 0.1.1</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/dysqm)</i>"
                        , "</br>- Minor changes to Google Spreadsheet Dataset"
                        , "</br>- Reworked info text and added link to the submission portal"
                        , "</br>- Modified main dataset to include new variables from submission portal"
                        , "</br>- Deleted subject variable that was only available for a very small subset of entries"
                        , "</br>- Authors and contributions are now automatically created from the Authors tab in the Google Spreadsheet"

                        , "</br></br><b>15.03.2023</b>"
                        , "</br><i>App version: Version 0.1.0</i>"
                        , "</br><i>Dataset name: Red.xlsx (from osf.io/dysqm)</i>"
                        , "</br>- Base dataset has been changed to a version with additional (optional) variables."
                        , "</br>- A large proportion fo the base dataset has been validated."
                        , "</br>- Added interpretation of replication result as suggested by Lebel et al., 2018."

                        , "</br></br><b>10.03.2023</b>"
                        , "</br><i>App version: Version 0.0.2.5</i>"
                        , "</br><i>Dataset name: Red.csv (from osf.io/2a3gb)</i>"
                        , "</br>- Updated logo."
                        , "</br>- Restricted inclusion of submission portal entries to prevent App from braking."
                        , "</br>- Added FAQ."

                        , "</br></br><b>07.03.2023</b>"
                        , "</br><i>App version: Version 0.0.2.4</i>"
                        , "</br><i>Dataset name: Red.csv (from osf.io/2a3gb)</i>"
                        , "</br>- Data from the submission form will now be automatically included."
                        , "</br>- Data from the submission form will by default be highlighted as not validated."
                        , "</br>- Updated references."
                        , "</br>- Updated design."

                        , "</br></br><b>06.03.2023</b>"
                        , "</br><i>App version: Version 0.0.2.3</i>"
                        , "</br><i>Dataset name: Red.csv (from osf.io/2a3gb)</i>"
                        , "</br>- Two new co-authors have been added."
                        , "</br>- A newly created submission portal has been linked and info texts have been updated."

                        , "</br></br><b>24.01.2023</b>"
                        , "</br><i>App version: Version 0.0.2.2</i>"
                        , "</br><i>Dataset name: Red.csv (from osf.io/2a3gb)</i>"
                        , "</br>- Added Z-Curve analysis."
                        , "</br>- Polished the scatter- and barplots."
                        , "</br>- Added descriptions and explanations to some of the plots."
                        , "</br>- Added a References tab with articles referenced in the descriptions and a list of used R-packages."
                        , "</br>- Fixed a bug in the barplot that led to incorrect percentages."


                        , "</br></br><b>19.01.2023</b>"
                        , "</br><i>App version: Version 0.0.2.1</i>"
                        , "</br><i>Dataset name: Red_220627.csv</i>"
                        , "</br>- Added option to filter by replication power."
                        , "</br>- Data processing has been moved to an individual R-script. The App is now downloading the dataset directly from the OSF repository."

                        , "</br></br><b>19.01.2023</b>"
                        , "</br><i>App version: Version 0.0.2.0</i>"
                        , "</br><i>Dataset name: Red_220627.csv</i>"
                        , "</br>- Fixed minor error."

                        , "</br></br><b>16.08.2022</b>"
                        , "</br><i>App version: Version 0.0.1.9</i>"
                        , "</br><i>Dataset name: Red_220627.csv</i>"
                        , "</br>- Fixed minor error."

                        , "</br></br><b>27.06.2022</b>"
                        , "</br><i>App version: Version 0.0.1.8</i>"
                        , "</br><i>Dataset name: Red_220627.csv</i>"
                        , "</br>- Updated authors."
                        , "</br>- Updated data."

                        , "</br></br><b>22.06.2022</b>"
                        , "</br><i>App version: Version 0.0.1.8</i>"
                        , "</br><i>Dataset name: ReD_220622.csv</i>"
                        , "</br>- Added option to filter by source."
                        , "</br>- Changed size of plots."
                        , "</br>- Added studies from individual submissions."

                        , "</br></br><b>21.06.2022</b>"
                        , "</br><i>App version: Version 0.0.1.7</i>"
                        , "</br><i>Dataset name: ReD_220621.csv</i>"
                        , "</br>- Fixed error in the blobbogram (missing error bars for original effect sizes."
                        , "</br>- Updated data (work in progress)."
                        , "</br>- Corrected authors."

                        , "</br></br><b>17.06.2022</b>"
                        , "</br><i>App version: Version 0.0.1.7</i>"
                        , "</br><i>Dataset name: ReD_220617.csv</i>"
                        , "</br>- Updated data (work in progress)."
                        , "</br>- Removed studies that did not report whether the replication was successful or not."
                        , "</br>- Updated overview plot visuals."
                        , "</br>- Updated blobbogram to highlight selected (previously: remove unselected)."
                        , "</br>- Corrected author contributions."

                        , "</br></br><b>14.06.2022</b>"
                        , "</br><i>App version: Version 0.0.1.6</i>"
                        , "</br><i>Dataset name: ReD_220614.csv</i>"
                        , "</br>- Updated data (work in progress)."
                        , "</br>- Fixed forest plot bugs."
                        , "</br>- Added data submission template."
                        , "</br>- Closeness scorer."

                        , "</br></br><b>01.06.2022</b>"
                        , "</br><i>App version: Version 0.0.1.5</i>"
                        , "</br><i>Dataset name: ReD_220601.csv</i>"
                        , "</br>- First App draft uploaded (mock data)."

                        , "</br></br>"
                        , sep = ""))
