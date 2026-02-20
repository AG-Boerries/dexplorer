# RNA-Seq Data Analysis App

## Inspiration

[DESeq shiny app](https://tgirke.shinyapps.io/systemPipeShiny/)

[Integrated Differential Expression & Pathway Analysis](https://bioinformatics.sdstate.edu/idep/)

[Shiny app contest winner 2024](https://montreal.curbcut.ca/?)

[esquisse: interactive creating of a plot based on ggplot2](https://dreamrs.github.io/esquisse/)

[shinyHeatmaply](https://github.com/yonicd/shinyHeatmaply)

[d3heatmap](https://github.com/talgalili/d3heatmap)

[HeatmapR](https://dillonhammill.github.io/HeatmapR/)

## Meetings

### 2025-10-02 (Melanie, Ahmad, Max, Katha, Tobi)

- Show in the QC plots, what is good quality of the data?
- **@Tobi:** Create a document with the required data format for the app. What shall be done by the bioinformatician, what by the app?
- How and where will the app be hosted? Or shall it be a locally installable version? **@Tobi:** What are the pros and cons of these options, what are further options?

### 2025-11-17 (Melanie, Ahmad, Max, Katha, Tobi)

- For the demo, remove the not working data sets.
- **Done:** Remove sample filtering in PCA as there is no recalculation and it would be wrong otherwise.
- Add the actual colors to the color picker or even better use esquisse R package for generating ggplots.
- **Done:** Top-scoring genes: allow user to select if x shall be p-value or log2FC (good use case for esquisse).
- There will be one user for all UcarE members, which they share. If the app is used in other SFBs, then they get a different user.
- Check again with Tom and Geritt the deployment strategy, as this was mainly agreed on.

#### 2025-11-19 (UcarE Meeting)

- **Done:** Upload list of genes for the heatmap. These genes could also be highlighted in the volcano plot.
- Let user define download size of images and type of image (png, pdf, ...).

#### 2026-01-09 (IBSM group meeting)

- Results of the bioinformatics tools usage at IBSM survey:
![](presentations/IBSM_survey_bioinf-tools/rna_seq_survey_summary.png)
- Maybe re-think the deployment strategy, when there are users outside of any SFB, it would then make sense to set up a database with user management, this can also allow to create an overall instance, with all data for IBSM staff only.
- How willingly do user share their data within a SFB?
- Further feature ideas:
    - Multiple volcano plots at once to be able to compare highlighted genes in different comparisons.
    - Heatmap:
        - Add heatmap grouping?
        - Remove dendogram?
        - Z-scoring instead of cpm
        - Scoring, centering, etc... is done before visualization, so also this is no longer correct, when removing samples from the heatmap, maybe this requires selective recalculation when samples are removed?
    - GSEA:
        - Add more collections (GO, MSigDB, ...)
        - Allow users to create heatmap from gene sets directly (same for highlighted genes in volcano plot)

## Deployment

- Melanie does not want a local version, thus this will become a web app.
- Deployment route: 
    - VM running.
    - Create and test Docker container locally.
    
    ```{.bash}
        # Build the container
        docker build . -t dexplorer:latest

        # Run the container and forward the port
        docker run -p 8080:1234 --expose 1234 dexplorer:latest
    ```

    - Copy the entire repository to the VM using `sftp` (eventually, the repo can be pulled from GitLab once the VM has access to it).
    - Run `docker compose up -d` on the VM to start the container in detached mode, with all services for the different SFBs needed.
    - Eventually NGinx will handle the correct mapping to the correct container for the user, who logged in via Basic auth.
        - Basic auth stores the login credentials and the mapping only works after clearing browser cache.

## Publication

- Together with the `dexpreprocessr` package, the story can be: 
    - The [app can be part of the R package](https://medium.com/@msn.asg/how-to-create-an-r-package-with-integrated-shiny-apps-58151a9ccb68) and can be run with a command form an R script, like in the `InteractiveComplexHeatmap` with `interactivate()` but in this case the app is started and shows the data.
        - Furthermore, the app can be made accessible on our VM and as a docker container or even as an installable version?
    - Just an explorer for own and pre-calculated results (which are the ones that we can ship with the app?).
    - Allow to upload read counts after alignment, do pre-processing using `dexpreproessr` and then explore the results.
    - Link to several different databases to get more information, maybe also to publications with similar results via text mining (or AI?). So that this can eventually become a hub for RNA-Seq data exploration like cBioPortal for cancer genomics.
    - Can this be used for cross dataset comparisons and meta analyses? But what about batch effects in this case?
    - Similar approaches are: [iDEP](https://bioinformatics.sdstate.edu/idep/) and [systemPipeShiny](https://tgirke.shinyapps.io/systemPipeShiny/)
- Possible journals:
    - [Bioinformatics](https://academic.oup.com/bioinformatics) as an application note, see the [author guidelines](https://academic.oup.com/bioinformatics/pages/author-guidelines#section-17-11).
    - [F1000Research](https://f1000research.com) might be a suitable journal (Geoffroy uses it for the technical part of Proximap).
    - [Journal of Open Source Software (JOSS)](https://joss.theoj.org/) would also be nice, [but is not yet indexed](https://www.fz-juelich.de/en/rse/the_latest/should-i-publish-in-the-journal-of-open-source-software).
    - [BMC Bioinformatics](https://link.springer.com/journal/12859).

### Data Pre-Processing (PCA/RLDF and excluding samples)

- For the pre-processing, we actually need a standardized way, for instance in a form of a package or pipeline.
- We may or may not show samples, which were excluded from the analysis. As these samples were excluded for good reasons and should not be used for the analysis, it may make more sense to not show them. We could mention on that in the additional data set information.
- Advantage of providing pre-calculated results and disabling calculations from the user side:
    - In rare cases you can have specific setups that require additional effort to analyze the data correctly which cannot easily be done with a standard pipeline (example Severin with Zebrafish
wound healing).
    - If there are no well-defined controls, or maybe also in other cases, you can have confounded genes in some of the contrasts. If we pre-calculate the results, we can directly mark these genes as confounders. If the users calculate the DEGs, they may not be aware of potential confounders and get misleading results.
- Disadvantages of providing pre-calculated results:
    - More data storage required.
    - Maybe the users feel more comfortable when they can actually ‘do’ some things by themselves.

### Differential Gene Expression Analysis

-   If we want to allow the users to select any contrast they want, we can just as well pre-compute any contrast possible and provide the results directly.
    -   When all contrasts are pre-computed, we can set up the UI in such a way that the user can select *group a* vs *group b*, or vice versa to give them full flexibility on how the plot will look like.
-   How to deal with unknown genomic regions, e.g. no identifier?
    -   We can do the analysis for all the genomic regions, and implement a UI element, which allows showing or hiding all regions, which are not well-defined (without Entrez ID).
        -   This is preferred because it avoids p-value cherry-picking.
    -   Or we can do one analysis with all genomic regions and one analysis without the undefined regions and implement a UI element that allows to show either of the both.
-   P-value and log fold change cutoffs can be visualized in all figures.
    -   This may also be turned on and off by the user.
    -   We could allow the user to change the thresholds.

## Conclusion

Katha: I think we should find & finalize a version with minimum potential of re-calculations from the user side since this seems to be the most stable approach at the moment, and wait for a few feedbacks in order to figure out additional needs & adapt.