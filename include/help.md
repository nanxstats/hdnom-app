
# Getting started with hdnom.io

This web application helps you build penalized Cox models for high-dimensional data with survival outcomes. All the 9 types of model included in the _hdnom_ package are supported. It streamlined the process of nomogram building, model validation, calibration, comparison, and reproducible report generation --- all done inside your web browser.

Please read the <a href="https://github.com/road2stat/hdnom-policy" target="_blank">data privacy policy</a> first before you start using this app.

The following workflow is recommended:

 1. Click **Data** to upload your dataset;
 2. Specify the parameters for building models in the **Nomogram** tab;
 3. Validate and calibrate the built model internally using **Internal Validation** and **Internal  Calibration** under the **Model Validation** tab;
 4. Perform Kaplan-Meier analysis and log-rank test for the risk groups of internal calibration using **Kaplan-Meier Analysis for Internal Calibration**;
 5. (Optional.) Validate and calibrate the built model _with external datasets_ using **External Validation** and **External Calibration** under the **Model Validation** tab;
 6. (Optional.) Perform Kaplan-Meier analysis and log-rank test for the risk groups of external calibration using **Kaplan-Meier Analysis for External Calibration**;
 7. (Optional.) Compare the models via **by Validation** and **by Calibration** under the **Model Comparison** tab;
 8. (Optional.) Predict overall survival probability for new samples based on the built model using the **Prediction** tab;
 9. Finally, you will be able to download the PDF/HTML/Word reports containing the computation results, and the R model object in the **Report** tab.

Notes:

 * To generate the basic report, you need to at least do 1, 2, 3, and 4;
 * To generate the external validation report, at least do 1, 2, 5, and 6;
 * To generate the model comparison report, at least do 1 and 7;
 * To download the R model object, at least do 1 and 2.
 * After downloaded the model object, it might be interesting to try <a href="https://github.com/road2stat/hdnom-appmaker" target="_blank">hdnom appmaker</a> to make your own nomogram-based online prediction app.

### Feedback

If you have any questions, suggestions, or ideas about the web app, please feel free to let us know:

Miaozhu Li <<miaozhu.li@duke.edu>><br>
Nan Xiao <<nanx@uchicago.edu>>

### Citation

To cite the _hdnom_ package or the web application in publications, use:

<p><em>
Miaozhu Li and Nan Xiao (2015). hdnom: Nomograms for High-Dimensional Cox Models. R package version 3.0. <a href="https://cran.r-project.org/package=hdnom" target="_blank">https://cran.r-project.org/package=hdnom</a>
</em></p>

<hr>
<p class="text-muted">
© 2015 - 2016 <a href="http://miaozhu.li" target="_blank">Miaozhu Li</a> & <a href="http://nanx.me" target="_blank">Nan Xiao</a> · <a href="http://hdnom.org" target="_blank">The hdnom Project</a> · <a href="https://github.com/road2stat/hdnom-policy" target="_blank">Data Privacy Policy</a>
</p>
