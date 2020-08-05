
# Getting started with the hdnom app

This web application helps you build penalized Cox models for high-dimensional data with survival outcomes. All the 9 types of model included in the _hdnom_ package are supported. It streamlined the process of nomogram building, model validation, calibration, comparison, and reproducible report generation --- all done inside your web browser.

Please read the <a href="https://github.com/road2stat/hdnom-doc/blob/master/privacy.md" target="_blank">data privacy policy</a> first before you start using this app.

The following workflow is recommended:

 1. Click **Data** to upload your dataset;
 2. Specify the parameters for building models in the **Nomogram** tab;
 3. Validate and calibrate the built model internally using **Internal Validation** and **Internal  Calibration** under the **Model Validation** tab;
 4. Perform Kaplan-Meier analysis and log-rank test for the risk groups of internal calibration using **Kaplan-Meier Analysis for Internal Calibration**;
 5. (Optional) Validate and calibrate the built model _with external datasets_ using **External Validation** and **External Calibration** under the **Model Validation** tab;
 6. (Optional) Perform Kaplan-Meier analysis and log-rank test for the risk groups of external calibration using **Kaplan-Meier Analysis for External Calibration**;
 7. (Optional) Compare the models via **by Validation** and **by Calibration** under the **Model Comparison** tab;
 8. (Optional) Predict overall survival probability for new samples based on the built model using the **Prediction** tab;
 9. Finally, you will be able to download the PDF/HTML/Word report containing the computation results, and the R model object in the **Report** tab.

Notes:

 * To generate the basic report, please (at least) do step 1, 2, 3, and 4;
 * To generate the external validation report, please do step 1, 2, 5, and 6;
 * To generate the model comparison report, please do step 1 and 7;
 * To download the R model object, please do step 1 and 2.
 * After downloaded the model object, it might be interesting to try the <a href="https://github.com/road2stat/hdnom-appmaker" target="_blank">hdnom application maker</a> to make your own nomogram-based online prediction app.

### Citation

To cite the _hdnom_ package or the web application in your publications,
please <a href="https://github.com/road2stat/hdnom-doc/blob/master/citation.md" target="_blank">click here</a> to view the most recent reference to use.

### Feedback

If you have any questions, suggestions, or ideas about the web application, please feel free to let us know:

 * Miaozhu Li <<http://miaozhu.li>>
 * Nan Xiao <<https://nanx.me>>

<hr>
<p class="text">
© 2015 - 2016 <a href="http://miaozhu.li" target="_blank">Miaozhu Li</a> & <a href="https://nanx.me" target="_blank">Nan Xiao</a> · <a href="https://nanx.me/hdnom/" target="_blank">The hdnom Project</a> · <a href="https://github.com/road2stat/hdnom-doc/blob/master/privacy.md" target="_blank">Data Privacy Policy</a>
</p>
