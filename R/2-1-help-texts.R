# This script saves the help texts used throughout the Shiny app

# Edit this text to change what's displayed in the help portion of the landing page
home_text <- HTML(
  "
  <h5><div>
  <p>
    The Epilepsy Multiplatform Variant Prediction (EpiMVP) project is an NIH-sponsored Center Without Walls (CWOW) which aims to develop a modular, highly integrated platform to resolve variants of uncertain significance (VUS) in a subset of genes implicated in the rare pediatric epilepsies.
  </p>
    
  <p>
    The Gene Variant and Curation Core (GVCC) integrates data from clinical genetic data and functional readouts from EpiMVP and utilizes the power of machine learning to create a computational tool called EpiPred. EpiPred is currently only available for the gene STXBP1 but will be expanded to other epilepsy-related genes soon. 
  </p>
    
  <p>
    In the ‘Check my Variant’ tab you can query individual missense variants that you may have obtained as part of a clinical or research-based genetic test. Once you enter in a variant, the EpiPred prediction, i.e. more likely pathogenic or benign, will be returned.
  </p>
  
  <p>
    In the ‘For researchers’ tab we have calculated EpiPred scores for all possible missense variants in STXBP1 and provide these scores in comparison to a variety of other variant effect predictors and STXBP1 features.
  </p>
  
  <p>  
    The manuscript detailing this work is available here and all code is freely available at the Carvill Lab Github.
  </p>
  
  <p>
    This study was sponsored by the NIH EpiMVP CWOW NINDS U54 NS117170
  </p>
  
  <p>
    <small><small>
      Disclaimer: The information on this website is not intended for direct genetic and clinical diagnostic use or medical decision-making. We recommend review of any information provided by this website with a genetics professional. Individuals should not change their health behavior on the basis of information contained on this website. Access to general information is provided for educational purposes only. You are liable or responsible for any advice, course of treatment, diagnosis or any other information, services or product obtained through this site.
    </small></small>
  </p>
  
  </div>
  </h5>"
)

# Edit this to change the help text in the "For Patients" tab
patients_help_text <- HTML(
  "
  <p>
    In this tab, you can search for Amino Acid (AA) sequences from epilepsy-associated genes with a missense variant (mutation).
    The output is an EpiPred raw score and prediction on the sequence's pathogenicity (whether it is likely to cause epilepsy).
  </p>
  
  <strong>What is a missense variant?</strong>
  
  <p>
    A missense variant is a type of mutation in which a change in your DNA sequence can cause an amino acid in a protein to change to a different one.
    This type of mutation can be benign or pathogenic, depending on the location of the mutation and the resulting change in the protein's function.
  </p>
  
  <strong>What is EpiPred?</strong>
  
  <p>
    EpiPred is a machine learning model specifically trained for classification of missense variants as likely pathogenic or benign.
  </p>
  
  <p>
    If you already have a gene and its AA sequence ID in mind, you can input your sequence in the box below.
    To change settings for the visualizations, click on the gear icon next to the \"Result Viewer\".
  </p>
  
  <p>
    <strong>Guideline for Classification Interpretation:</strong>
    <ul>
      <li>Likely Benign</li>
      <li>Possibly Benign</li>
      <li>Possibly Pathogenic</li>
      <li>Likely Pathogenic</li>
  </p>
  "
)

select_mutation_help_text <- 
  "
  If the Amino Acid sequence has multiple variants, you can choose the 
  specific variant ID here.
  "

sequence_info_help_text <- 
  "
  Your sequence's information is displayed here.
  "

# Help text for the color bar
colorbar_help_text1 <- 
  "
  Predicted score and class of your amino acid is displayed here.
  "

# Help text for the NGL Viewer
protein_help_text <- 
  "
  3-D rendering of the protein (from selected gene) is shown here.\n
  Location of the mutation is highlighted by a white blob.
  "

# Help text for the distribution plot
distr_help_text <- 
  "
  The distribution of the EpiPred score across all possible sequences in the gene is shown here.
  "

# Help text for the "For Researchers" tab
allvar_help_text <- HTML(
  "
  <p>In this tab, you can explore across different genes and compare positions and various scores.
  Use the control on the left to select the gene explore relationships.</p>
     
  <p>The table below shows all the missense variants in the gene. You can filter the table by EpiPred class
  and Reported source. Clicking on a row highlights the mutation in the above plot.</p>
  
  <p>You can also download the table by clicking the download button below the table.</p>"
)

