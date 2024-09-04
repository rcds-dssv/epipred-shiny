# This script saves the help texts used throughout the Shiny app

# Edit this text to change what's displayed in the help portion of the landing page
home_text <- HTML(
  "
  <h5><div>
  <p>
    An The Epilepsy Multiplatform Variant Prediction project is an NIH-sponsoredCenter Without Walls (CWOW) that will develop a modular, 
    highly integrated platform approach to accelerate determination of the functional, pharmacological, neuronal network and whole animal 
    consequences of genetic variants among a range of clinical epilepsy types. Read more about <a href=https://epimvp.med.umich.edu/>EpiMVP</a>
  </p>
    
   <img src=gene2ppl.jpg alt=gene2ppl style=float:right;>
    
  <p>
    The Gene Variant and Curation Core (GVCC) integrates data from clinical genetic data and functional readouts from EpiMVP and 
    utilizes the power of machine learning to create a computational algorithm called EpiPred.
  </p>
    
  <p>
    EpiPred is currently only available for the gene <a href=https://www.ncbi.nlm.nih.gov/books/NBK396561/>STXBP1 </a> 
    but will be expanded to other epilepsy-associated genes in the future. 
    On this website you can query individual missense variants and receive a prediction score as to whether that variant is likely 
    pathogenic or benign. We have also calculated EpiPred scores for all possible missense variants in STXBP1.
  </p>
  
  <p>  
    The manuscript detailing this work is available here and all code is freely available at the EpiMVP github.
  </p>
  
  </div>
  </h5>"
)

# Edit this to change the help text in the "For Patients" tab
overall_help_text <- HTML(
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
  Use the control on the left to select the gene explore relationships</p>
     
  <p>The table below shows all the missense variants in the gene. You can filter the table by EpiPred class
  and Reported source. Clicking on a row highlights the mutation in the above plot.</p>
  
  <p>You can also download the table by clicking the download button below the table.</p>"
)

