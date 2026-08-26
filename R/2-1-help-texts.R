# This script saves the help texts used throughout the Shiny app

# Edit this text to change what's displayed in the help portion of the landing page
# Reformatted for readability (post-delivery enhancement, see TODO.md) --
# same facts/wording as before, just restructured into scannable sections
# with bold/italic emphasis, bullet lists, and a few icons instead of one
# long run of plain paragraphs. Tab names below match the actual nav
# panel labels ("Check My Variant" / "For Researchers") rather than the
# original text's quoted lowercase versions, for consistency with the UI.
home_text <- HTML(
  "
  <div>

  <p>
    🧬 The <strong>Epilepsy Multiplatform Variant Prediction (EpiMVP)</strong> project is an NIH-sponsored Center Without Walls (CWOW) which aims to develop a modular, highly integrated platform to resolve <strong>variants of uncertain significance (VUS)</strong> in a subset of genes implicated in the rare pediatric epilepsies.
  </p>

  <p>
    🔬 The <strong>Gene Variant and Curation Core (GVCC)</strong> integrates data from clinical genetic data and functional readouts from EpiMVP and utilizes the power of machine learning to create a computational tool called <strong>EpiPred</strong>. We focus on five genes most implicated in epilepsy that are <em>not</em> ion channels or receptors (which tend to be more well-studied already):
  </p>
  <ul>
    <li><strong>STXBP1</strong></li>
    <li><strong>SLC6A1</strong></li>
    <li><strong>PCDH19</strong></li>
    <li><strong>SYNGAP1</strong></li>
    <li><strong>CHD2</strong></li>
  </ul>

  <p>
    📋 All variants identified in clinical genetic testing are classified by the <strong>American College of Medical Genetics (ACMG)</strong> classification criteria. Variants fall primarily into three groups:
  </p>
  <ul>
    <li><strong>PLP</strong> &mdash; pathogenic or likely pathogenic</li>
    <li><strong>BLB</strong> &mdash; benign or likely benign</li>
    <li><strong>VUS</strong> &mdash; variant of uncertain significance</li>
  </ul>
  <p>
    Below, for each gene, we show the distribution of <strong>missense variants only</strong> &mdash; i.e. those variants which switch one amino acid for another.
  </p>

  <h5>What&rsquo;s in this app</h5>
  <ul>
    <li>🔎 <strong>Check My Variant</strong> &mdash; query individual missense variants that you may have obtained as part of a clinical or research-based genetic test, for any of the five genes above. Once you enter in a variant, the EpiPred prediction, i.e. more likely pathogenic or benign, will be returned.</li>
    <li>📊 <strong>For Researchers</strong> &mdash; we have calculated EpiPred scores for all possible missense variants in each of these five genes and provide these scores in comparison to a variety of other variant effect predictors and gene-specific features.</li>
  </ul>

  <h5>Learn more</h5>
  <p>
    📄 The manuscript detailing this work is available <strong>here</strong> and all code is freely available at the <strong>Carvill Lab Github</strong>.
  </p>
  <p>
    🏛️ This study was sponsored by the NIH EpiMVP CWOW <strong>NINDS U54 NS117170</strong>.
  </p>

  <div style='margin-top:16px; padding:10px 14px; background-color:#f1f1f1; border-left:4px solid #999; border-radius:4px;'>
    <small>
      ⚠️ <strong>Disclaimer:</strong> <em>The information on this website is not intended for direct genetic and clinical diagnostic use or medical decision-making. We recommend review of any information provided by this website with a genetics professional. Individuals should not change their health behavior on the basis of information contained on this website. Access to general information is provided for educational purposes only. You are liable or responsible for any advice, course of treatment, diagnosis or any other information, services or product obtained through this site.</em>
    </small>
  </div>

  </div>"
)

# Static legend explaining the ACMG classes shown in the landing-page
# waffle plot (TODO.md Phase 2). Exact wording from
# Researcher_asks/V2 landing page.docx -- "the text in the text box will
# be static and apply to all genes, it's just an explanation of the data".
landing_legend_text <- HTML(
  "
  <div style='display:flex; flex-wrap:wrap; gap:20px; justify-content:center;'>
    <div style='flex:1; min-width:200px;'>
      <strong>VUS = variant of uncertain significance</strong>
      <p>Variant where there is insufficient evidence to determine whether a variant is pathogenic or benign.</p>
    </div>
    <div style='flex:1; min-width:200px;'>
      <strong>Conflicting</strong>
      <p>Variant has conflicting information in ClinVar, where genetic testing companies disagree on what class the variant should fall into.</p>
    </div>
    <div style='flex:1; min-width:200px;'>
      <strong>BLB = Benign or likely benign</strong>
      <p>Variant unlikely to be associated with epilepsy.</p>
    </div>
    <div style='flex:1; min-width:200px;'>
      <strong>PLP = pathogenic or likely pathogenic</strong>
      <p>Variant is likely causative of the epilepsy and/or related neurodevelopmental disorder.</p>
    </div>
  </div>
  "
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

