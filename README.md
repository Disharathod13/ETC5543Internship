### **Project Title**:
Forecasting Year 12 Graduates for Monash’s Low SES Enrollment Targets

### **Author**:
Disha Rathod  
**Phone**: (04) 0328 1394  
**Email**: drat0009@student.monash.edu  
**Organization**: Intelligence and Insights Department of Monash


### **Project Overview**
This project involves a demographic and socio-economic analysis aimed at forecasting Year 12 graduate populations in Victoria, particularly focusing on low socio-economic status (SES) regions. Using Australian Bureau of Statistics (ABS) Census data and R programming tools, the study aims to guide Monash University in achieving the Australian government's target of 20% university enrollments from low SES backgrounds.

### **Objective**
The primary goal of the analysis is to support Monash University in:
1. Forecasting Year 12 graduate populations from 2021 to 2030.
2. Identifying regions with high socio-economic disadvantages.
3. Providing data-driven insights to inform strategies aimed at increasing low SES enrollment.

### **Data Sources**
1. Australian Bureau of Statistics (ABS) 2021 Census Data
2. Socio-Economic Indexes for Areas (SEIFA) data from ABS
3. VTAC and internal demographic records for Victoria (if integrated in future work)

### **Methodology**
1. **Data Collection & Preprocessing**:
   - Utilized ABS Census 2021 data for demographic details.
   - Incorporated SEIFA data to identify socio-economic status.
   - Cleaned, merged, and processed data using `tidyverse` tools in R.
2. **Forecasting**:
   - Projected Year 12-ready populations for 2021-2030 using three-year rolling averages.
3. **Geospatial Analysis**:
   - Mapped SES data using `sf` and `ggplot2` in R for visual representation.
   - Focused on Victoria’s Statistical Areas (SA1, SA3).
4. **Visualization**:
   - Created line graphs, bar charts, and detailed maps to highlight trends.

### **Usage Instructions**
1. **Dependencies**:
   - R version 4.0+ 
   - RStudio (Recommended)
   - Required R packages: `tidyverse`, `sf`, `ggplot2`, `zoo`, `readxl`
   - Install all dependencies using `install.packages()`.

2. **Running the Analysis**:
   - Ensure all datasets are placed in the `../Data/` directory.
   - Open the R Markdown file and knit to PDF to generate the full report.
   - For visualizations, the plots will automatically render in the output.

3. **Files**:
   - **R Markdown File**: Contains the report, including data processing, analysis, and visualizations.
   - **Data Folder**: Contains CSV/Excel files from ABS and related sources but excludes large files.
   - **Bibliography File**: `references.bib` containing all cited references.

### **Expected Outputs**
- **PDF Report**: A scholarly document detailing methodology, findings, and recommendations.
- **Visualizations**: Line graphs, SES status maps, trend graphs by region, etc.
- **Dataset Summary**: Cleaned datasets in R for further analysis.

### **Notes & Limitations**
- The analysis is based on the 2021 Census; updating it with more recent data is recommended.
- Projections assume stable socio-economic trends; any sudden changes in policy, migration, or economics could affect results.
- Future work may include integrating VTAC data for a deeper analysis.

### **Acknowledgements**
- Supervision by Patrick Leung, Intelligence and Insights Unit, Monash University.
- Data provided by ABS and internal Monash records.

### **Contact**
For any queries or further information, please contact Disha Rathod at drat0009@student.monash.edu.

