# Mandatory Training Dashboard

This repository contains a Shiny dashboard for the **CSAC Mandatory Training Performance Report**.  

It lets you upload a prepared data and interactively generate figures required for reporting.  

⚠️ **Note:** hich is extracted from the People Analytics Dashboard dataset


Raw safeguard data is **not** included in this repository. 
You will need to update Mandatory Training data manually from the People Analytics 
Dashboard which is updated every quarter. this process is not covered in this SOP  


---

## 📦 Requirements

Before you start, request installation of the following (via Service Now if applicable):

- [R]  
- [RStudio]  
- Git (x64)  

---

## 📂 Repository Structure

[To be completed]

---

## 🚀 Setup Instructions

### 1. Clone the Repository
Open **RStudio** → `File` → `New Project` → `Version Control` → `Git`  
Enter the repository URL: **`https://github.com/OhinUKHSA/MT_dashboard.git`**
  

Choose a project directory name (e.g., `MT_dashboard`) and location.  
✅ This creates a local copy of the repo with all scripts (but not the data).  

---

### 2. Install Packages (first-time only)

1. Open **`Load_packages.R`** in RStudio.  
2. Run all lines (`Ctrl + A` → `Ctrl + Enter`).  
3. This may take a while.  
4. When complete, you should see:  

`All required packages are installed. You can run report.`  

✅ You only need to do this once.  

---

### 3. Prepare the Data

1. Navigate to `MT_dashboard/data/`.  
2. If a data file already exists, create a copy and move it to `MT_dashboard/data/old_data/`.  
   - Recommended: create a dated subfolder within `old_data/` for version control.
3. update **`MT_data.csv`** file from People Analytic Dashboard  


---

### 4. Run the Dashboard

1. Open **`Rproj_MT_dashboard.Rproj`** in RStudio.  
2. Open **`MT_dashboard/server.R`** (or `ui.R`).  
3. Click **Run App** (or press **`Ctrl`** + **`Shift`** + **`Enter`**).  
4. In the app window, click **Browse** and select the prepared Excel/CSV file in `data/`.  
5. Customise plot and toggle between tabs to view different analyses.  

---

## ⚠️ Important Notes
- Always archive the previous month’s data in `data/old_data/` before replacing it.  
- Do not edit any of the scripts.  

---

## ✅ Quick Checklist
- [ ]  Clone repo in RStudio.  
- [ ]  Run `Load_packages.R` (once).  
- [ ]  Archive old data → `data/old_data/`.  
- [ ]  Update Mandatory Training data from People Analytics PowerBi Dashboard, save in `data/`.  
- [ ]  Open Rstudio project **`Rproj_MT_dashboard.Rproj`**  → Open file → `server.R`/`ui.R` → **Run App**.  
- [ ]  Upload new file via **Browse** in the dashboard.  
- [ ]  Customise plot as needed.