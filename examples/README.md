# ExLab Example Data

This directory contains example files for testing bulk operations and data uploads in ExLab.

## 📦 Products
- `example_products.csv`: Standard CSV for bulk importing labware and reagents.

## 🧬 Strains
- `example_strains.csv`: Standard CSV for bulk importing biological strains.
- `example_strains.json`: JSON format for bulk strain creation, including support for external database links.

## 🧪 Samples
- `example_samples.csv`: CSV for bulk creating samples within a project. 
  - *Note: IDs like `parent_sample_id` and `strain_id` are database-specific and must be updated to match your environment.*

## 📊 Results
- `example_results.csv`: Standard CSV for bulk importing results against existing samples.
- `example_results_plate.csv`: Standard CSV for bulk importing results against existing plates.
  - *Note: The headers (e.g. `OD600`, `Yield`) must exactly match the `short_id` of existing Result Definitions in your database. The first column must be `sample_short_id` or `plate_short_id` depending on the target.*

## 🧫 Plate Layouts
ExLab supports three different CSV formats for updating plate layouts:

1. **Alphanumeric (`plate_layout.csv`):** A simple two-column list of `well` (e.g., A1) and `sample_short_id`.
2. **Numeric (`plate_layout_numeric.csv`):** Similar to alphanumeric, but uses 1-based integers for well positions (1-96).
3. **Matrix (`plate_layout_matrix.csv`):** A visual grid where the first row is column numbers and the first column is row letters. Ideal for researchers who manage layouts in Excel.

## 🧫 Bulk Plate Creation
- `bulk_plate_creation.csv`: A specialized 3-column CSV format used to instantly generate multiple new plates and assign samples to them in a single upload.
  - *Format requires exact headers: `plate_name,well,sample_short_id`.*
  - *Rows with the same `plate_name` are grouped together and assigned to a newly generated plate of that name.*
