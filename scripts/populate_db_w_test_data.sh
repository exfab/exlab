#!/bin/bash
#
# This script populates the ExLab database with comprehensive test data.
# It handles authentication and creates a realistic lab environment.
#
# Usage: ./populate_db_w_test_data.sh [BASE_URL] [ADMIN_EMAIL] [ADMIN_PASSWORD]
# Default: http://localhost:8080 admin@exlab.com admin123

BASE_URL=${1:-"http://localhost:8080"}
EMAIL=${2:-"admin@exlab.com"}
PASSWORD=${3:-"admin123"}
COOKIE_FILE="test_cookies.txt"

echo "--- Logging in to ExLab ---"
curl -s -X POST "${BASE_URL}/api/v1/login" \
     -H "Content-Type: application/json" \
     -d "{\"email\": \"$EMAIL\", \"password\": \"$PASSWORD\"}" \
     -c $COOKIE_FILE > /dev/null

if [ $? -ne 0 ]; then
    echo "❌ Login failed. Is the server running at $BASE_URL?"
    exit 1
fi

echo "✅ Login successful."

echo "--- Creating Sample Project ---"
curl -s -b $COOKIE_FILE -X POST "${BASE_URL}/api/v1/projects" \
     -H "Content-Type: application/json" \
     -d '{
       "name": "Synthetic Biology Project Alpha",
       "description": "A high-throughput study of genetic circuits in E. coli.",
       "owner": "'$EMAIL'",
       "status": "Active"
     }' > /dev/null

echo "--- Creating Result Definitions ---"
# OD600
curl -s -b $COOKIE_FILE -X POST "${BASE_URL}/api/v1/result-definitions" \
     -H "Content-Type: application/json" \
     -d '{
       "short_id": "OD600",
       "name": "Optical Density",
       "description": "Standard OD at 600nm wavelength.",
       "data_type": "Float",
       "unit": "AU"
     }' > /dev/null

# Fluorescence
curl -s -b $COOKIE_FILE -X POST "${BASE_URL}/api/v1/result-definitions" \
     -H "Content-Type: application/json" \
     -d '{
       "short_id": "GFP",
       "name": "GFP Fluorescence",
       "description": "Measured green fluorescence intensity.",
       "data_type": "Float",
       "unit": "RFU"
     }' > /dev/null

echo "--- Creating Strains ---"
# Strain 1
curl -s -b $COOKIE_FILE -X POST "${BASE_URL}/api/v1/strains" \
     -H "Content-Type: application/json" \
     -d '{
       "genus": "Escherichia",
       "species": "coli",
       "strain_name": "MG1655",
       "genotype": "F- lambda- ilvG- rfb-50 rph-1",
       "notes": "Wild-type lab strain."
     }' > /dev/null

# Strain 2
curl -s -b $COOKIE_FILE -X POST "${BASE_URL}/api/v1/strains" \
     -H "Content-Type: application/json" \
     -d '{
       "genus": "Escherichia",
       "species": "coli",
       "strain_name": "Circuit-01",
       "genotype": "MG1655 + pRS-GFP",
       "parent_strain_id": 1,
       "notes": "MG1655 harboring a GFP expression plasmid."
     }' > /dev/null

echo "--- Creating Labware Product ---"
curl -s -b $COOKIE_FILE -X POST "${BASE_URL}/api/v1/products" \
     -H "Content-Type: application/json" \
     -d '{
       "name": "Greiner 96-well Plate",
       "brand": "Greiner Bio-One",
       "manufacturer_part_number": "655090",
       "description": "Black-walled, clear bottom 96-well microplate."
     }' > /dev/null

echo "--- Creating Plate ---"
curl -s -b $COOKIE_FILE -X POST "${BASE_URL}/api/v1/plates" \
     -H "Content-Type: application/json" \
     -d '{
       "name": "Expression Assay Plate 01",
       "project_id": 1,
       "product_id": 1,
       "plate_format": "96-well"
     }' > /dev/null

echo "--- Creating Samples (Bulk) ---"
curl -s -b $COOKIE_FILE -X POST "${BASE_URL}/api/v1/projects/1/samples/bulk" \
     -H "Content-Type: application/json" \
     -d '{
       "samples": [
         {"sample_type": "Glycerol Stock", "category": "Source", "strain_id": 2},
         {"sample_type": "Liquid Culture", "category": "Experimental", "parent_sample_id": 1}
       ]
     }' > /dev/null

echo "--- Mapping Sample to Plate Well ---"
curl -s -b $COOKIE_FILE -X PATCH "${BASE_URL}/api/v1/plates/1/wells/A1" \
     -H "Content-Type: application/json" \
     -d '{"sample_id": 2}' > /dev/null

echo "--- Logging Initial measurement ---"
curl -s -b $COOKIE_FILE -X POST "${BASE_URL}/api/v1/samples/2/results" \
     -H "Content-Type: application/json" \
     -d '{
       "result_definition_id": 1,
       "value": { "type": "Float", "value": 0.05 }
     }' > /dev/null

rm $COOKIE_FILE
echo "✅ Database population complete! Visit $BASE_URL to explore the data."
