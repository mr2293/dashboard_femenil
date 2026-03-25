import os
import requests

# Get the Google Sheet ID from environment variable
sheet_id = os.getenv('RPE_SHEET_ID_FEM')
if not sheet_id:
    raise ValueError("RPE_SHEET_ID_FEM environment variable not set")

# URL to download Google Sheet as XLSX
url = f'https://docs.google.com/spreadsheets/d/{sheet_id}/export?format=xlsx'

# Path where the file will be saved in the repo (relative to repo root)
output_dir = 'data'
output_path = os.path.join(output_dir, 'rpe_femenil.xlsx')

# Ensure the folder exists
os.makedirs(output_dir, exist_ok=True)

# Download the file
response = requests.get(url)
response.raise_for_status()

# Overwrite the file to ensure git sees changes
with open(output_path, 'wb') as f:
    f.write(response.content)

print(f'Downloaded Google Sheet as {output_path}')
