import json
import os

def filter_data_for_fy_2021(file_path):
    with open(file_path, 'r') as file:
        data = json.load(file)
    
    try:
        shares_outstanding = data['facts']['dei']['EntityCommonStockSharesOutstanding']['units']['shares']
        fy_2021_data = [item for item in shares_outstanding if item.get('fy') == 2021]

        if any(item.get('form') == '10-Q' and item.get('fp') == 'Q1' for item in fy_2021_data):
            return [item for item in fy_2021_data if item.get('form') == '10-Q' and item.get('fp') == 'Q1']
        elif any(item.get('form') == '10-K' for item in fy_2021_data):
            return [item for item in fy_2021_data if item.get('form') == '10-K']
        else:
            return fy_2021_data  # If neither '10-Q' with 'Q1' nor '10-K' is present

    except KeyError as e:
        print(f"Key error in file {file_path}: {e}")
        return []  

def process_json_files(directory, processed_directory):
    if not os.path.exists(processed_directory):
        os.makedirs(processed_directory)

    for filename in os.listdir(directory):
        if filename.endswith('.json'):
            file_path = os.path.join(directory, filename)
            fy_2021_data = filter_data_for_fy_2021(file_path)
            processed_file_path = os.path.join(processed_directory, filename)

            with open(processed_file_path, 'w') as outfile:
                json.dump(fy_2021_data, outfile)

            print(f"Processed data written to {processed_file_path}")

processed_directory = './processed_ciks'
directory = './CIKData'
process_json_files(directory, processed_directory)
