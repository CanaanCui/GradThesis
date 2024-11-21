import re
import requests
from bs4 import BeautifulSoup
import csv
import time

# Define request and parsing functions with a retry mechanism

def fetch_chopin_etudes_data(url, retries=3):
    cookies = {
        'imslp_wikiLanguageSelectorLanguage': 'zh',
        '__qca': 'P0-1774061278-1726369217946',
        '_clck': 'zs361e%7C2%7Cfp9%7C0%7C1719',
        '_gid': 'GA1.2.2103525010.1726538461',
        '_ga': 'GA1.2.998890174.1726369217',
        '_ga_8370FT5CWW': 'GS1.2.1726538462.4.1.1726539429.0.0.0',
        '_ga_4QW4VCTZ4E': 'GS1.1.1726538318.6.1.1726539686.0.0.0',
        '_clsk': '1hyh7ea%7C1726540495515%7C10%7C1%7Ce.clarity.ms%2Fcollect',
    }

    headers = {
        'accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
        'accept-language': 'zh-CN,zh;q=0.9,en;q=0.8',
        'cache-control': 'no-cache',
        'pragma': 'no-cache',
        'priority': 'u=0, i',
        'sec-ch-ua': '"Chromium";v="128", "Not;A=Brand";v="24", "Google Chrome";v="128"',
        'sec-ch-ua-mobile': '?0',
        'sec-ch-ua-platform': '"Windows"',
        'sec-fetch-dest': 'document',
        'sec-fetch-mode': 'navigate',
        'sec-fetch-site': 'none',
        'sec-fetch-user': '?1',
        'upgrade-insecure-requests': '1',
        'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36',
    }

    # Attempt to request the page with retry support
    for attempt in range(retries):
        try:
            response = requests.get(url, cookies=cookies, headers=headers)
            if response.status_code == 200:
                break
        except Exception as e:
            print(f"Request failed (attempt {attempt + 1}/{retries}), error: {e}")
            time.sleep(3)  # Wait 3 seconds before retrying
    else:
        print(f"Request failed, status code: {response.status_code}")
        return None

    # Parse the HTML content
    soup = BeautifulSoup(response.content, "html.parser")


    # Initialize result dictionary
    result = {
        "song_title": "",
        "movement_section": '',
        "work_year": "",
        "style": "",
        "catalog_number": "",
        "instrument": "",
        "movement_section_detail": "",
        "top_3_downloads": [],
        "total_top_3": 0,
        "average_top_3": 0,
        "median_top_3": 0,
        'average_duration': ''
    }

    # Parse song title
    try:
        song_title = soup.find("h1", {"id": "firstHeading"}).get_text(strip=True)
        result["song_title"] = song_title
    except Exception as e:
        print("Error parsing song title:", e)

    # Parse movement/section (fuzzy match)
    try:
        movement = soup.find("th", string=re.compile("Movement/Section")).find_next("td")
        result["movement_section"] = movement.text.strip()
    except Exception as e:
        print("Error parsing movement/section:", e)


    # Parse work year/date (fuzzy match)
    try:
        work_year = soup.find("th", string=re.compile("Work Year/Date")).find_next("td").get_text(strip=True)
        result["work_year"] = work_year
    except Exception as e:
        print("Error parsing work year/date:", e)

    # Parse musical style (fuzzy match)
    try:
        style = soup.find("th", string=re.compile("Musical Style")).find_next("td").get_text(strip=True)
        result["style"] = style
    except Exception as e:
        print("Error parsing musical style:", e)

    # Parse catalog number (fuzzy match)
    try:
        catalog_number = soup.find("th", string=re.compile("Catalog Number")).find_next("td").get_text(strip=True)
        result["catalog_number"] = catalog_number
    except Exception as e:
        print("Error parsing catalog number:", e)

    # Parse instrumentation (fuzzy match)
    try:
        instrument = soup.find("th", string=re.compile("Instrumentation")).find_next("td").get_text(strip=True)
        result["instrument"] = instrument
    except Exception as e:
        print("Error parsing instrumentation:", e)


    # Parse movement_section detail
    try:
        movement_detail = soup.find("th", string=re.compile("Movement/Section Detail")).find_next("td")
        if movement_detail:
            result["movement_section_detail"] = movement_detail.text.strip()
    except Exception as e:
        print("Error parsing movement/section detail:", e)

    # Parse average duration
    try:
        average_duration = soup.find("th", string=re.compile("Average Duration")).find_next("td")
        if average_duration:
            result["average_duration"] = average_duration.text.strip()
    except Exception as e:
        print("Error parsing average duration:", e)

    # Parse download links and counts for "complete performances"
    download_counts = []
    try:
        complete_performances = soup.find_all("div", class_="we_file_download plainlinks")
        for performance in complete_performances:
            if "Complete Performance" in performance.get_text() or "Complete Score" in performance.get_text():
                a = performance.find("a", title=re.compile("Special:GetFCtrStats"))
                download_count = a.get_text(strip=True)
                # Convert download count to integer and add to the list
                download_counts.append(int(download_count))
    except Exception as e:
        print("Error parsing complete performance download links:", e)


    # If the download counts list is not empty, perform statistical analysis
    if download_counts:
        top_3_downloads = sorted(download_counts, reverse=True)[:3]
        result["top_3_downloads"] = top_3_downloads
        result["total_top_3"] = sum(top_3_downloads)
        result["average_top_3"] = sum(top_3_downloads) / len(top_3_downloads)
        result["median_top_3"] = top_3_downloads[len(top_3_downloads) // 2]

    return result


# Read the composers_info.csv file
with open('composers_info.csv', mode='r', encoding='utf-8') as file:
    reader = csv.DictReader(file)
    rows = list(reader)

# Open or create a new CSV file to save the merged data
with open('composers_info_with_details.csv', mode='a', newline='', encoding='utf-8') as file:
    fieldnames = list(rows[0].keys()) + ['song_title', 'movement_section', 'work_year', 'style', 'catalog_number', 'instrument', 'movement_section_detail', 'top_3_downloads', 'total_top_3', 'average_top_3', 'median_top_3', 'average_duration']
    writer = csv.DictWriter(file, fieldnames=fieldnames)
    writer.writeheader()


    # Iterate over each link, fetch data, and write it to the CSV
    for index, row in enumerate(rows):
        # Uncomment the following line to skip already processed rows
        # if index < 15749:
        #     continue
        print(index)
        link = row['Work Link']
        if '/' not in link or 'http' in link:
            continue
        url = "https://imslp.org" + row['Work Link']

        # Example URL for testing
        # url = 'https://imslp.org/wiki/3_Ecossaises%2C_Op.72_No.3_(Chopin%2C_Fr%C3%A9d%C3%A9ric)'
        print(f"Processing {url} ...")
        new_data = fetch_chopin_etudes_data(url)

        if new_data:
            row.update(new_data)  # Merge new data into the original row
            writer.writerow(row)  # Write the updated row to the new file
            file.flush()

        # Uncomment the following line to pause for 3 seconds after each request
        # time.sleep(3)


print("Data has been successfully saved to composers_info_with_details.csv")

