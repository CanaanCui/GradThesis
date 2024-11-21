# -*- coding: utf-8 -*-

import requests
from bs4 import BeautifulSoup
import time
import csv
import pandas as pd

# Define the cookies and headers for the request
cookies = {
    'imslp_wikiLanguageSelectorLanguage': 'zh',
    '_clck': '1v8u2zl%7C2%7Cfp7%7C0%7C1719',
    '__qca': 'P0-1455575356-1726383962895',
    '_gid': 'GA1.2.426092878.1726384344',
    'imslp_wiki_session': '96d5216deb9db0107c7e4997962f64d6',
    '_ga_8370FT5CWW': 'GS1.2.1726388078.2.0.1726388078.0.0.0',
    '_ga': 'GA1.1.50446523.1726383963',
    '_clsk': '1n8kqwy%7C1726388159248%7C200%7Cs.clarity.ms%2Fcollect',
    '_ga_4QW4VCTZ4E': 'GS1.1.1726383962.1.1.1726388161.0.0.0',
}

headers = {
    'accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
    'accept-language': 'zh-CN,zh;q=0.9',
    'priority': 'u=0, i',
    'referer': 'https://imslp.org/index.php?title=Category:People_from_the_Early_20th_century_era',
    'sec-ch-ua': '"Chromium";v="128", "Not;A=Brand";v="24", "Google Chrome";v="128"',
    'sec-ch-ua-mobile': '?0',
    'sec-ch-ua-platform': '"Windows"',
    'sec-fetch-dest': 'document',
    'sec-fetch-mode': 'navigate',
    'sec-fetch-site': 'same-origin',
    'sec-fetch-user': '?1',
    'upgrade-insecure-requests': '1',
    'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36',
}

def extract_info(content):
    #print(content)
    # Use BeautifulSoup to parse the page
    soup = BeautifulSoup(content, 'html.parser')

    # Extract the h1 tag (composer name)
    h1_tag = soup.find('h1', class_='firstHeading')
    composer_name = h1_tag.text.strip() if h1_tag else "N/A"

    # Extract the div tag (birth and death years)
    div_tag = soup.find('div', class_='cp_firsth')
    life_span = "N/A"
    if div_tag:
        life_span = div_tag.text.strip() if div_tag else "N/A"

    # Extract the number of collections from the <ul> tag
    ul_tag = soup.find('ul', class_='jsonly')
    compositions_count = "N/A"
    if ul_tag:
        li_tag = ul_tag.find('li')
        compositions_count = li_tag.text.strip() if li_tag else "N/A"

    #div = soup.find("div", class_='ui-corner-bottom')
    div = soup.find("div", id='mw-pages')
    #print(div.text)
    # Extract all <li> tags and parse the composition information
    li_tags = div.find_all('li')
    work_titles = []  # List to store work titles
    work_hrefs = []   # List to store work links
    for li in li_tags:
        work_link = li.find('a')  # Find the <a> tag
        if work_link:
            work_title = work_link.text.strip()  # Extract the work title
            work_href = work_link['href']  # Extract the link
            work_titles.append(work_title)  # Add the title to the list
            work_hrefs.append(work_href)    # Add the link to the list


    return composer_name, life_span, compositions_count, work_titles, work_hrefs

def check_for_captcha(content):
    # Use BeautifulSoup to parse the page content and check for CAPTCHA prompts
    soup = BeautifulSoup(content, 'html.parser')
    captcha = soup.find(text="This page is protected from bots by a captcha.")
    if captcha:
        print("CAPTCHA detected, please solve it manually...")
        return True
    return False

def fetch_url(url, retries=3, delay=35):
    """Request a webpage, with support for retrying on network errors. Defaults to 3 retries, with a delay of 5 seconds on failure."""
    for attempt in range(1, retries + 1):
        try:
            response = requests.get(url, cookies=cookies, headers=headers, timeout=10)
            if response.status_code == 200:
                return response
            else:
                print(f"Request failed, status code: {response.status_code}, retrying {attempt}/{retries}")
        except (requests.exceptions.RequestException, requests.exceptions.Timeout) as e:
            print(f"Exception encountered during request: {e}, retrying {attempt}/{retries}")
        time.sleep(delay)  # Wait before retrying
    return None

# Read composers_with_era.csv and extract the first 100 URLs, composer names, and periods
df = pd.read_excel('Composer_URLs_and_Names.xlsx')
urls = df['Composer URLs']  # Assumes the 'Composer URLs' column contains webpage links
composer_names = df['Composer Names']  # Assumes the 'Composer Names' column contains composer names
periods = df['Period']  # Assumes the 'Period' column contains the era of the composers

# Create a new CSV file to store the extracted information
with open('composers_info.csv', mode='a', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)
    writer.writerow(['url', 'Composer Name', 'Period', 'Life Span', 'Compositions Count', 'Work Title', 'Work Link'])  # Write the header

    # Iterate over all URLs, composer names, and periods
    for i, (url, composer_name, period) in enumerate(zip(urls, composer_names, periods), start=1):
        print(i)
        print(f"Requesting URL {i}: {url}")

        # Use the fetch_url function with retry logic to handle requests
        response = fetch_url(url)

        if response:
            # Extract information
            composer_name, life_span, compositions_count, work_titles, work_hrefs = extract_info(response.text)

            # Save each work as a separate row
            if work_titles and work_hrefs:
                for work_title, work_href in zip(work_titles, work_hrefs):
                    writer.writerow([url, composer_name, period, life_span, compositions_count, work_title, work_href])
            else:
                # If no works are found, save a row with N/A
                writer.writerow([url, composer_name, period, life_span, compositions_count, "N/A", "N/A"])
        else:
            print(f"Unable to access {url} after multiple attempts, skipping this link.")

        # Check if CAPTCHA needs to be resolved manually
        if response and check_for_captcha(response.text):
            input("Please solve the CAPTCHA and press Enter to continue...")

        # Pause to avoid overwhelming the server
        # time.sleep(3)

print("All requests completed. Data has been saved to composers_info.csv.")

