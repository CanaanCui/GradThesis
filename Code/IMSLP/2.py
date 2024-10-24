# -*- coding: utf-8 -*-

import requests
from bs4 import BeautifulSoup
import time
import csv
import pandas as pd

# 定义请求的 cookies 和 headers
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
    # 使用 BeautifulSoup 解析页面
    soup = BeautifulSoup(content, 'html.parser')

    # 提取 h1 标签 (作曲家名称)
    h1_tag = soup.find('h1', class_='firstHeading')
    composer_name = h1_tag.text.strip() if h1_tag else "N/A"

    # 提取 div 标签 (生卒年份)
    div_tag = soup.find('div', class_='cp_firsth')
    life_span = "N/A"
    if div_tag:
        life_span = div_tag.text.strip() if div_tag else "N/A"

    # 提取作品集数量 <ul> 标签信息
    ul_tag = soup.find('ul', class_='jsonly')
    compositions_count = "N/A"
    if ul_tag:
        li_tag = ul_tag.find('li')
        compositions_count = li_tag.text.strip() if li_tag else "N/A"

    #div = soup.find("div", class_='ui-corner-bottom')
    div = soup.find("div", id='mw-pages')
    #print(div.text)
    # 提取所有 <li> 标签并解析作品信息
    li_tags = div.find_all('li')
    work_titles = []  # 保存作品标题的列表
    work_hrefs = []   # 保存作品链接的列表
    for li in li_tags:
        work_link = li.find('a')  # 找到 <a> 标签
        if work_link:
            work_title = work_link.text.strip()  # 提取作品标题
            work_href = work_link['href']  # 提取链接
            work_titles.append(work_title)  # 将标题添加到列表
            work_hrefs.append(work_href)    # 将链接添加到列表

    return composer_name, life_span, compositions_count, work_titles, work_hrefs

def check_for_captcha(content):
    # 使用 BeautifulSoup 解析页面内容，检查是否存在验证码提示
    soup = BeautifulSoup(content, 'html.parser')
    captcha = soup.find(text="This page is protected from bots by a captcha.")
    if captcha:
        print("检测到 CAPTCHA，请手动解决...")
        return True
    return False

def fetch_url(url, retries=3, delay=35):
    """请求网页，支持网络异常重试，默认重试3次，失败后等待5秒"""
    for attempt in range(1, retries + 1):
        try:
            response = requests.get(url, cookies=cookies, headers=headers, timeout=10)
            if response.status_code == 200:
                return response
            else:
                print(f"请求失败，状态码: {response.status_code}，重试 {attempt}/{retries}")
        except (requests.exceptions.RequestException, requests.exceptions.Timeout) as e:
            print(f"请求时遇到异常: {e}，重试 {attempt}/{retries}")
        time.sleep(delay)  # 等待后重试
    return None

# 读取 composers_with_era.csv 并提取前 100 个 URL 和 时代
df = pd.read_excel('Composer_URLs_and_Names.xlsx')
urls = df['Composer URLs']  # 假设 'url' 列包含网页链接
composer_names = df['Composer Names'] # 假设 '时代' 列表示作曲家的时代
periods = df['Period']

# 创建一个新的 CSV 文件用于存储获取到的信息
with open('composers_info.csv', mode='a', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)
    writer.writerow(['url',  'Composer Name','Period', 'Life Span', 'Compositions Count', 'Work Title', 'Work Link'])  # 写入表头

    # 遍历所有 URL 和 时代
    for i, (url, composer_name ,period) in enumerate(zip(urls, composer_names, periods), start=1):
        print(i)
        #url = 'https://imslp.org/wiki/Category:Chopin,_Fr%C3%A9d%C3%A9ric'
        print(f"正在请求第 {i} 个 URL: {url}")

        # if i < 14797:
        #     continue

        # 使用 fetch_url 函数处理带有重试逻辑的请求
        response = fetch_url(url)

        if response:
            # 提取信息
            composer_name, life_span, compositions_count, work_titles, work_hrefs = extract_info(response.text)

            # 每个作品单独一行保存
            if work_titles and work_hrefs:
                for work_title, work_href in zip(work_titles, work_hrefs):
                    writer.writerow([url, composer_name, period, life_span, compositions_count, work_title, work_href])
            else:
                # 如果没有作品，则保存空行
                writer.writerow([url, composer_name, period, life_span, compositions_count, "N/A", "N/A"])
        else:
            print(f"多次尝试后仍无法访问 {url}，跳过该链接")

        # 检查是否需要手动解决 CAPTCHA
        if response and check_for_captcha(response.text):
            input("请解决 CAPTCHA 验证后按回车键继续...")

        # 暂停 3 秒以避免过度请求
        #time.sleep(3)

print("所有请求完成，数据已保存到 composers_info.csv。")
