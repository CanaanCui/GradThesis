import re
import requests
from bs4 import BeautifulSoup
import csv
import time

# 定义请求和解析函数，增加重试机制
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

    # 尝试请求页面，支持重试
    for attempt in range(retries):
        try:
            response = requests.get(url, cookies=cookies, headers=headers)
            if response.status_code == 200:
                break
        except Exception as e:
            print(f"请求失败（尝试 {attempt + 1}/{retries}），错误: {e}")
            time.sleep(3)  # 等待3秒再重试
    else:
        print(f"请求失败，状态码: {response.status_code}")
        return None

    # 解析HTML内容
    soup = BeautifulSoup(response.content, "html.parser")

    # 初始化结果字典
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
        'average_duration':''
    }

    # 解析 乐曲名字
    try:
        song_title = soup.find("h1", {"id": "firstHeading"}).get_text(strip=True)
        result["song_title"] = song_title
    except Exception as e:
        print("解析乐曲名字时出错:", e)

    # 解析 乐章/段落（模糊匹配）
    try:
        movement = soup.find("th", string=re.compile("乐章/部分")).find_next("td")
        #result["movement_section"] = [dd.get_text(strip=True) for dd in movements.find_all("dd")]

        result["movement_section"] = movement.text.strip()

    except Exception as e:
        print("解析乐章/段落时出错:", e)

    # 解析 作品年份/日期（模糊匹配）
    try:
        work_year = soup.find("th", string=re.compile("作品年份/日期")).find_next("td").get_text(strip=True)
        result["work_year"] = work_year
    except Exception as e:
        print("解析作品年份/日期时出错:", e)

    # 解析 乐曲风格（模糊匹配）
    try:
        style = soup.find("th", string=re.compile("乐曲风格")).find_next("td").get_text(strip=True)
        result["style"] = style
    except Exception as e:
        print("解析乐曲风格时出错:", e)

    # 解析 作品/目录编号（模糊匹配）
    try:
        catalog_number = soup.find("th", string=re.compile("作品/ 目录编号")).find_next("td").get_text(strip=True)
        result["catalog_number"] = catalog_number
    except Exception as e:
        print("解析作品/目录编号时出错:", e)

    # 解析 乐器配置（模糊匹配）
    try:
        instrument = soup.find("th", string=re.compile("配置")).find_next("td").get_text(strip=True)
        result["instrument"] = instrument
    except Exception as e:
        print("解析乐器配置时出错:", e)

    # 解析 movement_section
    # try:
    #     audio_tab = soup.find("li", id="tabAudio1_tab")
    #     if audio_tab:
    #         recording_count = audio_tab.find("span", id="tabAudio1_ct").get_text(strip=True)
    #         result["recording_count"] = recording_count
    # except Exception as e:
    #     print("解析录音数时出错:", e)

    #解析  movement_section detail
    try:
        movement_detail = soup.find("th", string=re.compile("乐章/ 段落")).find_next("td")
        if movement_detail:
            result["movement_section_detail"] = movement_detail.text.strip()
    except Exception as e:
        print("解析录音数时出错:", e)


    try:
        average_duration = soup.find("th", string=re.compile("平均时长")).find_next("td")
        if average_duration:
            result["average_duration"] = average_duration.text.strip()
    except Exception as e:
        print("解析录音数时出错:", e)

    # 解析带有“完整演奏”字样的下载链接和下载次数
    download_counts = []
    try:
        complete_performances = soup.find_all("div", class_="we_file_download plainlinks")
        for performance in complete_performances:
            if "完整演奏" in performance.get_text() or '完整乐谱' in performance.get_text() :
                a = performance.find("a", title=re.compile("Special:GetFCtrStats"))
                download_count = a.get_text(strip=True)
                # 将下载次数转换为整数并添加到列表中
                download_counts.append(int(download_count))
    except Exception as e:
        print("解析完整演奏链接时出错:", e)

    # 如果下载次数列表不为空，进行统计分析
    if download_counts:
        top_3_downloads = sorted(download_counts, reverse=True)[:3]
        result["top_3_downloads"] = top_3_downloads
        result["total_top_3"] = sum(top_3_downloads)
        result["average_top_3"] = sum(top_3_downloads) / len(top_3_downloads)
        result["median_top_3"] = top_3_downloads[len(top_3_downloads) // 2]

    return result


# 读取 composers_info.csv 文件
with open('composers_info.csv', mode='r', encoding='utf-8') as file:
    reader = csv.DictReader(file)
    rows = list(reader)

# 打开或创建新的 CSV 文件，保存合并的数据
with open('composers_info_with_details.csv', mode='a', newline='', encoding='utf-8') as file:
    fieldnames = list(rows[0].keys()) + ['song_title', 'movement_section', 'work_year', 'style', 'catalog_number', 'instrument', 'movement_section_detail', 'top_3_downloads', 'total_top_3', 'average_top_3', 'median_top_3','average_duration']
    writer = csv.DictWriter(file, fieldnames=fieldnames)
    writer.writeheader()

    # 遍历每个链接，获取数据并写入 CSV
    for index, row in enumerate(rows):
        # if index<15749:
        #     continue
        print(index)
        link = row['Work Link']
        if '/' not in link or  'http' in link:
            continue
        url = "https://imslp.org" + row['Work Link']

        #url = 'https://imslp.org/wiki/3_Ecossaises%2C_Op.72_No.3_(Chopin%2C_Fr%C3%A9d%C3%A9ric)'
        print(f"正在处理 {url} ...")
        new_data = fetch_chopin_etudes_data(url)

        if new_data:
            row.update(new_data)  # 将新数据合并到原始行数据中
            writer.writerow(row)  # 将合并后的数据写入新文件
            file.flush()

        #time.sleep(3)  # 每次请求后休息3秒钟

print("数据已成功保存到 composers_info_with_details.csv")
