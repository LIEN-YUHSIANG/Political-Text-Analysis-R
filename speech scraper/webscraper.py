import itertools
import json
import logging
import re
import unicodedata
import urllib.request as requests
from decimal import Decimal
from lxml import html

linkofu = []


if __name__ == "__main__":

    for i in range(1, 53):

        url = f"https://www.president.gov.ua/en/news/speeches?date-from=17-06-2022&date-to=17-06-2023&page={i}"

        req = requests.Request(url, headers={'User-Agent': 'XYZ/3.0'})

        resp = requests.urlopen(req, timeout=20).read()

        fin_content = html.fromstring(resp).xpath("//a[starts-with(@href, 'https://www.president.gov.ua/en/news/')]/@href")

        unique_links = set(fin_content)
        
        for line in unique_links:
            line = str(line)
            if len(line)>100:
                linkofu.append(line)
    print()
    linkofu = set(linkofu)
    for line in linkofu:
        with open(f'links.txt', 'a') as file:
            file.write(line)
            file.write('\n')
        print(line)