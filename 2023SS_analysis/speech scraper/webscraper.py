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

        # url = f"https://www.president.gov.ua/en/news/speeches?date-from=06-07-2022&date-to=06-07-2023&page={i}"

        req = requests.Request(url, headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36',
  'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
  'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
  'Accept-Encoding': 'none',
  'Accept-Language': 'en-US,en;q=0.8',
  'Connection': 'keep-alive',
  'refere': 'https://example.com',
  'cookie': """your cookie value ( you can get that from your web page) """
})

        resp = requests.urlopen(req, timeout=22).read()

        fin_content = html.fromstring(resp).xpath("//a[starts-with(@href, 'https://www.president.gov.ua/en/news/')]/@href")

        unique_links = set(fin_content)
        
        for line in unique_links:
            line = str(line)
            if len(line)>100:
                linkofu.append(line)
    print()
    linkofu = set(linkofu)
    for line in linkofu:
        with open(f'links2.txt', 'a') as file:
            file.write(line)
            file.write('\n')
        print(line)