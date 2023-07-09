import itertools
import json
import logging
import re
import unicodedata
import urllib.request as requests
from decimal import Decimal
from lxml import html
from datetime import datetime
from datetime import datetime

file_num = 1


if __name__ == "__main__":

    with open('links2.txt', 'r') as file:
        for links in file:
            date_list = []
            links = links.strip()
            print(links)

            url = links
                
            req = requests.Request(url, headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36',
  'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
  'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
  'Accept-Encoding': 'none',
  'Accept-Language': 'en-US,en;q=0.8',
  'Connection': 'keep-alive',
  'refere': 'https://example.com',
  'cookie': """your cookie value ( you can get that from your web page) """
})
            resp = requests.urlopen(req, timeout=5).read()

            date_resp = html.fromstring(resp).xpath("//div/p[@class='date']/text()")
            title_resp = html.fromstring(resp).xpath("//div/h1/text()")
            fin_resp1 = html.fromstring(resp).xpath("//div[@itemprop]/p/strong/text()")
            fin_resp2 = html.fromstring(resp).xpath("//div[@itemprop]/p/text()")

            for line in date_resp:
                date_list.append(line)

            print(date_list[4])

            date_time_str = str(date_list[4].strip())
            print(date_time_str)

            date_time_obj = datetime.strptime(date_time_str, '%d %B %Y - %H:%M')
            formatted_date = date_time_obj.strftime('%Y-%m-%d')
            print ("The date is", formatted_date)
                        
            with open(f'{formatted_date}_content_D_{file_num}.txt', 'a') as file:
                file.write(formatted_date)
                file.write('\n')

            for line in title_resp:
                print(line)
                with open(f'{formatted_date}_content_D_{file_num}.txt', 'a') as file:
                    file.write(line)
                    file.write('\n')
            for line in fin_resp1:
                print(line)
                with open(f'{formatted_date}_content_D_{file_num}.txt', 'a') as file:
                    file.write(line)
                    file.write('\n')
            for line in fin_resp2:
                print(line)
                with open(f'{formatted_date}_content_D_{file_num}.txt', 'a') as file:
                    file.write(line)
                    file.write('\n')

            file_num += 1
