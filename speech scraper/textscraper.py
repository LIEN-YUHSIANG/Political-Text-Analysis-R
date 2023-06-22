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

    with open('links.txt', 'r') as file:
        for links in file:
            date_list = []
            links = links.strip()
            print(links)

            url = links
                
            req = requests.Request(url, headers={'User-Agent': 'XYZ/3.0'})

            resp = requests.urlopen(req, timeout=20).read()

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
                        
            with open(f'{formatted_date}content.txt - {file_num}', 'a') as file:
                file.write(date_list[4])
                file.write('\n')

            for line in title_resp:
                print(line)
                with open(f'{formatted_date}content.txt - {file_num}', 'a') as file:
                    file.write(line)
                    file.write('\n')
            for line in fin_resp1:
                print(line)
                with open(f'{formatted_date}content.txt - {file_num}', 'a') as file:
                    file.write(line)
                    file.write('\n')
            for line in fin_resp2:
                print(line)
                with open(f'{formatted_date}content.txt - {file_num}', 'a') as file:
                    file.write(line)
                    file.write('\n')

            file_num += 1
