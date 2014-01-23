#!/bin/bash

url="http://www.google.com/dictionary/json?callback=a&sl=en&tl=en&q="
json=$(curl $url$1)
echo ${json:2:-10} |
   perl -pe 's/\\x([0-9a-f]{2})/chr hex $1/gie' |
   python -mjson.tool > ${1}.json
