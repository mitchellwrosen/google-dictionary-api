#!/bin/bash

url="http://www.google.com/dictionary/json?callback=a&sl=en&tl=en&q="
json=$(curl $url$1)
json=${json:2:-10}
echo $json > ${1}.json
cat ${1}.json | perl -pe 's/\\x([0-9a-f]{2})/chr hex $1/gie' > ${1}2.json
cat ${1}2.json | python -mjson.tool > ${1}.json
rm ${1}2.json
