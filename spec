#!/bin/bash

if [ ! -f cache/$@ ]; then
  data=$(curl -s $(printf "https://oeis.org/search?fmt=json&q=id:A%06d" $@))
  echo "$data" > "cache/$@"
else
  data=$(cat "cache/$@")
fi

echo "instance OEISSpec $@ where"
echo -n "  spec = ["
echo "$data" | jq '.results|.[0]|.data' -rj
echo ']'
echo
