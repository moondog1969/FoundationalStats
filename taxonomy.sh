#!/bin/bash
FILE_NAME="$1"

echo "Phyla = $(grep -Po '(?<=p__)[^; ]+' $FILE_NAME | sort -u | wc -l) "
echo "Classes = $(grep -Po '(?<=c__)[^; ]+' $FILE_NAME | sort -u | wc -l) "
echo "Orders = $(grep -Po '(?<=o__)[^; ]+' $FILE_NAME | sort -u | wc -l) "
echo "Families = $(grep -Po '(?<=f__)[^; ]+' $FILE_NAME | sort -u | wc -l) "
