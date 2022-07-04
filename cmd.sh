#!/bin/bash

curl \
  --verbose \
  -H "Content-Type: application/json" \
  -X POST \
  -d @data.json \
  "http://localhost:8000/people"
