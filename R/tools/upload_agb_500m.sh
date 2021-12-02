#!/bin/bash

# to upload each year into a separate image in an imagecollection:
# for y in $(seq 2003 2016); do
# 	echo $y
# 	sed "s/YEAR/$y/g" /Users/sgorelik/projects/gee_modis_upload/manifest_template.json > /Users/sgorelik/projects/gee_modis_upload/manifest_year.json
# 	earthengine upload image --manifest /Users/sgorelik/projects/gee_modis_upload/manifest_year.json
# done

# to upload all years into a single image as bands
earthengine upload image --manifest /Users/sgorelik/projects/gee_modis_upload/manifest_stack.json
