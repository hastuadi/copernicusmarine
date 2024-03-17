# -*- coding: utf-8 -*-
"""
Created on Wed Dec  6 18:57:44 2023

@author: emi yati
"""
from datetime import datetime
import copernicusmarine
import sys
def generate_copernicus_subset():
    start_date = sys.argv[1]
    end_date = sys.argv[2]
    output_filename = sys.argv[3]
    start_datetime = datetime.strptime(start_date, "%Y-%m-%d")
    end_datetime = datetime.strptime(end_date, "%Y-%m-%d")

    copernicusmarine.subset(
        dataset_id="cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D",
        variables=["CHL", "CHL_uncertainty"],
        minimum_longitude=float(sys.argv[4]),
        maximum_longitude=float(sys.argv[5]),
        minimum_latitude=float(sys.argv[6]),
        maximum_latitude=float(sys.argv[7]),
	username=sys.argv[8],
	password=sys.argv[9],
#	netcdf_compression_enabled=True,
#	netcdf_compression_level=int(sys.argv[10]),	
        start_datetime=start_datetime,
	force_download=True,
        end_datetime=end_datetime,
        output_filename=output_filename,
        # output_directory=output_directory
    )

generate_copernicus_subset()