# -*- coding: utf-8 -*-
"""
Created on Tue Feb 11 09:12:34 2020

@author: Anthony Campbell
"""
#import necessary packages
import os
import fiona
import fnmatch
from subprocess import call
import osgeo
import sys
import numpy as np
import gdal
import glob

#output location
dest = ""

#Shapefile to be split based on data field, countries or watersheds
#seperate shapefile into individual shapefiles to clip rasters with
infile = (
   "")

with fiona.open(infile) as source:

    meta = source.meta

    for f in source:

        outfile = os.path.join(dest, "%s.shp" % f['properties']['ADMIN'])

        with fiona.open(outfile, 'w', **meta) as sink:

            sink.write(f)
         

#folder with shapefiles, will have outputs in seperate folders too
os.chdir (dest)   
#rasters location
infolder2 = ""

for shps in glob.glob(os.path.join(dest, '*.shp')):
    (pathshape, shapename)=os.path.split (shps)
    newpath = pathshape+ '/'+shapename[:-4]
    print(newpath)        
    os.makedirs(newpath)
    for raster in glob.glob(os.path.join(infolder2, '*.tif')):
            (infilepath, infilename)= os.path.split (raster)
            print(infilename)
            outRaster= newpath+'/'+infilename[:-4]+'.tif'
            print(outRaster)
            warp= 'gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES --config CHECK_DISK_FREE_SPACE FALSE -dstnodata 0 -q -s_srs EPSG:4326 -t_srs EPSG:4326 -cutline %s -crop_to_cutline -of GTiff %s %s' % (shps, raster, outRaster)
            call(warp)
            ds = gdal.Open(outRaster)
            if ds is None:
                pass
            else:
                myarray = np.array(ds.GetRasterBand(1).ReadAsArray())
                if np.nansum(myarray) == 0:
                    del ds
                    del myarray
                    os.remove(outRaster)
                    print('NoDATA') 
                else: 
                    del ds
                    del myarray
