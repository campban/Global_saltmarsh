# -*- coding: utf-8 -*-
"""
Created on Tue Feb 11 09:12:34 2020

@author: campb
"""
shp = "C:/Work/global_salt_marsh/outputs/europe/country/country.shp"
import os
import fiona
import fnmatch
from subprocess import call
import osgeo

dest = "C:/Work/global_salt_marsh/outputs/europe/country/"

infile = (
   "C:/Work/global_salt_marsh/outputs/europe/country/country.shp")

with fiona.open(infile) as source:

    meta = source.meta

    for f in source:

        outfile = os.path.join(dest, "%s.shp" % f['properties']['ADMIN'])

        with fiona.open(outfile, 'w', **meta) as sink:

            sink.write(f)
         

#inFolder= "C:/Work/light/global_harmonized_light"
inFolder = "C:/Work/global_salt_marsh/USA\gpp/Tidal_Wetland_GPP_CONUS_1792/data"
os.chdir (inFolder)

def findRasters (path, filter):
    for root, dirs, files in os.walk(path, filter):
        for file in fnmatch.filter(files, filter):
            yield os.path.join (root, file)
            print(file)   
#infolder2 ="C:/Work/light/pixeltosystem"
infolder2 = "C:/Work/global_salt_marsh/USA/update_5_years/GPP"
def findShapes (path, filter):
    for root, dirs, files in os.walk(path, filter):
        for file in fnmatch.filter(files, filter):
            yield os.path.join (root, file)
shapes2=findShapes(infolder2,'.shp')
shapes2


for root, dirs, files in os.walk(infolder2,'.shp'):
    (pathshape, shapename)=os.path.split(files)
    shapename = shapename[:-4]
    print(shapename)
for raster in findRasters (inFolder, '*.tif'):
    print(raster)
import sys
import numpy as np
import gdal

for shps in findShapes (infolder2, '*.shp'):
    (pathshape, shapename)=os.path.split (shps)
    newpath = pathshape+ '/'+shapename[:-4]
    print(newpath)        
    os.makedirs(newpath)
    for raster in findRasters (inFolder, '*.tif'):
            (infilepath, infilename)= os.path.split (raster)
            print(infilename)
            outRaster= newpath+'/'+infilename[:-4]+'.tif'
            print(outRaster)
            warp= 'gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES --config CHECK_DISK_FREE_SPACE FALSE -dstnodata 0 -q -s_srs EPSG:32614 -t_srs EPSG:32618 -cutline %s -crop_to_cutline -of GTiff %s %s' % (shps, raster, outRaster)
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
warp
print('data')
print(ds)
np.isnan(myarray).sum()
    file_list = glob.glob(newpath+"\*.tif")
    files_string = " ".join(file_list)
    out2 = shapename[:-4]+'.tif'
    command = "C:/Users/adcampb1/Anaconda3/envs/qgis/Scripts/gdal_merge.py -o %s -of gtiff %s)" % (out2,files_string)
    call(command)
import gdal
outraster = "'C:/Work/global_salt_marsh/outputs/mexico/watersheds/7060006420/7060006420Mexico_ndvi_Anomaly_1991_2019_V02_dl-0000023296-0000046592.tif"
C:\Work\global_salt_marsh\USA\nwi_sm_ss_fo\split
inFolder3 ='C:/Work/global_salt_marsh/outputs/china/extent'
for shps in findShapes (infolder2, '*.shp'):
    (pathshape, shapename)=os.path.split (shps)
    newpath = pathshape+ '/'+shapename[:-4]
    print(newpath)        
    for raster in findRasters (inFolder3, '*.vrt'):
            (infilepath, infilename)= os.path.split (raster)
            print(infilename)
            outRaster= newpath+'/'+shapename[:-4]+infilename[:-4]+'.tif'
            print(outRaster)
            warp= 'gdalwarp  --config GDALWARP_IGNORE_BAD_CUTLINE YES -dstnodata 0 -q -cutline %s -crop_to_cutline -of GTiff %s %s' % (shps, raster, outRaster)
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
for shps in findShapes (infolder2, '*.shp'):
    (pathshape, shapename)=os.path.split (shps)
    newpath = pathshape+ '/'+shapename[:-4]
    for raster in findRasters (inFolder3, '*.tif'):
            (infilepath, infilename)= os.path.split (raster)
            print(infilename)
            outRaster= newpath+'/'+shapename[:-4]+infilename[:-4]+'.tif'
            print(outRaster)
            warp= 'gdalwarp  --config GDALWARP_IGNORE_BAD_CUTLINE YES -dstnodata 0 -q -cutline %s -crop_to_cutline -of GTiff %s %s' % (shps, raster, outRaster)
            call(warp)
            
    file_list = glob.glob(newpath+"\*.tif")
    files_string = " ".join(file_list)
    out2 = shapename[:-4]+'.tif'
    command = "gdal_merge -o %s -of gtiff %s)" % (out2,files_string)
    os.system(command)
raster
import numpy as np
from osgeo import gdal
ds = gdal.Open(raster)
myarray = np.array(ds.GetRasterBand(1).ReadAsArray())
np.isnan(myarray).all()
if not np.isfinite(prop).all()


from osgeo import gdal,ogr
import gdal_merge
os.path
import os
folder1 ="D:/Atlantic_coast/output"
import glob
file_list = glob.glob('C:/Work/global_salt_marsh/outputs/mexico/watersheds/7060005810')
gdal_merge
file_list = glob.glob(newpath+"\*.tif")
files_string = " ".join(file_list)
out2 = shapename[:-4]+'.tif'
command = "gdal_merge -o %s -of gtiff %s)" % (out2,files_string)
os.system(command)


def findFolders (path):
    for root, dirs in os.walk(path):
        for dirs in dirs:
            yield os.path.join (root)
findFolders(folder1)
for folds in os.listdir(folder1):
    location1=folder1+"/"+folds 
    os.chdir(location1)
    (pathshape, shapename)=os.path.split (folds)
    filename='clip_CCAP_1996_sm'
    inDs = gdal.Open('{}.img'.format(filename))
    outDs = gdal.Translate('{}.xyz'.format(shapename), inDs, format='XYZ', creationOptions=["ADD_HEADER_LINE=YES"])
    outDs = None
    try:
        os.remove('{}.csv'.format(shapename))
    except OSError:
        pass
    os.rename('{}.xyz'.format(shapename), '{}.csv'.format(shapename))
    os.system('ogr2ogr -f "ESRI Shapefile" -oo X_POSSIBLE_NAMES=X* -oo Y_POSSIBLE_NAMES=Y* -oo KEEP_GEOM_COLUMNS=NO {0}.shp {0}.csv'.format(shapename))

import sys
import numpy as np
import gdal

for shps in findShapes (infolder2, '*.shp'):
    (pathshape, shapename)=os.path.split (shps)
    newpath = pathshape+ '/'+shapename[:-4]
    print(newpath)        
    os.makedirs(newpath)
    for raster in findRasters (inFolder3, '*.vrt'):
            (infilepath, infilename)= os.path.split (raster)
            print(infilename)
            outRaster= newpath+'/'+infilename[:-4]+'.tif'
            print(outRaster)
            warp= 'gdalwarp  --config GDALWARP_IGNORE_BAD_CUTLINE YES --config CHECK_DISK_FREE_SPACE FALSE -dstnodata 0 -q -cutline %s -crop_to_cutline -of GTiff %s %s' % (shps, raster, outRaster)
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
polygonizer = "gdal_polygonize C:/Work/bioscape/estuary/wv/014179617010_01_003/014179617010_01/014179617010_01_P001_PSH/segs_1.tif C:/Work/bioscape/estuary/wv/segs_all.shp -b 1 -f %s segs_all DN" % ("ESRI Shapefile")
call(polygonizer)
     
inFolder= "C:/Work/biomass/ready"
os.chdir (inFolder)
for raster in findRasters (inFolder, '*.tif'):
            (infilepath, infilename)= os.path.split (raster)
            print(infilename)
            ds = gdal.Open(raster)
            if ds is None:
                pass
            else:
                myarray = np.array(ds.GetRasterBand(1).ReadAsArray())
                if np.nansum(myarray) == 0:
                    del ds
                    del myarray
                    os.remove(raster)
                    print('NoDATA') 
                else: 
                    print('DATA')
                    