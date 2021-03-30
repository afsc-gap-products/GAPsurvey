BVDR convert set up:
Put current BVDRReader.exe
            bvdr_path.cfg
            convert_bvdr.exe
into C:\nm

The current (automated) version of convert_bvdr.exe is dated 8/4/2011 9:51 AM

Edit bvdr_path.cfg:
 
Leave first two lines intact (instructions)

change first file line to point to your bvdr files (this will usually be C:\Program Files\Marport Server\Logs while at sea)

change second file line to point to your NM data directory (this will be the same directory as scanpath.cfg)

Make a shortcut for convert_bvdr.exe on the desktop

---------------------------
Run convert_bvdr.  It will prompt you for a haul number.  This haul number is used to select the tow start and end times and select the appropriate 1-3 .BVDR files to convert.  If a .BVDR file is available, the program shells out to BVDRReader which will open in a DOS (black) window.  It will tell you whether it converted the files or say that they are up to date (if it tells you the file is up to date, it means the file already exists, if you want to replace it, you need to delete the old file before running the program). You will need to press Enter at the prompt to end BVDRReader. A BVDRReader window will open for each file that needs to be converted automatically.  This will normally be two files; if there are fewer or more, double check your .SGT file to confirm tow start/end/duration.

 If the .BVDR file is NOT available, it will give you a message saying it could not find the file. Just press "OK" and it will convert the available .csv file.

The .csv files from BVDRReader at automatically converted into two files:

All output files have the following prefix, cruise_vessel_haul:
CCCCCC_VVV_HHHH.xxx

and the following extentions:
.mpt  height and spread ***CODED 12 & 23 for unit numbers to run through Scanplot!!!****
the rest match the $nmea names in the input file:

.teh  height 
.ted  depth
.tet  headrope temperature
.tmp  wing temperature
.pit  pitch
.rol  roll
.gll  GLL gps data

.mpt is a NM program readable .txt file (can be renamed or appended to current .SGP files; same data structure, but NO header)

The main bug we have encountered is that the BVDRReader adds a corrupted data line after each height record ($01TEH).  My conversion program can handle 99% of these lines, but sometimes a EOF (end of file) binary value is in the corrupted line and is interpreted as the end of the file.  You will need to check each .txt or .mpt file to see that the last record corresponds to the last $01TEH or $01DST record in the .csv file.

Also remember that the conversion program from .csv to .txt/.mpt ALWAYS appends, so if you have a problem with the program, you will want to delete or rename the .txt/.mpt files before re-converting.

The .BVDR and .CSV files are named with YYYYMMDD-HHZ; year, month, day, hour (zulu).  If you want to compare these files to .SGT and other NM data files, you will have to pick the hour that corresponds to +8 hours after the time in the .SGT files, rounded up.  If your tow started at 06:50 and continued into 07:45 (.SGT), your .BVDR and .CSV files will have a 14Z and 15Z extension.

Robin (-4139)