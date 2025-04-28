#!/bin/bash
path=`dirname $0`
sleep 1
cd $path
echo "This bash script will create table from ?.stats files"
echo "Written by Jamaan Alghamdi & Dr. Vanessa Sluming"
echo "University of Liverpool"
echo "jamaan.alghamdi@gmail.com"
echo "http://www.easyneuroimaging.com"
echo "20/12/2010

"


#export FREESURFER_HOME=/usr/local/freesurfer
#sleep 1
#source $FREESURFER_HOME/SetUpFreeSurfer.sh
#sleep 1

export SUBJECTS_DIR=$PWD
list="`ls -d */`"
asegstats2table --subjects $list --meas volume --skip --statsfile wmparc.stats --all-segs --tablefile wmparc_stats.txt
asegstats2table --subjects $list --meas volume --skip --tablefile aseg_stats.txt
aparcstats2table --subjects $list --hemi lh --meas volume --skip --tablefile aparc_volume_lh.txt
aparcstats2table --subjects $list --hemi rh --meas volume --skip --tablefile aparc_volume_rh.txt
aparcstats2table --hemi lh --subjects $list --parc aparc.a2009s --meas volume --skip -t lh.a2009s.volume.txt
aparcstats2table --hemi rh --subjects $list --parc aparc.a2009s --meas volume --skip -t rh.a2009s.volume.txt
aparcstats2table --hemi lh --subjects $list --parc BA --meas volume --skip -t lh.BA.volume.txt
aparcstats2table --hemi rh --subjects $list --parc BA --meas volume --skip -t rh.BA.volume.txt
