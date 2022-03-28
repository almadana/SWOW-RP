
Yes, almost all scripts for the paper (data analysis and plots) are in the shared paperSwow1 folder: https://www.dropbox.com/s/8o3cvsox97m4xaz/paperSwow1.Rproj?dl=0
there's an R project there as well. Even as I tried to be consistent on commenting and naming files and variables, scripts can be a bit messy. Sorry for that. Just ask me if something is hard to follow.

The "main_analyses.r" script gives you an overview of the main analysis scripts. The "settings.r" script is the first you need to run, and it loads the data. In my folder I have symbolic links to the main output files of the main SWOWES-UY2018 folder such as cueStats, strength and responseStats files. Since Dropbox no longer syncs them, I guess you can try and add them manually, or maybe just copy these files from the other folder. However, if you load the project you'll probably have all the necessary variables already in the environment. There's also a ".RData" file with all that as well.

Anyway, the files you'll need to source to reproduce the analysis are:
  
  #main  script of the whole gender analysis thing. it uses ANCORA and ESPAL data to assign POS and gender to cues and responses
  source('scripts/compare_mf_cues.r')

#base gender stats-tables - some stats
source('scripts/gender_base.r')

#compare cue pairs - this calculates the cosine distances between cue pairs
source('scripts/compare_cue_pairs.r')

#bootstrap analysis - It takes a lot of time. I just ran it once and stored the results.
source('scripts/bootstrap_cue_sim.r')

#cosine similarity of cue pairs from the whole graph! #I used this to load the whole G_rw matrix and calculate cosine similarities from there... now unused in the paper
#source('scripts/compare_cue_pairs_cosine_sim.r') # 10GB+ of RAM needed!

#many unused plots, go to the "by POS" section to see the figure is currently in the manuscript
source('scripts/cue_pair_merge_and_plots.r')


Also, the "assortativity" or gender agreement between cues and responses figure is currently in line 92 of "assortativity.r". You'll need to run most of that file to get there.

Just let me know if this makes sense.

Cheers!