# plot_and_find_steps

 plots time series data and finds steps from optical trapping experiments of molecular motors

#Algis Toleikis 2015-2019 algirdas.toleikis@gmail.com

#plot traces from optical trapping of motor proteins
#options: 
0) change the 'filename' and 'dataDirectory' accordingly. 'codeDirectory' as well if needed
1) plot the whole file or just a section: minx maxx
2) filter data with a moving-average filter: n
3) **key feature**: find steps (based on a custom t-score finding algorithm - Cross and Carter Nature 2005)
adjust stepfinder parametres: box, thr, minstep, minF, stepdif
Note: The step-finder itself is in a separate file. Also - peak finder. 
4) x&y can be rotated
5) comments from the experiment can be added (if any): comments
6) plots are produced and these features are added: detected steps, comments, second y axis (force)

#file format
#txt file with a header of n-lines, each line starts like this "Hertz =  50000Hz"
#data with 3 columns: Time, x, y
#typically 3-10 mln lines


test file included (testfile.txt)
expected result is below. Green are the detected steps
![Image description](https://github.com/algyz200/plot_traces/blob/master/testfile_result.png)
