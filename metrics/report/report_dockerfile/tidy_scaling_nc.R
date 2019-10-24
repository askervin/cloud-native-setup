#!/usr/bin/env Rscript
# Copyright (c) 2019 Intel Corporation
#
# SPDX-License-Identifier: Apache-2.0

# Show pod communication latency

suppressMessages(suppressWarnings(library(ggplot2)))	# ability to plot nicely.
suppressMessages(library(jsonlite))			# to load the data.
library(tibble)						# tibbles for tidy data

testnames=c(
	"k8s-scaling-nc.*"
)

### For developers: uncomment following variables to run this as is in R
# resultdirs=c("PATH/TO/RES1/", ...) # keep the ending slash on result paths
# inputdir=""

latencydata=c()

# iterate over every set of results (test run)
for (currentdir in resultdirs) {
	# For every results file we are interested in evaluating
	for (testname in testnames) {
		matchdir=paste(inputdir, currentdir, sep="")
		matchfile=paste(testname, '\\.json', sep="")
		files=list.files(matchdir, pattern=matchfile)

		# For every matching results file
		for (ffound in files) {
			fname=paste(inputdir, currentdir, ffound, sep="")
			if (!file.exists(fname)) {
				warning(paste("Skipping non-existent file: ", fname))
				next
			}

			# Derive the name from the test result dirname
			datasetname=basename(currentdir)

			# Import the data
			fdata=fromJSON(fname)
			# De-nest the test name specific data
			shortname=substr(ffound, 1, nchar(ffound)-nchar(".json"))
			fdata=fdata[[shortname]]
			testname=datasetname

			# All the data we are looking for comes in BootResults,
			# so pick it out to make referencing easier
			br=fdata$BootResults

			########################################################
			#### Now extract latency time percentiles (ltp) ########
			########################################################
			ltp=br$latency_time$Percentiles
			# Percentile thresholds, for example [5, 25, 50, 75, 95]
			ltp_perc=ltp[[1]]
			perc_count = length(ltp_perc)
			# Measured times
			ltp_meas=matrix(unlist(ltp[c(2:length(ltp))]), nrow=perc_count)
			# Build latency percentiles tibble with nice headings
			ltpt=tibble(n_pods=br$n_pods$Result[c(2:length(br$n_pods$Result))])
			for (n in seq(perc_count)) {
				p_title = paste0("p", ltp_perc[n])
				ltpt[p_title] = ltp_meas[n,]
			}
			# ltpt example: with percentiles [5, 50, 95]:
			# n_pods  p5  p50  p95
			#    100   4	8   10
			#    200   5   11   15
			#    300   6   14   19
			ltpt$testname=testname
			latencydata=rbind(latencydata, ltpt)
		}
	}
}

if (length(latencydata[[1]]) <= 20 && perc_count == 5) {
	# Use boxplot when there are not too many boxes to draw
	latp = ggplot(data=latencydata, aes(x=n_pods)) + ylab("Latency (ms)") + xlab("pods")
	p=names(ltpt)[2:6]
	latp = latp + geom_boxplot(aes_string(group="interaction(testname,n_pods)",ymin=p[1],lower=p[2],middle=p[3],upper=p[4],ymax=p[5],fill="testname"),stat="identity")
} else {
	# Use colored areas and median lines when there are many ticks on X axis
	latp = ggplot(data=latencydata, aes(x=n_pods)) + ylab("Latency (ms)") + xlab("pods")
	perc_mid = floor((perc_count)/2)
	perc_maxdist = perc_mid
	plot_number = 0
	for (plot_test in unique(latencydata$testname)) {
		plot_number = plot_number + 1
		plot_color = plot_colors[plot_number]
		for (n in seq(perc_mid)) {
			# First fill outmost areas, like p5..p25 and p75..p95,
			# then areas closer to the middle, like p25..p50 and p50..p75
			lower_name = names(ltpt)[n+1]
			lower_next_name = names(ltpt)[n+2]
			upper_name = names(ltpt)[perc_count-n+2]
			upper_prev_name = names(ltpt)[perc_count-n+1]
			perc_dist = abs(n-perc_mid)
			alpha = 0.4 * (n / perc_mid)**2
			latp = latp + geom_ribbon(data=latencydata[latencydata$testname==plot_test,],aes_string(x="n_pods",ymin=lower_name,ymax=lower_next_name,fill="testname"),alpha=alpha)
			latp = latp + geom_ribbon(data=latencydata[latencydata$testname==plot_test,],aes_string(x="n_pods",ymin=upper_prev_name,ymax=upper_name,fill="testname"),alpha=alpha)
		}
		# Draw median line
		latp = latp + geom_line(data=latencydata[latencydata$testname==plot_test,],aes_string(x="n_pods",y=names(ltpt)[perc_mid+1],color="testname"))
	}
}

cat("\n\nLatency percentiles illustrated in the Figure below: ", paste0(ltp_perc, "\\%"), "\n\n")

page1 = grid.arrange(latp, ncol=1)

# pagebreak, as the graphs overflow the page otherwise
cat("\n\n\\pagebreak\n")
