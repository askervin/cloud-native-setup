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
# resultdirs=c("")
# inputdir="PATH/TO/DIR/CONTAINING/testnames/WITH/ENDING/SLASH/"

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
		}
	}
}

########## Output pod communication latency page ##############
ltpp = ggplot(data=ltpt, aes(x=n_pods)) + ylab("Latency (ms)") + xlab("pods")
# Highlight the middle percentile (usually median)
# and symmetrically belittle other percentage lines
perc_mid = floor((perc_count+1)/2)
perc_maxdist = perc_mid - 1
for (n in seq(perc_count)) {
	# The sparser the dots the farther away the line is from the middle
	perc_dist = abs(n-perc_mid)
	if (perc_dist != 0) {
		perc_linetype = paste0(2*(1+perc_maxdist-perc_dist), perc_dist+1)
	} else {
		perc_linetype = "solid"
	}
	ltpp = ltpp + geom_line(
		aes_string(y=names(ltpt)[n+1]),
		alpha=1.0 - 0.4 * (perc_dist/perc_maxdist),
		linetype=perc_linetype,
		color="blue")
}

cat("\n\nLatency percentiles illustrated in the Figure below: ", paste0(ltp_perc, "\\%"), "\n\n")

page1 = grid.arrange(ltpp, ncol=1)

# pagebreak, as the graphs overflow the page otherwise
cat("\n\n\\pagebreak\n")
