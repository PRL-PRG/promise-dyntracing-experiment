#!/bin/sh
# ------------------------------------------------------------------
# This material is distributed under the GNU General Public License
# Version 2. You may review the terms of this license at
# http://www.gnu.org/licenses/gpl-2.0.html
#
# Copyright (c) 2012-2016, Ingo Korb, Michel Lang, Helena Kotthaus,
# TU Dortmund University
#
# All rights reserved.
# ------------------------------------------------------------------

SUBSAMPLES=3

./autobench.pl --in r31 --rep 5 \
    "bench-data.R classif MAGICGammaTelescope ada          $SUBSAMPLES" \
    "bench-data.R classif MAGICGammaTelescope ctree        $SUBSAMPLES" \
    "bench-data.R classif MAGICGammaTelescope gbm          $SUBSAMPLES" \
    "bench-data.R classif MAGICGammaTelescope kknn         $SUBSAMPLES" \
    "bench-data.R classif MAGICGammaTelescope ksvm         $SUBSAMPLES" \
    "bench-data.R classif MAGICGammaTelescope lda          $SUBSAMPLES" \
    "bench-data.R classif MAGICGammaTelescope logreg       $SUBSAMPLES" \
    "bench-data.R classif MAGICGammaTelescope lssvm        $SUBSAMPLES" \
    "bench-data.R classif MAGICGammaTelescope naiveBayes   $SUBSAMPLES" \
    "bench-data.R classif MAGICGammaTelescope nnet         $SUBSAMPLES" \
    "bench-data.R classif MAGICGammaTelescope randomForest $SUBSAMPLES" \
    "bench-data.R classif MAGICGammaTelescope rda          $SUBSAMPLES" \
    "bench-data.R classif MAGICGammaTelescope rpart        $SUBSAMPLES"
