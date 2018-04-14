# ------------------------------------------------------------------
# This material is distributed under the GNU General Public License
# Version 2. You may review the terms of this license at
# http://www.gnu.org/licenses/gpl-2.0.html
#
# Copyright (c) 2012-2016, Michel Lang, Helena Kotthaus,
# TU Dortmund University
#
# All rights reserved.
# ------------------------------------------------------------------

library(BBmisc)
library(mlr)
library(mlrData)

chars = load2("all_data_chars_1_1_3.RData")
df = subset(chars, type == "classif" & size >= 500 & size <= 50000 & NA.rows < 50 & maxclass/minclass < 5)

for (id in df$id) {
  tmp = getDataset(id, na.action=na.omit)
  task = tmp[["task"]]
  fn = sprintf("%s_%s.RData", task$task.desc$type, id)
  save(task, file = fn)
}


df[order(df$size), ]
