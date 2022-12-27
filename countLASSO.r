# Take csv in 'IVs_Selected_by_LASSO.csv' and create a new column `good` stating
# if te variables selected are those expected. By design these should be the n
# first variables in the set, named X_n.

df = read.csv('IVs_Selected_by_LASSO.csv', header = FALSE, col.names = 'out')

good_model = function (str) {
  str  = unlist(strsplit(str, ';  '))
  desc = str[1]
  pred = str[2]
  n_2  = unlist(strsplit(desc, '-'))[2]
  expt = paste0('X', 1:as.integer(n_2), collapse = ', ')
  identical(pred, expt)
}

df[['good']] = with(df, sapply(out, good_model))

cat('done checking variables selected by LASSO,',
    paste0(with(df, round(mean(good) * 100)), '%'), 'are correct\n')

outfile = 'countLASSO_out.csv'
write.csv(df, file = outfile)
cat('results writen to', outfile, '\n')
