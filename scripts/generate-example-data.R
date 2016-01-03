x = readRDS('data/x.rds')
y = readRDS('data/y.rds')
time = readRDS('data/time.rds')
event = readRDS('data/event.rds')

write.table(x, file = 'www/example/x.csv', quote = FALSE, sep = ',', row.names = FALSE)
write.table(y, file = 'www/example/y.csv', quote = FALSE, sep = ',', row.names = FALSE)
write.table(x, file = 'www/example/x.tsv', quote = FALSE, sep = '\t', row.names = FALSE)
write.table(y, file = 'www/example/y.tsv', quote = FALSE, sep = '\t', row.names = FALSE)
write.table(x, file = 'www/example/x.txt', quote = FALSE, sep = ';', row.names = FALSE)
write.table(y, file = 'www/example/y.txt', quote = FALSE, sep = ';', row.names = FALSE)

set.seed(97)
idx = sample(1:nrow(x), 500)
x_new = x[idx, ]
y_new = y[idx, ]
write.table(x_new, file = 'www/example/x_new.csv', quote = FALSE, sep = ',', row.names = FALSE)
write.table(y_new, file = 'www/example/y_new.csv', quote = FALSE, sep = ',', row.names = FALSE)
write.table(x_new, file = 'www/example/x_new.tsv', quote = FALSE, sep = '\t', row.names = FALSE)
write.table(y_new, file = 'www/example/y_new.tsv', quote = FALSE, sep = '\t', row.names = FALSE)
write.table(x_new, file = 'www/example/x_new.txt', quote = FALSE, sep = ';', row.names = FALSE)
write.table(y_new, file = 'www/example/y_new.txt', quote = FALSE, sep = ';', row.names = FALSE)
