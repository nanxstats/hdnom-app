x = readRDS('data/x.rds')
y = readRDS('data/y.rds')
time = readRDS('data/time.rds')
event = readRDS('data/event.rds')

set.seed(1010)
idx = sample(1:nrow(x), 600)
x = x[idx, ]
y = y[idx, ]
write.table(x, file = 'www/example/x.csv', quote = FALSE, sep = ',', row.names = FALSE)
write.table(y, file = 'www/example/y.csv', quote = FALSE, sep = ',', row.names = FALSE)
write.table(x, file = 'www/example/x.tsv', quote = FALSE, sep = '\t', row.names = FALSE)
write.table(y, file = 'www/example/y.tsv', quote = FALSE, sep = '\t', row.names = FALSE)
write.table(x, file = 'www/example/x.txt', quote = FALSE, sep = ';', row.names = FALSE)
write.table(y, file = 'www/example/y.txt', quote = FALSE, sep = ';', row.names = FALSE)
