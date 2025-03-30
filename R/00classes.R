setOldClass(c('servsockconn', 'sockconn'))
setClassUnion('optional_sockconn', c('servsockconn', 'sockconn', 'NULL'))
