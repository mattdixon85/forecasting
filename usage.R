
x <- strftime(strptime('2016-01-01','%Y-%m-%d') + (0:70 * 86400),'%Y-%m-%d')
y <- as.numeric(valueofstocks[,1])

plot(y, type='l')

is_train <- 1:length(x) < length(x)/2
x_train <- x[is_train]
y_train <- y[is_train]
x_test <- x[!is_train]
y_test <- y[!is_train]

fc <- forecaster(period=10, k=3)
fc$fit(x_train, y_train)

yh_train <- fc$predict(x_train)
plot(y_train, type='l')
lines(yh_train, type = 'l', lty=3, col='blue')
mean(abs(y_train - yh_train))
summary(fc$get('lm'))

yh_test <- fc$predict(x_test)
plot(y_test, type='l')
lines(yh_test, type = 'l', lty=3, col='blue')
mean(abs(y_test - yh_test))

