
data{
    int<lower=0> theLength;
    vector[theLength] total_time;
}

parameters {
    real<lower=0> mmean;
    real<lower=0> mstddev;
}

model {
    total_time ~ normal(mmean, mstddev);
}
