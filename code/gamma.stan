
data{
    int<lower=0> theLength;
    vector[theLength] total_time;
}

parameters {
    real<lower=0> mshape;
    real<lower=0> mrate;
}

model {
    total_time ~ gamma(mshape, mrate);
}
