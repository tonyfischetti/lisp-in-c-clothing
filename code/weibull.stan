
data{
    int<lower=0> theLength;
    vector[theLength] total_time;
}

parameters {
    real<lower=0> mshape;
    real<lower=0> mscale;
}

model {
    total_time ~ weibull(mshape, mscale);
}
