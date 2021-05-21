
library(aws.s3)
libary(ggplot2)
source('env.R')
ANALYTICS.BUCKET = 'endinghiv.analytics'

MIN.ANALYTICS.DATE = as.Date(strptime("2021-05-15", format='%Y-%m-%d'))
    # records before this date are erroneously saved

get.analytics.df <- function(from=-90,
                             to=0,
                             relative.to.from=T)
{
    if (relative.to.from)
    {
        from = Sys.Date() + from
        to = Sys.Date() + to
    }
    from = max(MIN.ANALYTICS.DATE, from)
    
    rv = NULL
    if (from <= to)
    {
        for (delta in 0:as.numeric(to-from))
        {
            date = from + delta
            prefix = format(date, '%Y/%m/%d')
            
            contents = get_bucket(ANALYTICS.BUCKET, prefix=prefix, max=Inf)
            for (elem in contents)
            {
                s3load(elem$Key, bucket=ANALYTICS.BUCKET)
                rv = rbind(rv, df)
            }
        }
    }
    
    rv
}

plot.requests.by.time <- function(from,
                                  to,
                                  df=get.analytics.df(),
                                  geom=c('line','bar')[1])
{
    date = as.Date(df$created_at)
    
    
    
    ggplot(df) + aes(created_at)
}

if (1==2)
{
    test.df = data.frame(hi=1, hello=2)
    
    bucket = bucket
}