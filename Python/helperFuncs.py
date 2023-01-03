def df_optimized(df, verbose=True, **kwargs):
    """
    Reduces size of dataframe by downcasting numerical columns
    :param df: input dataframe
    :param verbose: print size reduction if set to True
    :param kwargs:
    :return:
    """
    in_size = df.memory_usage(index=True).sum()
    for type in ["float", "integer"]:
        l_cols = list(df.select_dtypes(include=type))
        for col in l_cols:
            df[col] = pd.to_numeric(df[col], downcast=type)
            if type == "float":
                df[col] = pd.to_numeric(df[col], downcast="integer")
    out_size = df.memory_usage(index=True).sum()
    ratio = (1 - round(out_size / in_size, 2)) * 100
    GB = out_size / 1000000000
    if verbose:
        print("optimized size by {} % | {} GB".format(ratio, GB))
    return df

##############################################################################################
import subprocess
import sys

def install_packages(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])
    
#install_packages("fitter")

#############################################################################################
def installed_packages(package):
    result=package in sys.modules
    print(result)
    
#installed_packages("distributions")

###################################################################################
import scipy.stats as ss
import numpy as np
import matplotlib.pyplot as plt

plt.style.use('seaborn') # pretty matplotlib plots
plt.rcParams['figure.figsize'] = (12, 8)

def plot_normal(x_range=[0, 1], mu=0.5, sigma=0.1, cdf=False, **kwargs):
    '''
    Plots the normal distribution function for a given x range
    If mu and sigma are not provided, standard normal is plotted
    If cdf=True cumulative distribution is plotted
    Passes any keyword arguments to matplotlib plot function
    '''
    x = x = np.linspace(x_range[0], x_range[1], 5000)
    if cdf:
        y = ss.norm.cdf(x, mu, sigma)
    else:
        y = ss.norm.pdf(x, mu, sigma)
    plt.plot(x, y, **kwargs)
    

#plot_normal(mu=0.5, sigma=0.1)

########################################################

import tensorflow_probability as tfp
import numpy as np
import matplotlib.pyplot as plt
tfd = tfp.distributions
plt.style.use('seaborn') # pretty matplotlib plots
plt.rcParams['figure.figsize'] = (12, 8)

def plot_sinar(x_range=[0, 1], mu=0.5, sigma=0.1,skewness = 1.2,tailweight=0, cdf=False, **kwargs):
    '''
    Plots the SinhArcsinh distribution function for a given x range
    If mu and sigma are not provided, default distribution paramters are used
    If cdf=True cumulative distribution is plotted
    Passes any keyword arguments to matplotlib plot function
    '''
    sinar=tfd.SinhArcsinh(mu, sigma, skewness, tailweight)
    x = x = np.linspace(x_range[0], x_range[1], 5000)
    if cdf:
        y = sinar.cdf(x)
    else:
        y = sinar.prob(x)
    plt.plot(x, y, **kwargs)
    
#mu= 0.2,sigma = 0.2,,nu = 1.2, tau = 0.1
#plot_sinar(mu= 0.1,sigma = 0.6,skewness=1.2,tailweight=2,cdf=False)


