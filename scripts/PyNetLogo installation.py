# -*- coding: utf-8 -*-
"""
Created on Wed Aug 24 16:20:03 2022

@author: Femke Keij
"""
%matplotlib inline

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_style('white')
sns.set_context('talk')

import pyNetLogo

netlogo = pyNetLogo.NetLogoLink(gui = TRUE)

netlogo.load_model('./models/Wolf Sheep Predation_v6.nlogo')
netlogo.command('setup')