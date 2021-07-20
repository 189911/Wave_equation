# -*- coding: utf-8 -*-
"""
Created on Tue Nov 24 19:24:18 2020

@author: renec
"""

import numpy as np
import matplotlib.pyplot as plt
import time 
from mpl_toolkits.mplot3d import Axes3D
from IPython import display
datos=np.loadtxt("archivo.txt",skiprows=0)
x=np.array(datos[:,0])
y=np.array(datos[:,1])
z=np.array(datos[:,2])
print(datos.shape)
print(y.shape)
print(z)

fig=plt.figure()
ax=Axes3D(fig)


ax.plot(x[:,],y[:,],z[:,])

plt.show()

