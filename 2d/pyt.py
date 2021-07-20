# -*- coding: utf-8 -*-
"""
Created on Sun Nov 15 16:38:05 2020

@author: renec
"""

import numpy as np
import matplotlib.pyplot as plt
import time 
from mpl_toolkits.mplot3d import axes3d
import matplotlib.animation as animation
from IPython import display
datos=np.loadtxt("archivoPy.txt",skiprows=0)
print(datos.shape)
print(datos)
intervalos=150
particulas=860

fig=plt.figure()
ax=fig.gca(projection="3d")
r=0
a=time.time()
print(a)
t=0
def actualizar(i):
    global r
    if r==intervalos-1:
        r=0
    ax.clear()
    ax.plot(datos[r:r+particulas,0],datos[r:r+particulas,1],datos[r:r+particulas,2],linestyle='solid')
    plt.title("hola")
    plt.xlim(0,8)
    plt.xlim(0,8)
    
    
   
    r=r+particulas
  
    #--------reasignamos posiciones-------------
    # atomo.px=atomo.px+atomo.vx*(i-ix)
    # atomo.py=atomo.py+atomo.vy*(i-iy)
ani=animation.FuncAnimation(fig,actualizar,np.arange(0,1000,0.00001),interval=0.00001)
plt.show()
