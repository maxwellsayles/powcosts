import os
from glob import glob

env = Environment(CCFLAGS=['-O3', '-Wall', '-Werror', '-DNDEBUG'],
                  CPPPATH=['..', '/home/max/github'])

ccfiles = glob('*.cc')
ccfiles += ['/home/max/github/libqform/libqform.a',
            '/home/max/github/liboptarith/liboptarithxx.a']


libs = []
if os.path.isfile('/usr/local/lib/libgmp.a'):
    ccfiles += ['/usr/local/lib/libgmp.a']
elif os.path.isfile('/usr/lib/libgmp.a'):
    ccfiles += ['/usr/lib/libgmp.a']
else:
    libs += ['gmp']

env.Program(target='main', 
            source=ccfiles, 
            LIBS=libs)
