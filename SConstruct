import os
from glob import glob

# env = Environment(CCFLAGS=['-O3', '-Wall', '-Werror',
#                            '-DNDEBUG', '-std=c++11'],
#                   CPPPATH=['..'])
env = Environment(CCFLAGS=['-O3', '-Wall', '-Werror',
                           '-std=c++11'],
                  CPPPATH=['..'])

ccfiles = glob('*.cc')
ccfiles += ['../libqform/libqform.a',
            '../liboptarith/liboptarithxx.a']


libs = ['rt']
if os.path.isfile('/usr/local/lib/libgmp.a'):
    ccfiles += ['/usr/local/lib/libgmp.a']
elif os.path.isfile('/usr/lib/libgmp.a'):
    ccfiles += ['/usr/lib/libgmp.a']
else:
    libs += ['gmp']

env.Program(target='main', 
            source=ccfiles, 
            LIBS=libs)
