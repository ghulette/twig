#!/usr/bin/env python

# From here: http://docs.python.org/extending/building.html

from distutils.core import setup, Extension

module_simple = Extension('simple',sources = ['simple.c','module_simple.c'])

setup(name = 'Simple',
      version = '1.0',
      description = 'This is a demo package',
      ext_modules = [module_simple])
