from distutils.core import setup, Extension

module_simple = Extension('demo',sources = ['simple.c','byhand_wrap.c'])

setup(name = 'Simple',
      version = '1.0',
      description = 'This is a demo package',
      ext_modules = [module_simple])
