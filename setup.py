""""""

import os

from importlib.machinery import SourceFileLoader
from pkg_resources import parse_requirements
from setuptools import find_packages, setup


module_name = 'rpserver'


module = SourceFileLoader(module_name, 
                          os.path.join(module_name, '__init__.py')).load_module()


def load_requirements(file_name: str) -> list:
	requirements = []
	with open(file_name, 'r') as fn:
		for req in parse_requirements(fn.read()):
			extras = '[{}]'.format(', '.join(req.extras)) if req.extras \
									else ''
			requirements.append(
				'{}{}{}'.format(req.name, extras, req.specifier)
			)
	return requirements

setup(
    name=module_name,
    version=module.__version__,
    author='Ignatenko Oleg',
    author_email='oleg.ignatenko12@gmail.com',
    license=,
    description=,
    platforms='all',
    python_requires='>=3.8',
    install_requires=load_requirements('requirements.txt')
        )
