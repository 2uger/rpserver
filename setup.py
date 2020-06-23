""""""

import os

from importlib.machinery import SourceFileLoader
from pkg_resources import parse_requirements
from setuptools import find_packages, setup


module_name = 'ridersPlatform'


module = SourceFileLoader(module_name, 
		          os.path.join(module_nam, '__init__.py')).load_module()


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
