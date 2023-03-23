##############################################################################
# Copyright 2018 Rigetti Computing
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.
##############################################################################

import sys

from setuptools import setup

if sys.version_info < (3, 6):
    raise ImportError('The rpcq library requires Python 3.6 or above.')

with open('VERSION.txt', 'r') as f:
    __version__ = f.read().strip()

with open('README.md', 'r') as f:
    long_description = f.read()

# save the source code in version.py
with open('rpcq/version.py', 'r') as f:
    version_file_source = f.read()

# overwrite version.py in the source distribution
with open('rpcq/version.py', 'w') as f:
    f.write(f'__version__ = \'{__version__}\'\n')

setup(
    name='rpcq',
    version=__version__,
    author='Rigetti Computing',
    author_email='info@rigetti.com',
    license='Apache-2.0',
    packages=[
        'rpcq',
        'rpcq.external'
    ],
    package_data={'rpcq': ['py.typed']},
    url='https://github.com/rigetticomputing/rpcq.git',
    description='''The RPC framework and message specification for Rigetti QCS.''',
    long_description=long_description,
    long_description_content_type='text/markdown',
    install_requires=[
        "msgpack>=0.6,<2.0",
        "python-rapidjson",
        "pyzmq>=17",
        "ruamel.yaml",
    ],
    keywords='quantum rpc qcs',
    python_requires='>=3.6',
    # zip_safe must be disabled in order for mypy to find the installed py.typed file, according to
    # https://mypy.readthedocs.io/en/latest/installed_packages.html#making-pep-561-compatible-packages
    zip_safe=False
)

# restore version.py to its previous state
with open('rpcq/version.py', 'w') as f:
    f.write(version_file_source)
