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

from setuptools import setup

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
    ],
    url='https://github.com/rigetticomputing/rpcq.git',
    description='''The RPC framework and message specification for Rigetti QCS.''',
    long_description=long_description,
    install_requires=[
        'future',
        'msgpack>=0.5.2',
        'python-rapidjson',
        'pyzmq>=17',
        'ruamel.yaml',
        'typing'
    ],
    keywords='quantum rpc qcs',
    python_requires='>=3.5',
)

# restore version.py to its previous state
with open('rpcq/version.py', 'w') as f:
    f.write(version_file_source)
