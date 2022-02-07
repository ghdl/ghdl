from os.path import dirname, join
import json
import re

# Try to load JSON data from a file. If not found, use the argument as a tag name and retrieve the data from GitHub.
def getJSON(tag='all'):
   f = tag
   tag = '/'+tag
   if f == 'all':
      f = 'releases'
      tag = ''

   try:
      d = json.loads(open(join(dirname(__file__), f+'.json'), 'r').read())
   except:
      from urllib.request import urlopen
      d = json.loads(urlopen('https://api.github.com/repos/ghdl/ghdl/releases'+tag).read())
      json.dump(d, open(f+'.json', 'w'), indent=4)
   return d

#
# Print two versions of each shield. Onee for 'html' (`image::`) and one for 'latex' (`replace::`)
#

def printShieldSrc(label, alt, img, target, latex=False):
   if latex:
      if label[-6:] == '/total':
         label = label[:-6]
      idx = re.compile('[\W_]+').sub('', label)
      print(f"""
.. |l{idx}| replace:: `{label}`_
.. _{label}: {target}
""")
   else:
      print(f'''
.. |{label}| image:: {img}
   :target: {target}
   :height: 22
   :alt: {alt}
   :class: shield
''')

#
# Create shields/badges from JSON file
#

def createShields(file='shields'):
   shields = getJSON(file)
   for k, v in shields.items():
      t = v['target']
      if t[0:3] == 'gh:':
        t = 'https://github.com/' + t[3:]

      printShieldSrc(
         'SHIELD:'+k,
         v['alt'],
         'https://img.shields.io/' + v['src'] + '&style=flat-square&longCache=true',
         t,
         False
      )
