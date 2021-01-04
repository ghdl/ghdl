from os.path import dirname, join
import json

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
# Functions to print table with format `[ [], [], [], ... ]` to reStructuredText
#

# Print a row of data elements.
def printTabRow(l, r):
   printTabItem(l, r, '| ')

# Print a rule. Two characters in 'b' define the type of rule. Expected values are '+-', '+=' or '| '.
def printTabRule(l, b):
   printTabItem(l, [b[1] for x in range(len(l))], b)

# Print a full row, be it a rule or data.
# Extend the width of each field to the size specified in 'l'.
def printTabItem(l, a, b):
   for y, z in enumerate(a):
      print((b + z).ljust(l[y]+3, b[1]), end='')
   print(b[0])

# Get number of cols from number of elements in row 0.
# Compute minimum number of characters required for each col.
def getTabColLens(t):
   cl = [0 for _ in t[0]]
   for row in t:
      for y, z in enumerate(row):
         cl[y] = max(cl[y], len(z))
   return cl

# Print a table using the functions above.
# The first row contains the headers.
def printTab(t):
   clens = getTabColLens(t)

   printTabRule(clens, '+-')
   printTabRow(clens, t[0])
   printTabRule(clens, '+=')
   for x in t[1:]:
      printTabRow(clens, x)
      printTabRule(clens, '+-')
   print()

#
# Print two versions of each shield. Onee for 'html' (`image::`) and one for 'latex' (`replace::`)
#

# Strip all non-alphanumeric characters when creating the labels
def stripLabel(label):
   import re
   pattern = re.compile('[\W_]+')
   return pattern.sub('', label)

def printShieldSrc(label, alt, img, target, latex=False):
   if latex:
      i = stripLabel(label)
      if label[-6:] == '/total':
         label = label[:-6]
      print('.. |l' + i + '| replace:: `' + label + '`_')
      print('.. _' + label + ': ' + target + '\n')
   else:
      print('.. |' + label + '| image:: '+ img + '\n',
            '   :target: ' + target + '\n',
            '   :height: 22\n',
            '   :alt: ' + alt + '\n')

#
# Display better OS and Backend names than those represented in the tarball name
#

def prettyOS(i):
   if i == 'fedora28':
      return 'Fedora 28'
   elif i == 'macosx':
      return 'Max OS X'
   elif i == 'mingw32':
      return 'Windows x86 (MinGW32)'
   elif  i == 'mingw64':
      return 'Windows x86 (MinGW64)'
   elif  i == 'stretch':
      return 'Debian 9 (Stretch)'
   elif  i == 'gpl':
      return 'Debian 9 (Stretch) GPL'
   elif  i == 'ubuntu14':
      return '14.04 LTS (Trusty Tahr)'
   return i

def prettyBackend(i):
   if i == 'llvm':
      return 'LLVM'
   if i == 'llvm-3.8':
      return 'LLVM (3.8)'
   return i

#
# Get, extract and process JSON data to create the shields and table with the assets of a release
#

def createTagShields(data='latest'):
   if isinstance(data, str):
      data = getJSON(data)

   assets=[['OS', 'Backend', 'Size', 'Downloads']]
   tag = data['tag_name']
   for x in data['assets']:
      name = x['name']
      s = []

      p = 'ghdl-gpl-'+tag[1:]
      if name[0:len(p)] == p:
         s = ['gpl', 'mcode']

      p = 'ghdl-'+tag[1:]
      if name[0:len(p)] == p:
         s = name[len(p)+1:-4].split('-',1)

      if len(s) > 1:
         assets.append([
            prettyOS(s[0]),
            prettyBackend(s[1]),
            (str(round(x['size']/1024**2, 2))+' MB').rjust(8),
            '|' + tag + '/' + name + '|']
         )

   assets.append(['Sum:', '', '', '|'+tag+'/total|'])

   for x in assets[1:-1]:
      i = x[3][1:-1]
      for latex in [False, True]:
         printShieldSrc(i, i,
            'https://img.shields.io/github/downloads/ghdl/ghdl/' + i + '?longCache=true&style=flat-square&logo=github&label=%7F',
            'https://github.com/ghdl/ghdl/releases/download/' + i, latex=latex)

   return assets

# TODO: Is github.com/ghdl/ghdl/releases/download/<tag>/<file> subject to rate limit? Is there an alternative (not documented) domain?


#
# Get, extract and process JSON data to create the shields and list/table with all the releases except the latest
#

def createReleasesShields(tag='latest'):
   d = getJSON()
   from dateutil.parser import parse as parseTime
   releases = [['Date', 'Downloads']]
   if tag == 'latest':
      t = d[1] if d[0]['name'] == 'nightly' else d[0]
   for x in d:
      name = x['tag_name']
      if tag == name:
         t = x
      date = parseTime( x['published_at'] ).strftime("%Y-%m-%d")
      releases.append([date, '|'+name+'/total|'])
      i = name
      for l in [False, True]:
         printShieldSrc(i+'/total', i+' Total',
            'https://img.shields.io/github/downloads/ghdl/ghdl/' + i + '/total?longCache=true&style=flat-square&logo=github&label=%7F',
            'https://github.com/ghdl/ghdl/releases/' + i, l)

   out = {'releases': releases, 'assets': createTagShields(t)}
   import json
   json.dump(out, open('data.json', 'w'), indent=4)
   return out


#
# Print the table with all the assets of a release/tag
#

def printReleaseTab(assets, latex=False):
   if isinstance(assets, str):
      assets = getJSON(assets)["assets"]

   if latex:
      for y, z in enumerate(assets[1:]):
         assets[y+1] = z[0:3] + ['|l' + stripLabel(z[3][1:-1]) + '|']

   printTab(assets)

#
# Print list of releases, except the latest (second row)
#

def printReleasesList(releases, latex=False):
   if isinstance(releases, str):
      releases = getJSON(releases)["releases"]

   rs = [releases[0]]

   for x, r in enumerate(releases):
      if 'nightly' in r[1]:
         releases.remove(r)
         break

   rs.extend(releases[2:])

   if latex:
     rs[0] = ['Release/Tag'] + [rs[0][0]]
     for x, r in enumerate(rs[1:]):
        rs[x+1] = ['|l' + stripLabel(r[1][1:-1]) + '|'] + [r[0]]
   else:
     rs[0] = ['Release/Tag'] + rs[0]
     for x, r in enumerate(rs[1:]):
        rs[x+1] = [r[1][1:-7]] + r

   printTab(rs)

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
