.. # Template created by Patrick Lehmann

Python Class Reference
######################

Reference of all packages and modules:

.. automodule:: {{ node.name }}

.. toctree::
   :maxdepth: 1
{% for item in subnodes %}
   {{ item.name }}
{%- endfor %}
