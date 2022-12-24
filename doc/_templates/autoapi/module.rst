.. # Template modified  by Patrick Lehmann
     * removed automodule on top, because private members are activated for autodoc (no doubled documentation).
     * Made sections like 'submodules' bold text, but no headlines to reduce number of ToC levels.

=={{ '=' * node.name|length }}==
``{{ node.name }}``
=={{ '=' * node.name|length }}==

.. automodule:: {{ node.name }}

{##}
{%- block modules -%}
{%- if subnodes %}

**Submodules**


.. toctree::
{% for item in subnodes %}
   {{ item.name }}
{%- endfor %}
{##}
{%- endif -%}
{%- endblock -%}
{##}
.. currentmodule:: {{ node.name }}
{##}

{%- if node.variables %}

**Variables**

{% for item, obj in node.variables.items() -%}
- :py:data:`{{ item }}`
  {#{ obj|summary }#}
{% endfor -%}
{%- endif -%}

{%- if node.functions %}

**Functions**

{% for item, obj in node.functions.items() -%}
- :py:func:`{{ item }}`:
  {{ obj|summary }}

{% endfor -%}
{%- endif -%}

{%- if node.exceptions %}

**Exceptions**

{% for item, obj in node.exceptions.items() -%}
- :py:exc:`{{ item }}`:
  {{ obj|summary }}

{% endfor -%}
{%- endif -%}

{%- if node.classes %}

**Classes**

{% for item, obj in node.classes.items() -%}
- :py:class:`{{ item }}`:
  {{ obj|summary }}

{% endfor -%}
{%- endif -%}

{%- block variables -%}
{%- if node.variables %}

---------------------

**Variables**

{#% for item, obj in node.variables.items() -%}
- :py:data:`{{ item }}`
{% endfor -%#}

{% for item, obj in node.variables.items() %}
.. autodata:: {{ item }}
   :annotation:

   .. code-block:: text

      {{ obj|pprint|indent(6) }}
{##}
{%- endfor -%}
{%- endif -%}
{%- endblock -%}

{%- block functions -%}
{%- if node.functions %}

---------------------

**Functions**

{% for item in node.functions %}
.. autofunction:: {{ item }}
{##}
{%- endfor -%}
{%- endif -%}
{%- endblock -%}

{%- block exceptions -%}
{%- if node.exceptions %}

---------------------

**Exceptions**

{#% for item, obj in node.exceptions.items() -%}
- :py:exc:`{{ item }}`:
  {{ obj|summary }}

{% endfor -%#}

{% for item in node.exceptions %}
.. autoexception:: {{ item }}

   .. rubric:: Inheritance
   .. inheritance-diagram:: {{ item }}
      :parts: 1
{##}
{%- endfor -%}
{%- endif -%}
{%- endblock -%}

{%- block classes -%}
{%- if node.classes %}

---------------------

**Classes**

{#% for item, obj in node.classes.items() -%}
- :py:class:`{{ item }}`:
  {{ obj|summary }}

{% endfor -%#}

{% for item in node.classes %}
.. autoclass:: {{ item }}
   :members:
   :private-members:
   :special-members:
   :inherited-members:
   :exclude-members: __weakref__

   .. rubric:: Inheritance
   .. inheritance-diagram:: {{ item }}
      :parts: 1
{##}
{%- endfor -%}
{%- endif -%}
{%- endblock -%}
