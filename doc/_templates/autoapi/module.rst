{{ node.name }}
=={{ '=' * node.name|length }}==

.. automodule:: {{ node.name }}

{##}
{%- block modules -%}
{%- if subnodes %}

.. #-----------------------------------
{##}
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

.. #-----------------------------------
{##}
{%- if node.variables %}
**Variables**
{##}
{% for item, obj in node.variables.items() -%}
- :py:data:`{{ item }}`
{% endfor -%}
{%- endif -%}


{%- if node.exceptions %}
{##}
**Exceptions**
{##}
{% for item, obj in node.exceptions.items() -%}
- :py:exc:`{{ item }}`:
  {{ obj|summary }}

{% endfor -%}
{%- endif -%}


{%- if node.classes %}
{##}
**Classes**
{##}
{% for item, obj in node.classes.items() -%}
- :py:class:`{{ item }}`:
  {{ obj|summary }}

{% endfor -%}
{%- endif -%}


{%- if node.functions %}
{##}
**Functions**
{##}
{% for item, obj in node.functions.items() -%}
- :py:func:`{{ item }}`:
  {{ obj|summary }}

{% endfor -%}
{%- endif -%}


{%- block variables -%}
{%- if node.variables %}
{% for item, obj in node.variables.items() %}
.. autodata:: {{ item }}
   :annotation:

   .. code-block:: guess

      {{ obj|pprint|indent(6) }}
{##}
{%- endfor -%}
{%- endif -%}
{%- endblock -%}


{%- block exceptions -%}
{%- if node.exceptions %}

.. #-----------------------------------

{% for item in node.exceptions %}
.. autoexception:: {{ item }}
   :members:
   :private-members:
   :inherited-members:
   :undoc-members:
{##}
   .. rubric:: Inheritance
   .. inheritance-diagram:: {{ item }}
{##}
   .. rubric:: Members
{##}
{%- endfor -%}
{%- endif -%}
{%- endblock -%}


{%- block classes -%}
{%- if node.classes %}

.. #-----------------------------------

{% for item in node.classes %}
.. autoclass:: {{ item }}
   :members:
   :private-members:
   :undoc-members:
   :inherited-members:
{##}
   .. rubric:: Inheritance
   .. inheritance-diagram:: {{ item }}
{##}
   .. rubric:: Members
{##}
{%- endfor -%}
{%- endif -%}
{%- endblock -%}


{%- block functions -%}
{%- if node.functions %}

.. #-----------------------------------

**Functions**

{% for item in node.functions %}
.. autofunction:: {{ item }}
{##}
{%- endfor -%}
{%- endif -%}
{%- endblock -%}
