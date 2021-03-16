GNATdoc.SourceFile = {
  "kind": "code",
  "children": [
    {
      "kind": "line",
      "number": 1,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  GHDL Run Time (GRT) - wave dumper (GHW) declarations."
        }
      ]
    },
    {
      "kind": "line",
      "number": 2,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Copyright (C) 2016 Tristan Gingold"
        }
      ]
    },
    {
      "kind": "line",
      "number": 3,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--"
        }
      ]
    },
    {
      "kind": "line",
      "number": 4,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  This program is free software: you can redistribute it and/or modify"
        }
      ]
    },
    {
      "kind": "line",
      "number": 5,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  it under the terms of the GNU General Public License as published by"
        }
      ]
    },
    {
      "kind": "line",
      "number": 6,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  the Free Software Foundation, either version 2 of the License, or"
        }
      ]
    },
    {
      "kind": "line",
      "number": 7,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  (at your option) any later version."
        }
      ]
    },
    {
      "kind": "line",
      "number": 8,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--"
        }
      ]
    },
    {
      "kind": "line",
      "number": 9,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  This program is distributed in the hope that it will be useful,"
        }
      ]
    },
    {
      "kind": "line",
      "number": 10,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  but WITHOUT ANY WARRANTY; without even the implied warranty of"
        }
      ]
    },
    {
      "kind": "line",
      "number": 11,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
        }
      ]
    },
    {
      "kind": "line",
      "number": 12,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  GNU General Public License for more details."
        }
      ]
    },
    {
      "kind": "line",
      "number": 13,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--"
        }
      ]
    },
    {
      "kind": "line",
      "number": 14,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  You should have received a copy of the GNU General Public License"
        }
      ]
    },
    {
      "kind": "line",
      "number": 15,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  along with this program.  If not, see <gnu.org/licenses>."
        }
      ]
    },
    {
      "kind": "line",
      "number": 16,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--"
        }
      ]
    },
    {
      "kind": "line",
      "number": 17,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  As a special exception, if other files instantiate generics from this"
        }
      ]
    },
    {
      "kind": "line",
      "number": 18,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  unit, or you link this unit with other files to produce an executable,"
        }
      ]
    },
    {
      "kind": "line",
      "number": 19,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  this unit does not by itself cause the resulting executable to be"
        }
      ]
    },
    {
      "kind": "line",
      "number": 20,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  covered by the GNU General Public License. This exception does not"
        }
      ]
    },
    {
      "kind": "line",
      "number": 21,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  however invalidate any other reasons why the executable file might be"
        }
      ]
    },
    {
      "kind": "line",
      "number": 22,
      "children": [
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  covered by the GNU Public License."
        }
      ]
    },
    {
      "kind": "line",
      "number": 23,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 24,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "with"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Interfaces"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "use"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Interfaces"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 25,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 26,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "package"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Grt.Ghw",
          "href": "docs/grt__ghw___spec.html#L26C13"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        }
      ]
    },
    {
      "kind": "line",
      "number": 27,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Hierarhcy elements."
        }
      ]
    },
    {
      "kind": "line",
      "number": 28,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Design",
          "href": "docs/grt__ghw___spec.html#L28C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "       "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "1"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 29,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Block",
          "href": "docs/grt__ghw___spec.html#L29C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "        "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "3"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 30,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Generate_If",
          "href": "docs/grt__ghw___spec.html#L30C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "  "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "4"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 31,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Generate_For",
          "href": "docs/grt__ghw___spec.html#L31C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "5"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 32,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Instance",
          "href": "docs/grt__ghw___spec.html#L32C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "6"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 33,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Package",
          "href": "docs/grt__ghw___spec.html#L33C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "7"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 34,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Process",
          "href": "docs/grt__ghw___spec.html#L34C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "13"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 35,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Generic",
          "href": "docs/grt__ghw___spec.html#L35C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "14"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 36,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Eos",
          "href": "docs/grt__ghw___spec.html#L36C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "          "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "15"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  End of scope."
        }
      ]
    },
    {
      "kind": "line",
      "number": 37,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Signal",
          "href": "docs/grt__ghw___spec.html#L37C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "       "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "16"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Signal."
        }
      ]
    },
    {
      "kind": "line",
      "number": 38,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Port_In",
          "href": "docs/grt__ghw___spec.html#L38C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "17"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Port"
        }
      ]
    },
    {
      "kind": "line",
      "number": 39,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Port_Out",
          "href": "docs/grt__ghw___spec.html#L39C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "18"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Port"
        }
      ]
    },
    {
      "kind": "line",
      "number": 40,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Port_Inout",
          "href": "docs/grt__ghw___spec.html#L40C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "19"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Port"
        }
      ]
    },
    {
      "kind": "line",
      "number": 41,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Port_Buffer",
          "href": "docs/grt__ghw___spec.html#L41C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "  "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "20"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Port"
        }
      ]
    },
    {
      "kind": "line",
      "number": 42,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Hie_Port_Linkage",
          "href": "docs/grt__ghw___spec.html#L42C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "21"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Port"
        }
      ]
    },
    {
      "kind": "line",
      "number": 43,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 44,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Type kind was initially ghdl_rtik, but to avoid coupling, we are now"
        }
      ]
    },
    {
      "kind": "line",
      "number": 45,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  using Ghw_Rtik (with old values)."
        }
      ]
    },
    {
      "kind": "line",
      "number": 46,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "type"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "is"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "new"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Unsigned_8"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 47,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Error",
          "href": "docs/grt__ghw___spec.html#L47C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "0"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 48,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Eos",
          "href": "docs/grt__ghw___spec.html#L48C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "15"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  End of scope."
        }
      ]
    },
    {
      "kind": "line",
      "number": 49,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 50,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Signal",
          "href": "docs/grt__ghw___spec.html#L50C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "       "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "16"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Signal."
        }
      ]
    },
    {
      "kind": "line",
      "number": 51,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Port_In",
          "href": "docs/grt__ghw___spec.html#L51C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "      "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "17"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Port"
        }
      ]
    },
    {
      "kind": "line",
      "number": 52,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Port_Out",
          "href": "docs/grt__ghw___spec.html#L52C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "18"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Port"
        }
      ]
    },
    {
      "kind": "line",
      "number": 53,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Port_Inout",
          "href": "docs/grt__ghw___spec.html#L53C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "19"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Port"
        }
      ]
    },
    {
      "kind": "line",
      "number": 54,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Port_Buffer",
          "href": "docs/grt__ghw___spec.html#L54C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "  "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "20"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Port"
        }
      ]
    },
    {
      "kind": "line",
      "number": 55,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Port_Linkage",
          "href": "docs/grt__ghw___spec.html#L55C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "21"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Port"
        }
      ]
    },
    {
      "kind": "line",
      "number": 56,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 57,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Type_B2",
          "href": "docs/grt__ghw___spec.html#L57C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "  "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "22"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 58,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Type_E8",
          "href": "docs/grt__ghw___spec.html#L58C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "  "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "23"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 59,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Type_E32",
          "href": "docs/grt__ghw___spec.html#L59C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "24"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "  "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Not used in waves"
        }
      ]
    },
    {
      "kind": "line",
      "number": 60,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Type_I32",
          "href": "docs/grt__ghw___spec.html#L60C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "25"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 61,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Type_I64",
          "href": "docs/grt__ghw___spec.html#L61C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "26"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 62,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Type_F64",
          "href": "docs/grt__ghw___spec.html#L62C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "27"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 63,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Type_P32",
          "href": "docs/grt__ghw___spec.html#L63C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "28"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 64,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Type_P64",
          "href": "docs/grt__ghw___spec.html#L64C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "29"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 65,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Type_Array",
          "href": "docs/grt__ghw___spec.html#L65C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "     "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "31"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 66,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Type_Record",
          "href": "docs/grt__ghw___spec.html#L66C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "    "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "32"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 67,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_Scalar",
          "href": "docs/grt__ghw___spec.html#L67C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "34"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 68,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_Array",
          "href": "docs/grt__ghw___spec.html#L68C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "  "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "35"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 69,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_Unbounded_Array",
          "href": "docs/grt__ghw___spec.html#L69C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "  "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "37"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 70,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_Record",
          "href": "docs/grt__ghw___spec.html#L70C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "38"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 71,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_Unbounded_Record",
          "href": "docs/grt__ghw___spec.html#L71C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "39"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 72,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 73,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "comment",
          "text": "--  Not used in waves"
        }
      ]
    },
    {
      "kind": "line",
      "number": 74,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_B1",
          "href": "docs/grt__ghw___spec.html#L74C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "  "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "41"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 75,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_E8",
          "href": "docs/grt__ghw___spec.html#L75C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": "  "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "42"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 76,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_E32",
          "href": "docs/grt__ghw___spec.html#L76C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "43"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 77,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_I32",
          "href": "docs/grt__ghw___spec.html#L77C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "44"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 78,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_I64",
          "href": "docs/grt__ghw___spec.html#L78C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "45"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 79,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_F64",
          "href": "docs/grt__ghw___spec.html#L79C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "46"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 80,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_P32",
          "href": "docs/grt__ghw___spec.html#L80C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "47"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 81,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik_Subtype_P64",
          "href": "docs/grt__ghw___spec.html#L81C4"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "constant"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Ghw_Rtik",
          "href": "docs/grt__ghw___spec.html#L46C9"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ":="
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "number",
          "text": "48"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    },
    {
      "kind": "line",
      "number": 82,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 83,
      "children": [
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "end"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Grt.Ghw"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    }
  ],
  "label": "grt-ghw.ads"
};