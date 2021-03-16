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
          "text": "--  Error handling for synthesis."
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
          "text": "--  Copyright (C) 2017 Tristan Gingold"
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
          "text": "--  This file is part of GHDL."
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
          "text": "--"
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
          "text": "--  This program is free software: you can redistribute it and/or modify"
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
          "text": "--  it under the terms of the GNU General Public License as published by"
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
          "text": "--  the Free Software Foundation, either version 2 of the License, or"
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
          "text": "--  (at your option) any later version."
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
          "text": "--"
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
          "text": "--  This program is distributed in the hope that it will be useful,"
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
          "text": "--  but WITHOUT ANY WARRANTY; without even the implied warranty of"
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
          "text": "--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
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
          "text": "--  GNU General Public License for more details."
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
          "text": "--"
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
          "text": "--  You should have received a copy of the GNU General Public License"
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
          "text": "--  along with this program.  If not, see <gnu.org/licenses>."
        }
      ]
    },
    {
      "kind": "line",
      "number": 18,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 19,
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
          "text": "Types"
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
          "text": "Types"
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
      "number": 20,
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
          "text": "Errorout"
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
          "text": "Errorout"
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
      "number": 21,
      "children": [
      ]
    },
    {
      "kind": "line",
      "number": 22,
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
          "text": "Synth.Errors",
          "href": "docs/synth__errors___spec.html#L22C15"
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
      "number": 23,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "procedure"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Error_Msg_Synth",
          "href": "docs/synth__errors___spec.html#L23C14"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Loc",
          "href": "docs/synth__errors___spec.html#L23C31"
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
          "cssClass": "identifier",
          "text": "Location_Type",
          "href": "docs/types___spec.html#L121C9"
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
      "number": 24,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                              "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Msg",
          "href": "docs/synth__errors___spec.html#L24C31"
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
          "cssClass": "identifier",
          "text": "String"
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
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                              "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Arg1",
          "href": "docs/synth__errors___spec.html#L25C31"
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
          "cssClass": "identifier",
          "text": "Earg_Type",
          "href": "docs/errorout___spec.html#L279C9"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
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
      "number": 26,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "   "
        },
        {
          "kind": "span",
          "cssClass": "keyword",
          "text": "procedure"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Error_Msg_Synth",
          "href": "docs/synth__errors___spec.html#L26C14"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Loc",
          "href": "docs/synth__errors___spec.html#L26C31"
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
          "cssClass": "identifier",
          "text": "Location_Type",
          "href": "docs/types___spec.html#L121C9"
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
      "number": 27,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                              "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Msg",
          "href": "docs/synth__errors___spec.html#L27C31"
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
          "cssClass": "identifier",
          "text": "String"
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
      "number": 28,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                              "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Args",
          "href": "docs/synth__errors___spec.html#L28C31"
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
          "cssClass": "identifier",
          "text": "Earg_Arr",
          "href": "docs/errorout___spec.html#L165C9"
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
          "cssClass": "identifier",
          "text": "No_Eargs"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
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
          "cssClass": "keyword",
          "text": "procedure"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Warning_Msg_Synth",
          "href": "docs/synth__errors___spec.html#L29C14"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Loc",
          "href": "docs/synth__errors___spec.html#L29C33"
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
          "cssClass": "identifier",
          "text": "Location_Type",
          "href": "docs/types___spec.html#L121C9"
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
          "text": "                                "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Msg",
          "href": "docs/synth__errors___spec.html#L30C33"
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
          "cssClass": "identifier",
          "text": "String"
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
          "text": "                                "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Arg1",
          "href": "docs/synth__errors___spec.html#L31C33"
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
          "cssClass": "identifier",
          "text": "Earg_Type",
          "href": "docs/errorout___spec.html#L279C9"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
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
          "cssClass": "keyword",
          "text": "procedure"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Warning_Msg_Synth",
          "href": "docs/synth__errors___spec.html#L32C14"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Loc",
          "href": "docs/synth__errors___spec.html#L32C33"
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
          "cssClass": "identifier",
          "text": "Location_Type",
          "href": "docs/types___spec.html#L121C9"
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
          "text": "                                "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Msg",
          "href": "docs/synth__errors___spec.html#L33C33"
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
          "cssClass": "identifier",
          "text": "String"
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
          "text": "                                "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Args",
          "href": "docs/synth__errors___spec.html#L34C33"
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
          "cssClass": "identifier",
          "text": "Earg_Arr",
          "href": "docs/errorout___spec.html#L165C9"
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
          "cssClass": "identifier",
          "text": "No_Eargs"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
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
          "cssClass": "keyword",
          "text": "procedure"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Info_Msg_Synth",
          "href": "docs/synth__errors___spec.html#L35C14"
        },
        {
          "kind": "span",
          "cssClass": "text",
          "text": " "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "("
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Loc",
          "href": "docs/synth__errors___spec.html#L35C30"
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
          "cssClass": "identifier",
          "text": "Location_Type",
          "href": "docs/types___spec.html#L121C9"
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
          "text": "                             "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Msg",
          "href": "docs/synth__errors___spec.html#L36C30"
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
          "cssClass": "identifier",
          "text": "String"
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
      "number": 37,
      "children": [
        {
          "kind": "span",
          "cssClass": "text",
          "text": "                             "
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": "Args",
          "href": "docs/synth__errors___spec.html#L37C30"
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
          "cssClass": "identifier",
          "text": "Earg_Arr",
          "href": "docs/errorout___spec.html#L165C9"
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
          "cssClass": "identifier",
          "text": "No_Eargs"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ")"
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
      "number": 38,
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
          "text": "Synth.Errors"
        },
        {
          "kind": "span",
          "cssClass": "identifier",
          "text": ";"
        }
      ]
    }
  ],
  "label": "synth-errors.ads"
};