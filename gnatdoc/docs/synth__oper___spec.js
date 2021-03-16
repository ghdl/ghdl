GNATdoc.Documentation = {
  "label": "Synth.Oper",
  "qualifier": "",
  "summary": [
  ],
  "description": [
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Create_Bounds_From_Length",
          "qualifier": "",
          "line": 42,
          "column": 13,
          "src": "srcs/synth-oper.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Create_Bounds_From_Length",
                      "href": "docs/synth__oper___spec.html#L42C13"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 43,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Syn_Inst",
                      "href": "docs/synth__oper___spec.html#L43C7"
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
                      "text": "Synth_Instance_Acc",
                      "href": "docs/synth__context___spec.html#L36C9"
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
                      "cssClass": "identifier",
                      "text": "Atype",
                      "href": "docs/synth__oper___spec.html#L43C38"
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
                      "text": "Iir",
                      "href": "docs/vhdl__nodes___spec.html#L6945C12"
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
                      "cssClass": "identifier",
                      "text": "Len",
                      "href": "docs/synth__oper___spec.html#L43C51"
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
                      "text": "Iir_Index32",
                      "href": "docs/vhdl__nodes___spec.html#L6327C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 44,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Bound_Type",
                      "href": "docs/synth__objtypes___spec.html#L49C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Syn_Inst",
              "line": 43,
              "column": 7,
              "type": {
                "label": "Synth.Context.Synth_Instance_Acc",
                "docHref": "docs/synth__context___spec.html#L36C9"
              },
              "description": [
              ]
            },
            {
              "label": "Atype",
              "line": 43,
              "column": 38,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Len",
              "line": 43,
              "column": 51,
              "type": {
                "label": "Vhdl.Nodes.Iir_Index32",
                "docHref": "docs/vhdl__nodes___spec.html#L6327C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Synth_Dyadic_Operation",
          "qualifier": "",
          "line": 31,
          "column": 13,
          "src": "srcs/synth-oper.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Synth_Dyadic_Operation",
                      "href": "docs/synth__oper___spec.html#L31C13"
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
                      "text": "Syn_Inst",
                      "href": "docs/synth__oper___spec.html#L31C37"
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
                      "text": "Synth_Instance_Acc",
                      "href": "docs/synth__context___spec.html#L36C9"
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
                      "text": "                                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Imp",
                      "href": "docs/synth__oper___spec.html#L32C37"
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
                      "text": "Node",
                      "href": "docs/vhdl__nodes___spec.html#L6946C12"
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
                      "text": "                                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Left_Expr",
                      "href": "docs/synth__oper___spec.html#L33C37"
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
                      "text": "Node",
                      "href": "docs/vhdl__nodes___spec.html#L6946C12"
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
                      "text": "                                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Right_Expr",
                      "href": "docs/synth__oper___spec.html#L34C37"
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
                      "text": "Node",
                      "href": "docs/vhdl__nodes___spec.html#L6946C12"
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
                      "text": "                                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Expr",
                      "href": "docs/synth__oper___spec.html#L35C37"
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
                      "text": "Node",
                      "href": "docs/vhdl__nodes___spec.html#L6946C12"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Valtyp",
                      "href": "docs/synth__values___spec.html#L91C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Syn_Inst",
              "line": 31,
              "column": 37,
              "type": {
                "label": "Synth.Context.Synth_Instance_Acc",
                "docHref": "docs/synth__context___spec.html#L36C9"
              },
              "description": [
              ]
            },
            {
              "label": "Imp",
              "line": 32,
              "column": 37,
              "type": {
                "label": "Vhdl.Nodes.Node",
                "docHref": "docs/vhdl__nodes___spec.html#L6946C12"
              },
              "description": [
              ]
            },
            {
              "label": "Left_Expr",
              "line": 33,
              "column": 37,
              "type": {
                "label": "Vhdl.Nodes.Node",
                "docHref": "docs/vhdl__nodes___spec.html#L6946C12"
              },
              "description": [
              ]
            },
            {
              "label": "Right_Expr",
              "line": 34,
              "column": 37,
              "type": {
                "label": "Vhdl.Nodes.Node",
                "docHref": "docs/vhdl__nodes___spec.html#L6946C12"
              },
              "description": [
              ]
            },
            {
              "label": "Expr",
              "line": 35,
              "column": 37,
              "type": {
                "label": "Vhdl.Nodes.Node",
                "docHref": "docs/vhdl__nodes___spec.html#L6946C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Synth_Monadic_Operation",
          "qualifier": "",
          "line": 37,
          "column": 13,
          "src": "srcs/synth-oper.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Synth_Monadic_Operation",
                      "href": "docs/synth__oper___spec.html#L37C13"
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
                      "text": "Syn_Inst",
                      "href": "docs/synth__oper___spec.html#L37C38"
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
                      "text": "Synth_Instance_Acc",
                      "href": "docs/synth__context___spec.html#L36C9"
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
                      "cssClass": "text",
                      "text": "                                     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Imp",
                      "href": "docs/synth__oper___spec.html#L38C38"
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
                      "text": "Node",
                      "href": "docs/vhdl__nodes___spec.html#L6946C12"
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
                  "number": 39,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Operand_Expr",
                      "href": "docs/synth__oper___spec.html#L39C38"
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
                      "text": "Node",
                      "href": "docs/vhdl__nodes___spec.html#L6946C12"
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
                  "number": 40,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Loc",
                      "href": "docs/synth__oper___spec.html#L40C38"
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
                      "text": "Node",
                      "href": "docs/vhdl__nodes___spec.html#L6946C12"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Valtyp",
                      "href": "docs/synth__values___spec.html#L91C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Syn_Inst",
              "line": 37,
              "column": 38,
              "type": {
                "label": "Synth.Context.Synth_Instance_Acc",
                "docHref": "docs/synth__context___spec.html#L36C9"
              },
              "description": [
              ]
            },
            {
              "label": "Imp",
              "line": 38,
              "column": 38,
              "type": {
                "label": "Vhdl.Nodes.Node",
                "docHref": "docs/vhdl__nodes___spec.html#L6946C12"
              },
              "description": [
              ]
            },
            {
              "label": "Operand_Expr",
              "line": 39,
              "column": 38,
              "type": {
                "label": "Vhdl.Nodes.Node",
                "docHref": "docs/vhdl__nodes___spec.html#L6946C12"
              },
              "description": [
              ]
            },
            {
              "label": "Loc",
              "line": 40,
              "column": 38,
              "type": {
                "label": "Vhdl.Nodes.Node",
                "docHref": "docs/vhdl__nodes___spec.html#L6946C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Synth_Operator_Function_Call",
          "qualifier": "",
          "line": 28,
          "column": 13,
          "src": "srcs/synth-oper.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Synth_Operator_Function_Call",
                      "href": "docs/synth__oper___spec.html#L28C13"
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
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Syn_Inst",
                      "href": "docs/synth__oper___spec.html#L29C7"
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
                      "text": "Synth_Instance_Acc",
                      "href": "docs/synth__context___spec.html#L36C9"
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
                      "cssClass": "identifier",
                      "text": "Expr",
                      "href": "docs/synth__oper___spec.html#L29C38"
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
                      "text": "Node",
                      "href": "docs/vhdl__nodes___spec.html#L6946C12"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Valtyp",
                      "href": "docs/synth__values___spec.html#L91C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Syn_Inst",
              "line": 29,
              "column": 7,
              "type": {
                "label": "Synth.Context.Synth_Instance_Acc",
                "docHref": "docs/synth__context___spec.html#L36C9"
              },
              "description": [
              ]
            },
            {
              "label": "Expr",
              "line": 29,
              "column": 38,
              "type": {
                "label": "Vhdl.Nodes.Node",
                "docHref": "docs/vhdl__nodes___spec.html#L6946C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Synth_Predefined_Function_Call",
          "qualifier": "",
          "line": 26,
          "column": 13,
          "src": "srcs/synth-oper.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Synth_Predefined_Function_Call",
                      "href": "docs/synth__oper___spec.html#L26C13"
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
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Syn_Inst",
                      "href": "docs/synth__oper___spec.html#L27C7"
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
                      "text": "Synth_Instance_Acc",
                      "href": "docs/synth__context___spec.html#L36C9"
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
                      "cssClass": "identifier",
                      "text": "Expr",
                      "href": "docs/synth__oper___spec.html#L27C38"
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
                      "text": "Node",
                      "href": "docs/vhdl__nodes___spec.html#L6946C12"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Valtyp",
                      "href": "docs/synth__values___spec.html#L91C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Syn_Inst",
              "line": 27,
              "column": 7,
              "type": {
                "label": "Synth.Context.Synth_Instance_Acc",
                "docHref": "docs/synth__context___spec.html#L36C9"
              },
              "description": [
              ]
            },
            {
              "label": "Expr",
              "line": 27,
              "column": 38,
              "type": {
                "label": "Vhdl.Nodes.Node",
                "docHref": "docs/vhdl__nodes___spec.html#L6946C12"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Subprograms"
    }
  ]
};