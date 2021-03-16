GNATdoc.Documentation = {
  "label": "Grt.Rtis_Utils",
  "qualifier": "",
  "summary": [
  ],
  "description": [
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Traverse_Result",
          "qualifier": "",
          "line": 35,
          "column": 9,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Traverse_Result",
                      "href": "docs/grt__rtis_utils___spec.html#L35C9"
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
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Traverse_Ok",
                      "href": "docs/grt__rtis_utils___spec.html#L35C29"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Traverse_Skip",
                      "href": "docs/grt__rtis_utils___spec.html#L35C42"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ","
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Traverse_Stop",
                      "href": "docs/grt__rtis_utils___spec.html#L35C57"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";",
                      "href": "docs/grt__rtis_utils___spec.html#L35C9"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Action to perform after a node was handled by the user function:\n"
                },
                {
                  "kind": "span",
                  "text": "Traverse_Ok: continue to process.\n"
                },
                {
                  "kind": "span",
                  "text": "Traverse_Skip: do not traverse children.\n"
                },
                {
                  "kind": "span",
                  "text": "Traverse_Stop: end of walk.\n"
                }
              ]
            }
          ],
          "literals": [
            {
              "label": "Traverse_Ok",
              "line": 35,
              "column": 29,
              "description": [
              ]
            },
            {
              "label": "Traverse_Skip",
              "line": 35,
              "column": 42,
              "description": [
              ]
            },
            {
              "label": "Traverse_Stop",
              "line": 35,
              "column": 57,
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Simple types"
    },
    {
      "entities": [
        {
          "label": "Rti_Object",
          "qualifier": "",
          "line": 38,
          "column": 9,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Rti_Object",
                      "href": "docs/grt__rtis_utils___spec.html#L38C9"
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
                      "text": "record"
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
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Obj",
                      "href": "docs/grt__rtis_utils___spec.html#L39C7"
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
                      "text": "Ghdl_Rti_Access",
                      "href": "docs/grt__rtis___spec.html#L138C9"
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
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Ctxt",
                      "href": "docs/grt__rtis_utils___spec.html#L40C7"
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
                      "text": "Rti_Context",
                      "href": "docs/grt__rtis_addr___spec.html#L40C9"
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
                  "number": 41,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
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
                      "cssClass": "keyword",
                      "text": "record"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";",
                      "href": "docs/grt__rtis_utils___spec.html#L38C9"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "An RTI object is a context and an RTI declaration.\n"
                }
              ]
            }
          ],
          "fields": [
            {
              "label": "Obj",
              "line": 39,
              "column": 7,
              "type": {
                "label": "Grt.Rtis.Ghdl_Rti_Access",
                "docHref": "docs/grt__rtis___spec.html#L138C9"
              },
              "description": [
              ]
            },
            {
              "label": "Ctxt",
              "line": 40,
              "column": 7,
              "type": {
                "label": "Grt.Rtis_Addr.Rti_Context",
                "docHref": "docs/grt__rtis_addr___spec.html#L40C9"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Record types"
    },
    {
      "entities": [
        {
          "label": "Disp_Value",
          "qualifier": "",
          "line": 95,
          "column": 14,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 95,
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
                      "text": "Disp_Value",
                      "href": "docs/grt__rtis_utils___spec.html#L95C14"
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
                      "text": "Stream",
                      "href": "docs/grt__rtis_utils___spec.html#L95C26"
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
                      "text": "FILEs",
                      "href": "docs/grt__stdio___spec.html#L34C9"
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
                  "number": 96,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                         "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/grt__rtis_utils___spec.html#L96C26"
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
                      "text": "Value_Union",
                      "href": "docs/grt__types___spec.html#L207C9"
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
                  "number": 97,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                         "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Type_Rti",
                      "href": "docs/grt__rtis_utils___spec.html#L97C26"
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
                      "text": "Ghdl_Rti_Access",
                      "href": "docs/grt__rtis___spec.html#L138C9"
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
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Disp a value.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Stream",
              "line": 95,
              "column": 26,
              "type": {
                "label": "Grt.Stdio.FILEs",
                "docHref": "docs/grt__stdio___spec.html#L34C9"
              },
              "description": [
              ]
            },
            {
              "label": "Value",
              "line": 96,
              "column": 26,
              "type": {
                "label": "Grt.Types.Value_Union",
                "docHref": "docs/grt__types___spec.html#L207C9"
              },
              "description": [
              ]
            },
            {
              "label": "Type_Rti",
              "line": 97,
              "column": 26,
              "type": {
                "label": "Grt.Rtis.Ghdl_Rti_Access",
                "docHref": "docs/grt__rtis___spec.html#L138C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Foreach_Scalar",
          "qualifier": "",
          "line": 57,
          "column": 14,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Foreach_Scalar",
                      "href": "docs/grt__rtis_utils___spec.html#L57C14"
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
                      "text": "Ctxt",
                      "href": "docs/grt__rtis_utils___spec.html#L57C30"
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
                      "text": "Rti_Context",
                      "href": "docs/grt__rtis_addr___spec.html#L40C9"
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
                      "text": "                             "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Obj_Type",
                      "href": "docs/grt__rtis_utils___spec.html#L58C30"
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
                      "text": "Ghdl_Rti_Access",
                      "href": "docs/grt__rtis___spec.html#L138C9"
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
                      "text": "                             "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Obj_Addr",
                      "href": "docs/grt__rtis_utils___spec.html#L59C30"
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
                      "text": "Address"
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
                  "number": 60,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                             "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Is_Sig",
                      "href": "docs/grt__rtis_utils___spec.html#L60C30"
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
                      "text": "Boolean"
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
                      "text": "                             "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Param",
                      "href": "docs/grt__rtis_utils___spec.html#L61C30"
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
                      "text": "Param_Type"
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
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Ctxt",
              "line": 57,
              "column": 30,
              "type": {
                "label": "Grt.Rtis_Addr.Rti_Context",
                "docHref": "docs/grt__rtis_addr___spec.html#L40C9"
              },
              "description": [
              ]
            },
            {
              "label": "Obj_Type",
              "line": 58,
              "column": 30,
              "type": {
                "label": "Grt.Rtis.Ghdl_Rti_Access",
                "docHref": "docs/grt__rtis___spec.html#L138C9"
              },
              "description": [
              ]
            },
            {
              "label": "Obj_Addr",
              "line": 59,
              "column": 30,
              "type": {
                "label": "Address"
              },
              "description": [
              ]
            },
            {
              "label": "Is_Sig",
              "line": 60,
              "column": 30,
              "type": {
                "label": "Boolean"
              },
              "description": [
              ]
            },
            {
              "label": "Param",
              "line": 61,
              "column": 30,
              "type": {
                "label": "Grt.Rtis_Utils.Foreach_Scalar.Param_Type",
                "docHref": "docs/grt__rtis_utils___spec.html#L52C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Get_Filename",
          "qualifier": "",
          "line": 114,
          "column": 13,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 114,
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
                      "text": "Get_Filename",
                      "href": "docs/grt__rtis_utils___spec.html#L114C13"
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
                      "text": "Ctxt",
                      "href": "docs/grt__rtis_utils___spec.html#L114C27"
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
                      "text": "Rti_Context",
                      "href": "docs/grt__rtis_addr___spec.html#L40C9"
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
                      "text": "Ghdl_C_String",
                      "href": "docs/grt__types___spec.html#L131C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Return the filename in which CTXT is defined.  Used to report locations.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Ctxt",
              "line": 114,
              "column": 27,
              "type": {
                "label": "Grt.Rtis_Addr.Rti_Context",
                "docHref": "docs/grt__rtis_addr___spec.html#L40C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Get_Linecol_Col",
          "qualifier": "",
          "line": 111,
          "column": 13,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 111,
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
                      "text": "Get_Linecol_Col",
                      "href": "docs/grt__rtis_utils___spec.html#L111C13"
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
                      "text": "Linecol",
                      "href": "docs/grt__rtis_utils___spec.html#L111C30"
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
                      "text": "Ghdl_Index_Type",
                      "href": "docs/grt__types___spec.html#L45C9"
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
                      "text": "Ghdl_U32",
                      "href": "docs/grt__types___spec.html#L34C9"
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
              "label": "Linecol",
              "line": 111,
              "column": 30,
              "type": {
                "label": "Grt.Types.Ghdl_Index_Type",
                "docHref": "docs/grt__types___spec.html#L45C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Get_Linecol_Line",
          "qualifier": "",
          "line": 110,
          "column": 13,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 110,
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
                      "text": "Get_Linecol_Line",
                      "href": "docs/grt__rtis_utils___spec.html#L110C13"
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
                      "text": "Linecol",
                      "href": "docs/grt__rtis_utils___spec.html#L110C31"
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
                      "text": "Ghdl_Index_Type",
                      "href": "docs/grt__types___spec.html#L45C9"
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
                      "text": "Ghdl_U32",
                      "href": "docs/grt__types___spec.html#L34C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Extract line and column from a linecol.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Linecol",
              "line": 110,
              "column": 31,
              "type": {
                "label": "Grt.Types.Ghdl_Index_Type",
                "docHref": "docs/grt__types___spec.html#L45C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Get_Path_Name",
          "qualifier": "",
          "line": 101,
          "column": 14,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 101,
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
                      "text": "Get_Path_Name",
                      "href": "docs/grt__rtis_utils___spec.html#L101C14"
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
                      "text": "Rstr",
                      "href": "docs/grt__rtis_utils___spec.html#L101C29"
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
                      "text": "in"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Rstring",
                      "href": "docs/grt__vstrings___spec.html#L88C9"
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
                  "number": 102,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                            "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Last_Ctxt",
                      "href": "docs/grt__rtis_utils___spec.html#L102C29"
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
                      "text": "Rti_Context",
                      "href": "docs/grt__rtis_addr___spec.html#L40C9"
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
                  "number": 103,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                            "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Sep",
                      "href": "docs/grt__rtis_utils___spec.html#L103C29"
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
                      "text": "Character"
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
                  "number": 104,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                            "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Is_Instance",
                      "href": "docs/grt__rtis_utils___spec.html#L104C29"
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
                      "text": "Boolean"
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
                      "text": "True"
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
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Get context as a path name.\n"
                },
                {
                  "kind": "span",
                  "text": "If IS_INSTANCE is true, the architecture name of entities is added.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Rstr",
              "line": 101,
              "column": 29,
              "type": {
                "label": "Grt.Vstrings.Rstring",
                "docHref": "docs/grt__vstrings___spec.html#L62C9"
              },
              "description": [
              ]
            },
            {
              "label": "Last_Ctxt",
              "line": 102,
              "column": 29,
              "type": {
                "label": "Grt.Rtis_Addr.Rti_Context",
                "docHref": "docs/grt__rtis_addr___spec.html#L40C9"
              },
              "description": [
              ]
            },
            {
              "label": "Sep",
              "line": 103,
              "column": 29,
              "type": {
                "label": "Character"
              },
              "description": [
              ]
            },
            {
              "label": "Is_Instance",
              "line": 104,
              "column": 29,
              "type": {
                "label": "Boolean"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Get_Physical_Unit_Name",
          "qualifier": "",
          "line": 86,
          "column": 13,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 86,
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
                      "text": "Get_Physical_Unit_Name",
                      "href": "docs/grt__rtis_utils___spec.html#L86C13"
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
                      "text": "Unit",
                      "href": "docs/grt__rtis_utils___spec.html#L86C37"
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
                      "text": "Ghdl_Rti_Access",
                      "href": "docs/grt__rtis___spec.html#L138C9"
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
                  "number": 87,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                   "
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
                      "text": "Ghdl_C_String",
                      "href": "docs/grt__types___spec.html#L131C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Get the name of a physical unit.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Unit",
              "line": 86,
              "column": 37,
              "type": {
                "label": "Grt.Rtis.Ghdl_Rti_Access",
                "docHref": "docs/grt__rtis___spec.html#L138C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Get_Physical_Unit_Value",
          "qualifier": "",
          "line": 90,
          "column": 13,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 90,
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
                      "text": "Get_Physical_Unit_Value",
                      "href": "docs/grt__rtis_utils___spec.html#L90C13"
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
                      "text": "Unit",
                      "href": "docs/grt__rtis_utils___spec.html#L90C38"
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
                      "text": "Ghdl_Rti_Access",
                      "href": "docs/grt__rtis___spec.html#L138C9"
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
                  "number": 91,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Type_Rti",
                      "href": "docs/grt__rtis_utils___spec.html#L91C38"
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
                      "text": "Ghdl_Rti_Access",
                      "href": "docs/grt__rtis___spec.html#L138C9"
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
                  "number": 92,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                    "
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
                      "text": "Ghdl_I64",
                      "href": "docs/grt__types___spec.html#L37C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Get the value of a physical unit.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Unit",
              "line": 90,
              "column": 38,
              "type": {
                "label": "Grt.Rtis.Ghdl_Rti_Access",
                "docHref": "docs/grt__rtis___spec.html#L138C9"
              },
              "description": [
              ]
            },
            {
              "label": "Type_Rti",
              "line": 91,
              "column": 38,
              "type": {
                "label": "Grt.Rtis.Ghdl_Rti_Access",
                "docHref": "docs/grt__rtis___spec.html#L138C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Get_Value",
          "qualifier": "",
          "line": 81,
          "column": 14,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Get_Value",
                      "href": "docs/grt__rtis_utils___spec.html#L81C14"
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
                      "text": "Str",
                      "href": "docs/grt__rtis_utils___spec.html#L81C25"
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
                      "text": "in"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Vstring",
                      "href": "docs/grt__vstrings___spec.html#L82C9"
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
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/grt__rtis_utils___spec.html#L82C25"
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
                      "text": "Value_Union",
                      "href": "docs/grt__types___spec.html#L207C9"
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
                  "number": 83,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                        "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Type_Rti",
                      "href": "docs/grt__rtis_utils___spec.html#L83C25"
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
                      "text": "Ghdl_Rti_Access",
                      "href": "docs/grt__rtis___spec.html#L138C9"
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
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Str",
              "line": 81,
              "column": 25,
              "type": {
                "label": "Grt.Vstrings.Vstring",
                "docHref": "docs/grt__vstrings___spec.html#L31C9"
              },
              "description": [
              ]
            },
            {
              "label": "Value",
              "line": 82,
              "column": 25,
              "type": {
                "label": "Grt.Types.Value_Union",
                "docHref": "docs/grt__types___spec.html#L207C9"
              },
              "description": [
              ]
            },
            {
              "label": "Type_Rti",
              "line": 83,
              "column": 25,
              "type": {
                "label": "Grt.Rtis.Ghdl_Rti_Access",
                "docHref": "docs/grt__rtis___spec.html#L138C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Is_Unbounded",
          "qualifier": "",
          "line": 79,
          "column": 13,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Is_Unbounded",
                      "href": "docs/grt__rtis_utils___spec.html#L79C13"
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
                      "text": "Rti",
                      "href": "docs/grt__rtis_utils___spec.html#L79C27"
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
                      "text": "Ghdl_Rti_Access",
                      "href": "docs/grt__rtis___spec.html#L138C9"
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
                      "text": "Boolean"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Return True iff RTI is an unbounded type.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Rti",
              "line": 79,
              "column": 27,
              "type": {
                "label": "Grt.Rtis.Ghdl_Rti_Access",
                "docHref": "docs/grt__rtis___spec.html#L138C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Object_To_Base_Bounds",
          "qualifier": "",
          "line": 65,
          "column": 14,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Object_To_Base_Bounds",
                      "href": "docs/grt__rtis_utils___spec.html#L65C14"
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
                      "text": "Obj_Type",
                      "href": "docs/grt__rtis_utils___spec.html#L65C37"
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
                      "text": "Ghdl_Rti_Access",
                      "href": "docs/grt__rtis___spec.html#L138C9"
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
                      "text": "                                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Obj_Loc",
                      "href": "docs/grt__rtis_utils___spec.html#L66C37"
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
                      "text": "Address"
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
                      "text": "                                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Addr",
                      "href": "docs/grt__rtis_utils___spec.html#L67C37"
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
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Address"
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
                      "text": "                                    "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Bounds",
                      "href": "docs/grt__rtis_utils___spec.html#L68C37"
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
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Address"
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
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Convert object address OBJ_LOC (got from RTIs) to ADDR and BOUNDS.\n"
                },
                {
                  "kind": "span",
                  "text": "Deals with complex types and fat pointers.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Obj_Type",
              "line": 65,
              "column": 37,
              "type": {
                "label": "Grt.Rtis.Ghdl_Rti_Access",
                "docHref": "docs/grt__rtis___spec.html#L138C9"
              },
              "description": [
              ]
            },
            {
              "label": "Obj_Loc",
              "line": 66,
              "column": 37,
              "type": {
                "label": "Address"
              },
              "description": [
              ]
            },
            {
              "label": "Addr",
              "line": 67,
              "column": 37,
              "type": {
                "label": "Address"
              },
              "description": [
              ]
            },
            {
              "label": "Bounds",
              "line": 68,
              "column": 37,
              "type": {
                "label": "Address"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Put",
          "qualifier": "",
          "line": 107,
          "column": 14,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 107,
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
                      "text": "Put",
                      "href": "docs/grt__rtis_utils___spec.html#L107C14"
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
                      "text": "Stream",
                      "href": "docs/grt__rtis_utils___spec.html#L107C19"
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
                      "text": "FILEs",
                      "href": "docs/grt__stdio___spec.html#L34C9"
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
                      "text": "Ctxt",
                      "href": "docs/grt__rtis_utils___spec.html#L107C35"
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
                      "text": "Rti_Context",
                      "href": "docs/grt__rtis_addr___spec.html#L40C9"
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
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Disp a context as a path.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Stream",
              "line": 107,
              "column": 19,
              "type": {
                "label": "Grt.Stdio.FILEs",
                "docHref": "docs/grt__stdio___spec.html#L34C9"
              },
              "description": [
              ]
            },
            {
              "label": "Ctxt",
              "line": 107,
              "column": 35,
              "type": {
                "label": "Grt.Rtis_Addr.Rti_Context",
                "docHref": "docs/grt__rtis_addr___spec.html#L40C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Record_To_Element",
          "qualifier": "",
          "line": 71,
          "column": 14,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Record_To_Element",
                      "href": "docs/grt__rtis_utils___spec.html#L71C14"
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
                      "text": "Obj",
                      "href": "docs/grt__rtis_utils___spec.html#L71C33"
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
                      "text": "Address"
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
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "El",
                      "href": "docs/grt__rtis_utils___spec.html#L72C33"
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
                      "text": "Ghdl_Rtin_Element_Acc",
                      "href": "docs/grt__rtis___spec.html#L398C9"
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
                  "number": 73,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Is_Sig",
                      "href": "docs/grt__rtis_utils___spec.html#L73C33"
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
                      "text": "Boolean"
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
                  "number": 74,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Rec_Layout",
                      "href": "docs/grt__rtis_utils___spec.html#L74C33"
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
                      "text": "Address"
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
                      "text": "                                "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "El_Addr",
                      "href": "docs/grt__rtis_utils___spec.html#L75C33"
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
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Address"
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
                      "text": "                                "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "El_Bounds",
                      "href": "docs/grt__rtis_utils___spec.html#L76C33"
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
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Address"
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
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Get address of element EL for record at OBJ.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Obj",
              "line": 71,
              "column": 33,
              "type": {
                "label": "Address"
              },
              "description": [
              ]
            },
            {
              "label": "El",
              "line": 72,
              "column": 33,
              "type": {
                "label": "Grt.Rtis.Ghdl_Rtin_Element_Acc",
                "docHref": "docs/grt__rtis___spec.html#L398C9"
              },
              "description": [
              ]
            },
            {
              "label": "Is_Sig",
              "line": 73,
              "column": 33,
              "type": {
                "label": "Boolean"
              },
              "description": [
              ]
            },
            {
              "label": "Rec_Layout",
              "line": 74,
              "column": 33,
              "type": {
                "label": "Address"
              },
              "description": [
              ]
            },
            {
              "label": "El_Addr",
              "line": 75,
              "column": 33,
              "type": {
                "label": "Address"
              },
              "description": [
              ]
            },
            {
              "label": "El_Bounds",
              "line": 76,
              "column": 33,
              "type": {
                "label": "Address"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Traverse_Blocks",
          "qualifier": "",
          "line": 49,
          "column": 13,
          "src": "srcs/grt-rtis_utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 49,
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
                      "text": "Traverse_Blocks",
                      "href": "docs/grt__rtis_utils___spec.html#L49C13"
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
                      "text": "Ctxt",
                      "href": "docs/grt__rtis_utils___spec.html#L49C30"
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
                      "text": "Rti_Context",
                      "href": "docs/grt__rtis_addr___spec.html#L40C9"
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
                      "text": "Traverse_Result",
                      "href": "docs/grt__rtis_utils___spec.html#L35C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Traverse all blocks (package, entities, architectures, block, generate,\n"
                },
                {
                  "kind": "span",
                  "text": "processes).\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Ctxt",
              "line": 49,
              "column": 30,
              "type": {
                "label": "Grt.Rtis_Addr.Rti_Context",
                "docHref": "docs/grt__rtis_addr___spec.html#L40C9"
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