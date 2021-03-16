GNATdoc.Documentation = {
  "label": "Trans.Chap8",
  "qualifier": "",
  "summary": [
  ],
  "description": [
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "State_Debug",
          "qualifier": "",
          "line": 20,
          "column": 4,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 20,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "State_Debug"
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
                  "text": "If TRUE, generate extra-code to catch at run-time incoherent state\n"
                },
                {
                  "kind": "span",
                  "text": "issues.\n"
                }
              ]
            }
          ]
        },
        {
          "label": "State_Init",
          "qualifier": "",
          "line": 23,
          "column": 4,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 23,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "State_Init"
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
                      "text": "State_Type"
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
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The initial state.  Used in process to loop.\n"
                }
              ]
            }
          ]
        },
        {
          "label": "State_Return",
          "qualifier": "",
          "line": 26,
          "column": 4,
          "src": "srcs/trans-chap8.ads.html",
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
                      "cssClass": "identifier",
                      "text": "State_Return"
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
                      "text": "State_Type"
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
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "The state for 'return' in a subprogram.\n"
                }
              ]
            }
          ]
        }
      ],
      "label": "Constants and variables"
    },
    {
      "entities": [
        {
          "label": "Case_Handler",
          "qualifier": "",
          "line": 68,
          "column": 9,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Case_Handler",
                      "href": "docs/trans__chap8___spec.html#L68C9"
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
                      "text": "tagged"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "null"
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
                      "href": "docs/trans__chap8___spec.html#L68C9"
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
                  "text": "Called to translate the associated node of a choice.\n"
                }
              ]
            }
          ]
        }
      ],
      "label": "Tagged types"
    },
    {
      "entities": [
        {
          "label": "Gen_Update_Iterator",
          "qualifier": "",
          "line": 78,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Gen_Update_Iterator",
                      "href": "docs/trans__chap8___spec.html#L78C14"
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
                      "text": "Iterator",
                      "href": "docs/trans__chap8___spec.html#L78C35"
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
                      "text": "O_Dnode",
                      "href": "docs/ortho_mcode___spec.html#L498C9"
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
                      "text": "                                  "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Dir",
                      "href": "docs/trans__chap8___spec.html#L79C35"
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
                      "cssClass": "identifier",
                      "text": "Direction_Type",
                      "href": "docs/types___spec.html#L187C9"
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
                      "text": "                                  "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Val",
                      "href": "docs/trans__chap8___spec.html#L80C35"
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
                      "cssClass": "identifier",
                      "text": "Unsigned_64"
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
                      "text": "                                  "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Itype",
                      "href": "docs/trans__chap8___spec.html#L81C35"
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
                      "cssClass": "identifier",
                      "text": "Iir",
                      "href": "docs/vhdl__nodes___spec.html#L6945C12"
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
                  "text": "Inc or dec by VAL ITERATOR according to DIR.\n"
                },
                {
                  "kind": "span",
                  "text": "Used for loop statements.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Iterator",
              "line": 78,
              "column": 35,
              "type": {
                "label": "Ortho_Mcode.O_Dnode",
                "docHref": "docs/ortho_mcode___spec.html#L41C9"
              },
              "description": [
              ]
            },
            {
              "label": "Dir",
              "line": 79,
              "column": 35,
              "type": {
                "label": "Types.Direction_Type",
                "docHref": "docs/types___spec.html#L187C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 80,
              "column": 35,
              "type": {
                "label": "Unsigned_64"
              },
              "description": [
              ]
            },
            {
              "label": "Itype",
              "line": 81,
              "column": 35,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "State_Allocate",
          "qualifier": "",
          "line": 43,
          "column": 13,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 43,
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
                      "text": "State_Allocate",
                      "href": "docs/trans__chap8___spec.html#L43C13"
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
                      "text": "State_Type",
                      "href": "docs/trans___spec.html#L975C9"
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
                  "text": "Create a new state.\n"
                }
              ]
            }
          ]
        },
        {
          "label": "State_Enabled",
          "qualifier": "",
          "line": 40,
          "column": 13,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "State_Enabled",
                      "href": "docs/trans__chap8___spec.html#L40C13"
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
                  "text": "True if the current process or subprogram is state based.\n"
                }
              ]
            }
          ]
        },
        {
          "label": "State_Entry",
          "qualifier": "",
          "line": 33,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "State_Entry",
                      "href": "docs/trans__chap8___spec.html#L33C14"
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
                      "text": "Info",
                      "href": "docs/trans__chap8___spec.html#L33C27"
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
                      "text": "Ortho_Info_Acc",
                      "href": "docs/trans___spec.html#L1183C9"
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
                  "text": "Called at the entry of the generated procedure to setup the state\n"
                },
                {
                  "kind": "span",
                  "text": "machinery: set the local state variable, create the state machine\n"
                },
                {
                  "kind": "span",
                  "text": "(loop, case, first choice).  The current position in the graph is\n"
                },
                {
                  "kind": "span",
                  "text": "vertex 0 (initial state): there is an implicit State_Allocate and a\n"
                },
                {
                  "kind": "span",
                  "text": "State_Start.  This is not reentrant (does not nest).\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Info",
              "line": 33,
              "column": 27,
              "type": {
                "label": "Trans.Ortho_Info_Acc",
                "docHref": "docs/trans___spec.html#L1183C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "State_Jump",
          "qualifier": "",
          "line": 51,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "State_Jump",
                      "href": "docs/trans__chap8___spec.html#L51C14"
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
                      "text": "Next_State",
                      "href": "docs/trans__chap8___spec.html#L51C26"
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
                      "text": "State_Type",
                      "href": "docs/trans___spec.html#L975C9"
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
                  "text": "Jump to state NEXT_STATE.  Note: this doesn't modify the control flow,\n"
                },
                {
                  "kind": "span",
                  "text": "so there must be no statements after State_Jump until the next\n"
                },
                {
                  "kind": "span",
                  "text": "State_Start.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Next_State",
              "line": 51,
              "column": 26,
              "type": {
                "label": "Trans.State_Type",
                "docHref": "docs/trans___spec.html#L975C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "State_Leave",
          "qualifier": "",
          "line": 37,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
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
                      "text": "State_Leave",
                      "href": "docs/trans__chap8___spec.html#L37C14"
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
                      "text": "Parent",
                      "href": "docs/trans__chap8___spec.html#L37C27"
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
                  "text": "Last action of the generated procedure: close the case and the loop.\n"
                },
                {
                  "kind": "span",
                  "text": "Destroy the state machinery.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Parent",
              "line": 37,
              "column": 27,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "State_Start",
          "qualifier": "",
          "line": 46,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "State_Start",
                      "href": "docs/trans__chap8___spec.html#L46C14"
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
                      "text": "State",
                      "href": "docs/trans__chap8___spec.html#L46C27"
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
                      "text": "State_Type",
                      "href": "docs/trans___spec.html#L975C9"
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
                  "text": "Start statements for STATE.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "State",
              "line": 46,
              "column": 27,
              "type": {
                "label": "Trans.State_Type",
                "docHref": "docs/trans___spec.html#L975C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "State_Suspend",
          "qualifier": "",
          "line": 55,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "State_Suspend",
                      "href": "docs/trans__chap8___spec.html#L55C14"
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
                      "text": "Next_State",
                      "href": "docs/trans__chap8___spec.html#L55C29"
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
                      "text": "State_Type",
                      "href": "docs/trans___spec.html#L975C9"
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
                  "text": "Suspend the current process or subprogram.  It will resume to\n"
                },
                {
                  "kind": "span",
                  "text": "NEXT_STATE.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Next_State",
              "line": 55,
              "column": 29,
              "type": {
                "label": "Trans.State_Type",
                "docHref": "docs/trans___spec.html#L975C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Translate_Case",
          "qualifier": "",
          "line": 74,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Translate_Case",
                      "href": "docs/trans__chap8___spec.html#L74C14"
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
                      "text": "N",
                      "href": "docs/trans__chap8___spec.html#L74C30"
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
                      "text": "Handler",
                      "href": "docs/trans__chap8___spec.html#L74C39"
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
                      "text": "Case_Handler",
                      "href": "docs/trans__chap8___spec.html#L68C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "'"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Class"
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
                  "text": "Translate a case statement or a selected signal assignment.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "N",
              "line": 74,
              "column": 30,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Handler",
              "line": 74,
              "column": 39,
              "type": {
                "label": "Trans.Chap8.Case_Handler",
                "docHref": "docs/trans__chap8___spec.html#L68C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Translate_Case_Choice",
          "qualifier": "",
          "line": 64,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Translate_Case_Choice",
                      "href": "docs/trans__chap8___spec.html#L64C14"
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
                      "text": "Choice",
                      "href": "docs/trans__chap8___spec.html#L65C7"
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
                      "text": "Choice_Type",
                      "href": "docs/trans__chap8___spec.html#L65C21"
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
                      "text": "Blk",
                      "href": "docs/trans__chap8___spec.html#L65C40"
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
                      "text": "O_Case_Block",
                      "href": "docs/ortho_mcode___spec.html#L522C9"
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
                  "text": "Create a case branch for CHOICE.\n"
                },
                {
                  "kind": "span",
                  "text": "Used by case statement and aggregates.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Choice",
              "line": 65,
              "column": 7,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Choice_Type",
              "line": 65,
              "column": 21,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Blk",
              "line": 65,
              "column": 40,
              "type": {
                "label": "Ortho_Mcode.O_Case_Block",
                "docHref": "docs/ortho_mcode___spec.html#L304C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Translate_For_Loop_Statement_Declaration",
          "qualifier": "",
          "line": 84,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 84,
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
                      "text": "Translate_For_Loop_Statement_Declaration",
                      "href": "docs/trans__chap8___spec.html#L84C14"
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
                      "text": "Stmt",
                      "href": "docs/trans__chap8___spec.html#L84C56"
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
                  "text": "Create declarations for a for-loop statement.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Stmt",
              "line": 84,
              "column": 56,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Translate_Procedure_Call_State",
          "qualifier": "",
          "line": 89,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 89,
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
                      "text": "Translate_Procedure_Call_State",
                      "href": "docs/trans__chap8___spec.html#L89C14"
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
                      "text": "Call",
                      "href": "docs/trans__chap8___spec.html#L89C46"
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
                  "text": "Create the state record for the CALL procedure call.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Call",
              "line": 89,
              "column": 46,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Translate_Report",
          "qualifier": "",
          "line": 86,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
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
                      "text": "Translate_Report",
                      "href": "docs/trans__chap8___spec.html#L86C14"
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
                      "text": "Stmt",
                      "href": "docs/trans__chap8___spec.html#L86C32"
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
                      "text": "Subprg",
                      "href": "docs/trans__chap8___spec.html#L86C44"
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
                      "text": "O_Dnode",
                      "href": "docs/ortho_mcode___spec.html#L498C9"
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
                      "text": "Level",
                      "href": "docs/trans__chap8___spec.html#L86C62"
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
              "label": "Stmt",
              "line": 86,
              "column": 32,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Subprg",
              "line": 86,
              "column": 44,
              "type": {
                "label": "Ortho_Mcode.O_Dnode",
                "docHref": "docs/ortho_mcode___spec.html#L41C9"
              },
              "description": [
              ]
            },
            {
              "label": "Level",
              "line": 86,
              "column": 62,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Translate_Statements_Chain",
          "qualifier": "",
          "line": 57,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
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
                      "text": "Translate_Statements_Chain",
                      "href": "docs/trans__chap8___spec.html#L57C14"
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
                      "text": "First",
                      "href": "docs/trans__chap8___spec.html#L57C42"
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
              "label": "First",
              "line": 57,
              "column": 42,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Translate_Statements_Chain_Has_Return",
          "qualifier": "",
          "line": 60,
          "column": 13,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Translate_Statements_Chain_Has_Return",
                      "href": "docs/trans__chap8___spec.html#L60C13"
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
                      "text": "First",
                      "href": "docs/trans__chap8___spec.html#L60C52"
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
                  "text": "Return true if there is a return statement in the chain.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "First",
              "line": 60,
              "column": 52,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Translate_Subprogram_Call",
          "qualifier": "",
          "line": 91,
          "column": 13,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 91,
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
                      "text": "Translate_Subprogram_Call",
                      "href": "docs/trans__chap8___spec.html#L91C13"
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
                      "text": "Call",
                      "href": "docs/trans__chap8___spec.html#L92C7"
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
                      "text": "Assoc_Chain",
                      "href": "docs/trans__chap8___spec.html#L92C19"
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
                      "text": "Obj",
                      "href": "docs/trans__chap8___spec.html#L92C38"
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
                      "text": "O_Enode",
                      "href": "docs/ortho_mcode___spec.html#L499C9"
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
              "label": "Call",
              "line": 92,
              "column": 7,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Assoc_Chain",
              "line": 92,
              "column": 19,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Obj",
              "line": 92,
              "column": 38,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Subprograms"
    },
    {
      "entities": [
        {
          "label": "Case_Association_Cb",
          "qualifier": "",
          "line": 70,
          "column": 14,
          "src": "srcs/trans-chap8.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Case_Association_Cb",
                      "href": "docs/trans__chap8___spec.html#L70C14"
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
                      "text": "Assoc",
                      "href": "docs/trans__chap8___spec.html#L70C35"
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
                      "text": "                                  "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Handler",
                      "href": "docs/trans__chap8___spec.html#L71C35"
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
                      "text": "Case_Handler",
                      "href": "docs/trans__chap8___spec.html#L68C9"
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
                      "text": "null"
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
              "label": "Assoc",
              "line": 70,
              "column": 35,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Handler",
              "line": 71,
              "column": 35,
              "type": {
                "label": "Trans.Chap8.Case_Handler",
                "docHref": "docs/trans__chap8___spec.html#L68C9"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Dispatching subprograms"
    }
  ]
};