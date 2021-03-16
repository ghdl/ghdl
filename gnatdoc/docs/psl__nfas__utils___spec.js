GNATdoc.Documentation = {
  "label": "PSL.NFAs.Utils",
  "qualifier": "",
  "summary": [
  ],
  "description": [
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Check_NFA",
          "qualifier": "",
          "line": 39,
          "column": 14,
          "src": "srcs/psl-nfas-utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Check_NFA",
                      "href": "docs/psl__nfas__utils___spec.html#L39C14"
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
                      "href": "docs/psl__nfas__utils___spec.html#L39C25"
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
                      "text": "NFA",
                      "href": "docs/psl__nodes___spec.html#L458C12"
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
                  "text": "Raise Program_Error if N is not internally coherent.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "N",
              "line": 39,
              "column": 25,
              "type": {
                "label": "PSL.Nodes.NFA",
                "docHref": "docs/psl__nodes___spec.html#L458C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Has_EOS",
          "qualifier": "",
          "line": 32,
          "column": 13,
          "src": "srcs/psl-nfas-utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Has_EOS",
                      "href": "docs/psl__nfas__utils___spec.html#L32C13"
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
                      "href": "docs/psl__nfas__utils___spec.html#L32C22"
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
                      "href": "docs/psl__nodes___spec.html#L450C12"
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
                  "text": "Return True if N or a child of N is EOS.\n"
                },
                {
                  "kind": "span",
                  "text": "N must be a boolean expression.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "N",
              "line": 32,
              "column": 22,
              "type": {
                "label": "PSL.Nodes.Node",
                "docHref": "docs/psl__nodes___spec.html#L450C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Merge_State_Dest",
          "qualifier": "",
          "line": 26,
          "column": 14,
          "src": "srcs/psl-nfas-utils.ads.html",
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
                      "text": "Merge_State_Dest",
                      "href": "docs/psl__nfas__utils___spec.html#L26C14"
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
                      "href": "docs/psl__nfas__utils___spec.html#L26C32"
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
                      "text": "NFA",
                      "href": "docs/psl__nodes___spec.html#L458C12"
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
                      "text": "S",
                      "href": "docs/psl__nfas__utils___spec.html#L26C41"
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
                      "text": "NFA_State",
                      "href": "docs/psl__nfas___spec.html#L28C9"
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
                      "text": "S1",
                      "href": "docs/psl__nfas__utils___spec.html#L26C56"
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
                      "text": "NFA_State",
                      "href": "docs/psl__nfas___spec.html#L28C9"
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
                  "text": "Move incoming edges of S1 to S, remove S1 and its outgoing edges.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "N",
              "line": 26,
              "column": 32,
              "type": {
                "label": "PSL.Nodes.NFA",
                "docHref": "docs/psl__nodes___spec.html#L458C12"
              },
              "description": [
              ]
            },
            {
              "label": "S",
              "line": 26,
              "column": 41,
              "type": {
                "label": "PSL.NFAs.NFA_State",
                "docHref": "docs/psl__nfas___spec.html#L28C9"
              },
              "description": [
              ]
            },
            {
              "label": "S1",
              "line": 26,
              "column": 56,
              "type": {
                "label": "PSL.NFAs.NFA_State",
                "docHref": "docs/psl__nfas___spec.html#L28C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Merge_State_Src",
          "qualifier": "",
          "line": 28,
          "column": 14,
          "src": "srcs/psl-nfas-utils.ads.html",
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
                      "text": "Merge_State_Src",
                      "href": "docs/psl__nfas__utils___spec.html#L28C14"
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
                      "href": "docs/psl__nfas__utils___spec.html#L28C31"
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
                      "text": "NFA",
                      "href": "docs/psl__nodes___spec.html#L458C12"
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
                      "text": "S",
                      "href": "docs/psl__nfas__utils___spec.html#L28C40"
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
                      "text": "NFA_State",
                      "href": "docs/psl__nfas___spec.html#L28C9"
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
                      "text": "S1",
                      "href": "docs/psl__nfas__utils___spec.html#L28C55"
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
                      "text": "NFA_State",
                      "href": "docs/psl__nfas___spec.html#L28C9"
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
              "label": "N",
              "line": 28,
              "column": 31,
              "type": {
                "label": "PSL.Nodes.NFA",
                "docHref": "docs/psl__nodes___spec.html#L458C12"
              },
              "description": [
              ]
            },
            {
              "label": "S",
              "line": 28,
              "column": 40,
              "type": {
                "label": "PSL.NFAs.NFA_State",
                "docHref": "docs/psl__nfas___spec.html#L28C9"
              },
              "description": [
              ]
            },
            {
              "label": "S1",
              "line": 28,
              "column": 55,
              "type": {
                "label": "PSL.NFAs.NFA_State",
                "docHref": "docs/psl__nfas___spec.html#L28C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Set_Init_Loop",
          "qualifier": "",
          "line": 36,
          "column": 14,
          "src": "srcs/psl-nfas-utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Set_Init_Loop",
                      "href": "docs/psl__nfas__utils___spec.html#L36C14"
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
                      "href": "docs/psl__nfas__utils___spec.html#L36C29"
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
                      "text": "NFA",
                      "href": "docs/psl__nodes___spec.html#L458C12"
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
                  "text": "Ensure there is an edge from init state to itself, for the implicit\n"
                },
                {
                  "kind": "span",
                  "text": "[*] of cover directive.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "N",
              "line": 36,
              "column": 29,
              "type": {
                "label": "PSL.Nodes.NFA",
                "docHref": "docs/psl__nodes___spec.html#L458C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Sort_Dest_Edges",
          "qualifier": "",
          "line": 22,
          "column": 14,
          "src": "srcs/psl-nfas-utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 22,
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
                      "text": "Sort_Dest_Edges",
                      "href": "docs/psl__nfas__utils___spec.html#L22C14"
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
                      "text": "S",
                      "href": "docs/psl__nfas__utils___spec.html#L22C31"
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
                      "text": "NFA_State",
                      "href": "docs/psl__nfas___spec.html#L28C9"
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
              "label": "S",
              "line": 22,
              "column": 31,
              "type": {
                "label": "PSL.NFAs.NFA_State",
                "docHref": "docs/psl__nfas___spec.html#L28C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Sort_Dest_Edges",
          "qualifier": "",
          "line": 23,
          "column": 14,
          "src": "srcs/psl-nfas-utils.ads.html",
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
                      "text": "Sort_Dest_Edges",
                      "href": "docs/psl__nfas__utils___spec.html#L23C14"
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
                      "href": "docs/psl__nfas__utils___spec.html#L23C31"
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
                      "text": "NFA",
                      "href": "docs/psl__nodes___spec.html#L458C12"
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
              "label": "N",
              "line": 23,
              "column": 31,
              "type": {
                "label": "PSL.Nodes.NFA",
                "docHref": "docs/psl__nodes___spec.html#L458C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Sort_Src_Edges",
          "qualifier": "",
          "line": 19,
          "column": 14,
          "src": "srcs/psl-nfas-utils.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 19,
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
                      "text": "Sort_Src_Edges",
                      "href": "docs/psl__nfas__utils___spec.html#L19C14"
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
                      "text": "S",
                      "href": "docs/psl__nfas__utils___spec.html#L19C30"
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
                      "text": "NFA_State",
                      "href": "docs/psl__nfas___spec.html#L28C9"
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
                  "text": "Sort outgoing edges by expression.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "S",
              "line": 19,
              "column": 30,
              "type": {
                "label": "PSL.NFAs.NFA_State",
                "docHref": "docs/psl__nfas___spec.html#L28C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Sort_Src_Edges",
          "qualifier": "",
          "line": 20,
          "column": 14,
          "src": "srcs/psl-nfas-utils.ads.html",
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
                      "text": "Sort_Src_Edges",
                      "href": "docs/psl__nfas__utils___spec.html#L20C14"
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
                      "href": "docs/psl__nfas__utils___spec.html#L20C30"
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
                      "text": "NFA",
                      "href": "docs/psl__nodes___spec.html#L458C12"
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
              "label": "N",
              "line": 20,
              "column": 30,
              "type": {
                "label": "PSL.Nodes.NFA",
                "docHref": "docs/psl__nodes___spec.html#L458C12"
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