GNATdoc.Documentation = {
  "label": "Vhdl.Sem_Types",
  "qualifier": "",
  "summary": [
  ],
  "description": [
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "Analyze of types (LRM93 3 / LRM08 5)\n"
        }
      ]
    }
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Build_Constrained_Subtype",
          "qualifier": "",
          "line": 59,
          "column": 13,
          "src": "srcs/vhdl-sem_types.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Build_Constrained_Subtype",
                      "href": "docs/vhdl__sem_types___spec.html#L59C13"
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
                      "text": "Atype",
                      "href": "docs/vhdl__sem_types___spec.html#L59C40"
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
                      "text": "Loc",
                      "href": "docs/vhdl__sem_types___spec.html#L59C53"
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
                      "text": "Iir",
                      "href": "docs/vhdl__nodes___spec.html#L6945C12"
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
                  "text": "If ATYPE is not fully constrained, build a fully constrained subtype.\n"
                },
                {
                  "kind": "span",
                  "text": "This is for 'subtype attribute.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Atype",
              "line": 59,
              "column": 40,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Loc",
              "line": 59,
              "column": 53,
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
          "label": "Copy_Resolution_Indication",
          "qualifier": "",
          "line": 55,
          "column": 13,
          "src": "srcs/vhdl-sem_types.ads.html",
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
                      "text": "Copy_Resolution_Indication",
                      "href": "docs/vhdl__sem_types___spec.html#L55C13"
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
                      "text": "Subdef",
                      "href": "docs/vhdl__sem_types___spec.html#L55C41"
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
                      "text": "Iir",
                      "href": "docs/vhdl__nodes___spec.html#L6945C12"
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
                  "text": "Return a copy of the resolution_indication in SUBDEF, or null_iir if\n"
                },
                {
                  "kind": "span",
                  "text": "none.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Subdef",
              "line": 55,
              "column": 41,
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
          "label": "Copy_Subtype_Indication",
          "qualifier": "",
          "line": 51,
          "column": 13,
          "src": "srcs/vhdl-sem_types.ads.html",
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
                      "text": "Copy_Subtype_Indication",
                      "href": "docs/vhdl__sem_types___spec.html#L51C13"
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
                      "text": "Def",
                      "href": "docs/vhdl__sem_types___spec.html#L51C38"
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
                      "text": "Iir",
                      "href": "docs/vhdl__nodes___spec.html#L6945C12"
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
                  "text": "Return a subtype definition copy of DEF.\n"
                },
                {
                  "kind": "span",
                  "text": "This is used when an alias of DEF is required (eg: subtype a is b).\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Def",
              "line": 51,
              "column": 38,
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
          "label": "Get_Nature_Simple_Nature",
          "qualifier": "",
          "line": 88,
          "column": 13,
          "src": "srcs/vhdl-sem_types.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 88,
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
                      "text": "Get_Nature_Simple_Nature",
                      "href": "docs/vhdl__sem_types___spec.html#L88C13"
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
                      "text": "Nat",
                      "href": "docs/vhdl__sem_types___spec.html#L88C39"
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
                      "text": "Iir",
                      "href": "docs/vhdl__nodes___spec.html#L6945C12"
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
                  "text": "Return the simple nature of NAT.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "AMS-LRM17 5.8.2 Scalar natures\n"
                },
                {
                  "kind": "span",
                  "text": "The simple nature of a scalar nature is the nature itself.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "AMS-LRM17 5.8.3 Composite natures\n"
                },
                {
                  "kind": "span",
                  "text": "The scalar subelements of a composite nature shall have the same simple\n"
                },
                {
                  "kind": "span",
                  "text": "nature, which is also the simple nature of the composite nature.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Nat",
              "line": 88,
              "column": 39,
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
          "label": "Is_A_Resolution_Function",
          "qualifier": "",
          "line": 47,
          "column": 13,
          "src": "srcs/vhdl-sem_types.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Is_A_Resolution_Function",
                      "href": "docs/vhdl__sem_types___spec.html#L47C13"
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
                      "text": "Func",
                      "href": "docs/vhdl__sem_types___spec.html#L47C39"
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
                      "text": "Atype",
                      "href": "docs/vhdl__sem_types___spec.html#L47C50"
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
                  "text": "Return TRUE iff FUNC is a resolution function.\n"
                },
                {
                  "kind": "span",
                  "text": "If ATYPE is not NULL_IIR, type must match.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Func",
              "line": 47,
              "column": 39,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Atype",
              "line": 47,
              "column": 50,
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
          "label": "Is_Composite_Nature",
          "qualifier": "",
          "line": 91,
          "column": 13,
          "src": "srcs/vhdl-sem_types.ads.html",
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
                      "text": "Is_Composite_Nature",
                      "href": "docs/vhdl__sem_types___spec.html#L91C13"
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
                      "text": "Nat",
                      "href": "docs/vhdl__sem_types___spec.html#L91C34"
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
                  "text": "Return TRUE iff nature NAT is a composite nature.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Nat",
              "line": 91,
              "column": 34,
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
          "label": "Is_Nature_Type",
          "qualifier": "",
          "line": 78,
          "column": 13,
          "src": "srcs/vhdl-sem_types.ads.html",
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
                      "text": "Is_Nature_Type",
                      "href": "docs/vhdl__sem_types___spec.html#L78C13"
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
                      "text": "Dtype",
                      "href": "docs/vhdl__sem_types___spec.html#L78C29"
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
                  "text": "AMS-LRM17 6.4.2.7 Quantity declarations\n"
                },
                {
                  "kind": "span",
                  "text": "A nature type is a floating-point type of a composite type whose\n"
                },
                {
                  "kind": "span",
                  "text": "elements are of a nature type.\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Return true iff DTYPE is a nature type.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Dtype",
              "line": 78,
              "column": 29,
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
          "label": "Range_To_Subtype_Indication",
          "qualifier": "",
          "line": 36,
          "column": 13,
          "src": "srcs/vhdl-sem_types.ads.html",
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
                      "text": "Range_To_Subtype_Indication",
                      "href": "docs/vhdl__sem_types___spec.html#L36C13"
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
                      "text": "A_Range",
                      "href": "docs/vhdl__sem_types___spec.html#L36C42"
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
                      "text": "Iir",
                      "href": "docs/vhdl__nodes___spec.html#L6945C12"
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
                  "text": "If A_RANGE is a range (range expression or range attribute), convert it\n"
                },
                {
                  "kind": "span",
                  "text": "to a subtype definition.  Otherwise return A_RANGE.\n"
                },
                {
                  "kind": "span",
                  "text": "The result is a subtype indication: either a type name or a subtype\n"
                },
                {
                  "kind": "span",
                  "text": "definition.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "A_Range",
              "line": 36,
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
          "label": "Sem_Nature_Definition",
          "qualifier": "",
          "line": 71,
          "column": 13,
          "src": "srcs/vhdl-sem_types.ads.html",
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
                      "text": "Sem_Nature_Definition",
                      "href": "docs/vhdl__sem_types___spec.html#L71C13"
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
                      "text": "Def",
                      "href": "docs/vhdl__sem_types___spec.html#L71C36"
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
                      "text": "Decl",
                      "href": "docs/vhdl__sem_types___spec.html#L71C47"
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
                      "text": "Iir",
                      "href": "docs/vhdl__nodes___spec.html#L6945C12"
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
              "label": "Def",
              "line": 71,
              "column": 36,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Decl",
              "line": 71,
              "column": 47,
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
          "label": "Sem_Protected_Type_Body",
          "qualifier": "",
          "line": 28,
          "column": 14,
          "src": "srcs/vhdl-sem_types.ads.html",
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
                      "text": "Sem_Protected_Type_Body",
                      "href": "docs/vhdl__sem_types___spec.html#L28C14"
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
                      "text": "Bod",
                      "href": "docs/vhdl__sem_types___spec.html#L28C39"
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
              "label": "Bod",
              "line": 28,
              "column": 39,
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
          "label": "Sem_Subnature_Indication",
          "qualifier": "",
          "line": 69,
          "column": 13,
          "src": "srcs/vhdl-sem_types.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Sem_Subnature_Indication",
                      "href": "docs/vhdl__sem_types___spec.html#L69C13"
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
                      "text": "Def",
                      "href": "docs/vhdl__sem_types___spec.html#L69C39"
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
                      "text": "Iir",
                      "href": "docs/vhdl__nodes___spec.html#L6945C12"
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
                  "text": "Although a nature is not a type, it is patterned like a type.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Def",
              "line": 69,
              "column": 39,
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
          "label": "Sem_Subtype_Indication",
          "qualifier": "",
          "line": 25,
          "column": 13,
          "src": "srcs/vhdl-sem_types.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 25,
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
                      "text": "Sem_Subtype_Indication",
                      "href": "docs/vhdl__sem_types___spec.html#L25C13"
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
                      "text": "Def",
                      "href": "docs/vhdl__sem_types___spec.html#L25C37"
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
                      "text": "Incomplete",
                      "href": "docs/vhdl__sem_types___spec.html#L25C47"
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
                      "text": "False"
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
                  "number": 26,
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
                      "text": "Iir",
                      "href": "docs/vhdl__nodes___spec.html#L6945C12"
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
                  "text": "Analyze subtype indication DEF.\n"
                },
                {
                  "kind": "span",
                  "text": "If INCOMPLETE is TRUE, then DEF may designate an incomplete type\n"
                },
                {
                  "kind": "span",
                  "text": "definition.  Return either a name (denoting a type), an anonymous\n"
                },
                {
                  "kind": "span",
                  "text": "subtype definition or a name whose type is an error node.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Def",
              "line": 25,
              "column": 37,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Incomplete",
              "line": 25,
              "column": 47,
              "type": {
                "label": "Boolean"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Sem_Type_Definition",
          "qualifier": "",
          "line": 30,
          "column": 13,
          "src": "srcs/vhdl-sem_types.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Sem_Type_Definition",
                      "href": "docs/vhdl__sem_types___spec.html#L30C13"
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
                      "text": "Def",
                      "href": "docs/vhdl__sem_types___spec.html#L30C34"
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
                      "text": "Decl",
                      "href": "docs/vhdl__sem_types___spec.html#L30C44"
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
                      "text": "Iir",
                      "href": "docs/vhdl__nodes___spec.html#L6945C12"
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
              "label": "Def",
              "line": 30,
              "column": 34,
              "type": {
                "label": "Vhdl.Nodes.Iir",
                "docHref": "docs/vhdl__nodes___spec.html#L6945C12"
              },
              "description": [
              ]
            },
            {
              "label": "Decl",
              "line": 30,
              "column": 44,
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
          "label": "Set_Type_Has_Signal",
          "qualifier": "",
          "line": 43,
          "column": 14,
          "src": "srcs/vhdl-sem_types.ads.html",
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
                      "text": "Set_Type_Has_Signal",
                      "href": "docs/vhdl__sem_types___spec.html#L43C14"
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
                      "text": "Atype",
                      "href": "docs/vhdl__sem_types___spec.html#L43C35"
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
                  "text": "ATYPE is used to declare a signal.\n"
                },
                {
                  "kind": "span",
                  "text": "Set (recursively) the Has_Signal_Flag on ATYPE and all types used by\n"
                },
                {
                  "kind": "span",
                  "text": "ATYPE (basetype, elements...)\n"
                },
                {
                  "kind": "span",
                  "text": "If ATYPE can have signal (eg: access or file type), then this procedure\n"
                },
                {
                  "kind": "span",
                  "text": "returns silently.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Atype",
              "line": 43,
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
          "label": "Update_Record_Constraint",
          "qualifier": "",
          "line": 64,
          "column": 14,
          "src": "srcs/vhdl-sem_types.ads.html",
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
                      "text": "Update_Record_Constraint",
                      "href": "docs/vhdl__sem_types___spec.html#L64C14"
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
                      "text": "Constraint",
                      "href": "docs/vhdl__sem_types___spec.html#L64C40"
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
                      "text": "Iir_Constraint",
                      "href": "docs/vhdl__nodes___spec.html#L6304C9"
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
                      "text": "                                       "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Composite_Found",
                      "href": "docs/vhdl__sem_types___spec.html#L65C40"
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
                  "number": 66,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "                                       "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "El_Type",
                      "href": "docs/vhdl__sem_types___spec.html#L66C40"
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
                  "text": "Adjust the constraint state CONSTRAINT given new element EL_TYPE.\n"
                },
                {
                  "kind": "span",
                  "text": "Initially CONSTRAINT must be Fully_Constrained and COMPOSITE_FOUND\n"
                },
                {
                  "kind": "span",
                  "text": "must be false.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Constraint",
              "line": 64,
              "column": 40,
              "type": {
                "label": "Vhdl.Nodes.Iir_Constraint",
                "docHref": "docs/vhdl__nodes___spec.html#L6304C9"
              },
              "description": [
              ]
            },
            {
              "label": "Composite_Found",
              "line": 65,
              "column": 40,
              "type": {
                "label": "Boolean"
              },
              "description": [
              ]
            },
            {
              "label": "El_Type",
              "line": 66,
              "column": 40,
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
    }
  ]
};