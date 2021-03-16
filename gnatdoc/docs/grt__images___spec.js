GNATdoc.Documentation = {
  "label": "Grt.Images",
  "qualifier": "",
  "summary": [
  ],
  "description": [
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "For all images procedures, the result is allocated on the secondary\n"
        },
        {
          "kind": "span",
          "text": "stack.\n"
        }
      ]
    }
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Ghdl_Array_Char_To_String_B1",
          "qualifier": "",
          "line": 66,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Ghdl_Array_Char_To_String_B1",
                      "href": "docs/grt__images___spec.html#L66C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L67C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L68C7"
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
                      "text": "Ghdl_Ptr",
                      "href": "docs/grt__types___spec.html#L44C9"
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
                      "href": "docs/grt__images___spec.html#L68C23"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L68C46"
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
              "label": "Res",
              "line": 67,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 68,
              "column": 7,
              "type": {
                "label": "Grt.Types.Ghdl_Ptr",
                "docHref": "docs/grt__types___spec.html#L44C9"
              },
              "description": [
              ]
            },
            {
              "label": "Len",
              "line": 68,
              "column": 23,
              "type": {
                "label": "Grt.Types.Ghdl_Index_Type",
                "docHref": "docs/grt__types___spec.html#L45C9"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 68,
              "column": 46,
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
          "label": "Ghdl_Array_Char_To_String_E32",
          "qualifier": "",
          "line": 72,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 72,
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
                      "text": "Ghdl_Array_Char_To_String_E32",
                      "href": "docs/grt__images___spec.html#L72C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L73C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L74C7"
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
                      "text": "Ghdl_Ptr",
                      "href": "docs/grt__types___spec.html#L44C9"
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
                      "href": "docs/grt__images___spec.html#L74C23"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L74C46"
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
              "label": "Res",
              "line": 73,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 74,
              "column": 7,
              "type": {
                "label": "Grt.Types.Ghdl_Ptr",
                "docHref": "docs/grt__types___spec.html#L44C9"
              },
              "description": [
              ]
            },
            {
              "label": "Len",
              "line": 74,
              "column": 23,
              "type": {
                "label": "Grt.Types.Ghdl_Index_Type",
                "docHref": "docs/grt__types___spec.html#L45C9"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 74,
              "column": 46,
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
          "label": "Ghdl_Array_Char_To_String_E8",
          "qualifier": "",
          "line": 69,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_Array_Char_To_String_E8",
                      "href": "docs/grt__images___spec.html#L69C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L70C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L71C7"
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
                      "text": "Ghdl_Ptr",
                      "href": "docs/grt__types___spec.html#L44C9"
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
                      "href": "docs/grt__images___spec.html#L71C23"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L71C46"
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
              "label": "Res",
              "line": 70,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 71,
              "column": 7,
              "type": {
                "label": "Grt.Types.Ghdl_Ptr",
                "docHref": "docs/grt__types___spec.html#L44C9"
              },
              "description": [
              ]
            },
            {
              "label": "Len",
              "line": 71,
              "column": 23,
              "type": {
                "label": "Grt.Types.Ghdl_Index_Type",
                "docHref": "docs/grt__types___spec.html#L45C9"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 71,
              "column": 46,
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
          "label": "Ghdl_BV_To_Hstring",
          "qualifier": "",
          "line": 79,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_BV_To_Hstring",
                      "href": "docs/grt__images___spec.html#L79C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L79C34"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "                                 "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Base",
                      "href": "docs/grt__images___spec.html#L80C34"
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
                      "text": "Std_Bit_Vector_Basep",
                      "href": "docs/grt__types___spec.html#L101C9"
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
                      "text": "                                 "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Len",
                      "href": "docs/grt__images___spec.html#L81C34"
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
              "label": "Res",
              "line": 79,
              "column": 34,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Base",
              "line": 80,
              "column": 34,
              "type": {
                "label": "Grt.Types.Std_Bit_Vector_Basep",
                "docHref": "docs/grt__types___spec.html#L101C9"
              },
              "description": [
              ]
            },
            {
              "label": "Len",
              "line": 81,
              "column": 34,
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
          "label": "Ghdl_BV_To_Ostring",
          "qualifier": "",
          "line": 76,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Ghdl_BV_To_Ostring",
                      "href": "docs/grt__images___spec.html#L76C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L76C34"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "                                 "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Base",
                      "href": "docs/grt__images___spec.html#L77C34"
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
                      "text": "Std_Bit_Vector_Basep",
                      "href": "docs/grt__types___spec.html#L101C9"
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
                      "text": "                                 "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Len",
                      "href": "docs/grt__images___spec.html#L78C34"
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
              "label": "Res",
              "line": 76,
              "column": 34,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Base",
              "line": 77,
              "column": 34,
              "type": {
                "label": "Grt.Types.Std_Bit_Vector_Basep",
                "docHref": "docs/grt__types___spec.html#L101C9"
              },
              "description": [
              ]
            },
            {
              "label": "Len",
              "line": 78,
              "column": 34,
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
          "label": "Ghdl_Image_B1",
          "qualifier": "",
          "line": 30,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_Image_B1",
                      "href": "docs/grt__images___spec.html#L30C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L31C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L31C29"
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
                      "text": "Ghdl_B1",
                      "href": "docs/grt__types___spec.html#L31C9"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L31C44"
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
              "label": "Res",
              "line": 31,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 31,
              "column": 29,
              "type": {
                "label": "Grt.Types.Ghdl_B1",
                "docHref": "docs/grt__types___spec.html#L31C9"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 31,
              "column": 44,
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
          "label": "Ghdl_Image_E32",
          "qualifier": "",
          "line": 34,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Ghdl_Image_E32",
                      "href": "docs/grt__images___spec.html#L34C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L35C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L35C29"
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
                      "text": "Ghdl_E32",
                      "href": "docs/grt__types___spec.html#L35C12"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L35C45"
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
              "label": "Res",
              "line": 35,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 35,
              "column": 29,
              "type": {
                "label": "Grt.Types.Ghdl_E32",
                "docHref": "docs/grt__types___spec.html#L35C12"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 35,
              "column": 45,
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
          "label": "Ghdl_Image_E8",
          "qualifier": "",
          "line": 32,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_Image_E8",
                      "href": "docs/grt__images___spec.html#L32C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L33C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L33C29"
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
                      "text": "Ghdl_E8",
                      "href": "docs/grt__types___spec.html#L33C12"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L33C44"
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
              "label": "Res",
              "line": 33,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 33,
              "column": 29,
              "type": {
                "label": "Grt.Types.Ghdl_E8",
                "docHref": "docs/grt__types___spec.html#L33C12"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 33,
              "column": 44,
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
          "label": "Ghdl_Image_F64",
          "qualifier": "",
          "line": 38,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_Image_F64",
                      "href": "docs/grt__images___spec.html#L38C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L38C30"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L38C52"
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
                      "text": "Ghdl_F64",
                      "href": "docs/grt__types___spec.html#L39C9"
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
              "label": "Res",
              "line": 38,
              "column": 30,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 38,
              "column": 52,
              "type": {
                "label": "Grt.Types.Ghdl_F64",
                "docHref": "docs/grt__types___spec.html#L39C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Ghdl_Image_I32",
          "qualifier": "",
          "line": 36,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_Image_I32",
                      "href": "docs/grt__images___spec.html#L36C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L36C30"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L36C52"
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
                      "text": "Ghdl_I32",
                      "href": "docs/grt__types___spec.html#L36C9"
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
              "label": "Res",
              "line": 36,
              "column": 30,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 36,
              "column": 52,
              "type": {
                "label": "Grt.Types.Ghdl_I32",
                "docHref": "docs/grt__types___spec.html#L36C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Ghdl_Image_I64",
          "qualifier": "",
          "line": 37,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_Image_I64",
                      "href": "docs/grt__images___spec.html#L37C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L37C30"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L37C52"
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
                      "text": "Ghdl_I64",
                      "href": "docs/grt__types___spec.html#L37C9"
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
              "label": "Res",
              "line": 37,
              "column": 30,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 37,
              "column": 52,
              "type": {
                "label": "Grt.Types.Ghdl_I64",
                "docHref": "docs/grt__types___spec.html#L37C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Ghdl_Image_P32",
          "qualifier": "",
          "line": 41,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Ghdl_Image_P32",
                      "href": "docs/grt__images___spec.html#L41C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L42C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L42C29"
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
                      "text": "Ghdl_I32",
                      "href": "docs/grt__types___spec.html#L36C9"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L42C45"
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
              "label": "Res",
              "line": 42,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 42,
              "column": 29,
              "type": {
                "label": "Grt.Types.Ghdl_I32",
                "docHref": "docs/grt__types___spec.html#L36C9"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 42,
              "column": 45,
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
          "label": "Ghdl_Image_P64",
          "qualifier": "",
          "line": 39,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_Image_P64",
                      "href": "docs/grt__images___spec.html#L39C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L40C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L40C29"
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
                      "text": "Ghdl_I64",
                      "href": "docs/grt__types___spec.html#L37C9"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L40C45"
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
              "label": "Res",
              "line": 40,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 40,
              "column": 29,
              "type": {
                "label": "Grt.Types.Ghdl_I64",
                "docHref": "docs/grt__types___spec.html#L37C9"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 40,
              "column": 45,
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
          "label": "Ghdl_Time_To_String_Unit",
          "qualifier": "",
          "line": 63,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Ghdl_Time_To_String_Unit",
                      "href": "docs/grt__images___spec.html#L63C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L64C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L65C7"
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
                      "text": "Std_Time",
                      "href": "docs/grt__types___spec.html#L63C9"
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
                      "text": "Unit",
                      "href": "docs/grt__images___spec.html#L65C23"
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
                      "text": "Std_Time",
                      "href": "docs/grt__types___spec.html#L63C9"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L65C40"
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
              "label": "Res",
              "line": 64,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 65,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_Time",
                "docHref": "docs/grt__types___spec.html#L63C9"
              },
              "description": [
              ]
            },
            {
              "label": "Unit",
              "line": 65,
              "column": 23,
              "type": {
                "label": "Grt.Types.Std_Time",
                "docHref": "docs/grt__types___spec.html#L63C9"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 65,
              "column": 40,
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
          "label": "Ghdl_To_String_B1",
          "qualifier": "",
          "line": 51,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_To_String_B1",
                      "href": "docs/grt__images___spec.html#L51C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L52C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L52C29"
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
                      "text": "Ghdl_B1",
                      "href": "docs/grt__types___spec.html#L31C9"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L52C44"
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
              "label": "Res",
              "line": 52,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 52,
              "column": 29,
              "type": {
                "label": "Grt.Types.Ghdl_B1",
                "docHref": "docs/grt__types___spec.html#L31C9"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 52,
              "column": 44,
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
          "label": "Ghdl_To_String_Char",
          "qualifier": "",
          "line": 57,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_To_String_Char",
                      "href": "docs/grt__images___spec.html#L57C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L58C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L58C29"
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
                      "text": "Std_Character",
                      "href": "docs/grt__types___spec.html#L76C12"
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
              "label": "Res",
              "line": 58,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 58,
              "column": 29,
              "type": {
                "label": "Grt.Types.Std_Character",
                "docHref": "docs/grt__types___spec.html#L76C12"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Ghdl_To_String_E32",
          "qualifier": "",
          "line": 55,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_To_String_E32",
                      "href": "docs/grt__images___spec.html#L55C14"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 56,
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L56C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L56C29"
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
                      "text": "Ghdl_E32",
                      "href": "docs/grt__types___spec.html#L35C12"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L56C45"
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
              "label": "Res",
              "line": 56,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 56,
              "column": 29,
              "type": {
                "label": "Grt.Types.Ghdl_E32",
                "docHref": "docs/grt__types___spec.html#L35C12"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 56,
              "column": 45,
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
          "label": "Ghdl_To_String_E8",
          "qualifier": "",
          "line": 53,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Ghdl_To_String_E8",
                      "href": "docs/grt__images___spec.html#L53C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L54C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L54C29"
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
                      "text": "Ghdl_E8",
                      "href": "docs/grt__types___spec.html#L33C12"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L54C44"
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
              "label": "Res",
              "line": 54,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 54,
              "column": 29,
              "type": {
                "label": "Grt.Types.Ghdl_E8",
                "docHref": "docs/grt__types___spec.html#L33C12"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 54,
              "column": 44,
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
          "label": "Ghdl_To_String_F64",
          "qualifier": "",
          "line": 46,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_To_String_F64",
                      "href": "docs/grt__images___spec.html#L46C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L46C34"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L46C56"
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
                      "text": "Ghdl_F64",
                      "href": "docs/grt__types___spec.html#L39C9"
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
              "label": "Res",
              "line": 46,
              "column": 34,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 46,
              "column": 56,
              "type": {
                "label": "Grt.Types.Ghdl_F64",
                "docHref": "docs/grt__types___spec.html#L39C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Ghdl_To_String_F64_Digits",
          "qualifier": "",
          "line": 47,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_To_String_F64_Digits",
                      "href": "docs/grt__images___spec.html#L47C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L48C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L48C29"
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
                      "text": "Ghdl_F64",
                      "href": "docs/grt__types___spec.html#L39C9"
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
                      "text": "Nbr_Digits",
                      "href": "docs/grt__images___spec.html#L48C45"
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
                      "text": "Ghdl_I32",
                      "href": "docs/grt__types___spec.html#L36C9"
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
              "label": "Res",
              "line": 48,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 48,
              "column": 29,
              "type": {
                "label": "Grt.Types.Ghdl_F64",
                "docHref": "docs/grt__types___spec.html#L39C9"
              },
              "description": [
              ]
            },
            {
              "label": "Nbr_Digits",
              "line": 48,
              "column": 45,
              "type": {
                "label": "Grt.Types.Ghdl_I32",
                "docHref": "docs/grt__types___spec.html#L36C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Ghdl_To_String_F64_Format",
          "qualifier": "",
          "line": 49,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_To_String_F64_Format",
                      "href": "docs/grt__images___spec.html#L49C14"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 50,
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L50C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L50C29"
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
                      "text": "Ghdl_F64",
                      "href": "docs/grt__types___spec.html#L39C9"
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
                      "text": "Format",
                      "href": "docs/grt__images___spec.html#L50C45"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
              "label": "Res",
              "line": 50,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 50,
              "column": 29,
              "type": {
                "label": "Grt.Types.Ghdl_F64",
                "docHref": "docs/grt__types___spec.html#L39C9"
              },
              "description": [
              ]
            },
            {
              "label": "Format",
              "line": 50,
              "column": 45,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Ghdl_To_String_I32",
          "qualifier": "",
          "line": 44,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Ghdl_To_String_I32",
                      "href": "docs/grt__images___spec.html#L44C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L44C34"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L44C56"
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
                      "text": "Ghdl_I32",
                      "href": "docs/grt__types___spec.html#L36C9"
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
              "label": "Res",
              "line": 44,
              "column": 34,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 44,
              "column": 56,
              "type": {
                "label": "Grt.Types.Ghdl_I32",
                "docHref": "docs/grt__types___spec.html#L36C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Ghdl_To_String_I64",
          "qualifier": "",
          "line": 45,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Ghdl_To_String_I64",
                      "href": "docs/grt__images___spec.html#L45C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L45C34"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L45C56"
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
                      "text": "Ghdl_I64",
                      "href": "docs/grt__types___spec.html#L37C9"
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
              "label": "Res",
              "line": 45,
              "column": 34,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 45,
              "column": 56,
              "type": {
                "label": "Grt.Types.Ghdl_I64",
                "docHref": "docs/grt__types___spec.html#L37C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Ghdl_To_String_P32",
          "qualifier": "",
          "line": 59,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
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
                      "text": "Ghdl_To_String_P32",
                      "href": "docs/grt__images___spec.html#L59C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L60C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L60C29"
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
                      "text": "Ghdl_I32",
                      "href": "docs/grt__types___spec.html#L36C9"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L60C45"
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
              "label": "Res",
              "line": 60,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 60,
              "column": 29,
              "type": {
                "label": "Grt.Types.Ghdl_I32",
                "docHref": "docs/grt__types___spec.html#L36C9"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 60,
              "column": 45,
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
          "label": "Ghdl_To_String_P64",
          "qualifier": "",
          "line": 61,
          "column": 14,
          "src": "srcs/grt-images.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
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
                      "text": "Ghdl_To_String_P64",
                      "href": "docs/grt__images___spec.html#L61C14"
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
                      "text": "Res",
                      "href": "docs/grt__images___spec.html#L62C7"
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
                      "text": "Std_String_Ptr",
                      "href": "docs/grt__types___spec.html#L94C9"
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
                      "text": "Val",
                      "href": "docs/grt__images___spec.html#L62C29"
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
                      "text": "Ghdl_I64",
                      "href": "docs/grt__types___spec.html#L37C9"
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
                      "text": "Rti",
                      "href": "docs/grt__images___spec.html#L62C45"
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
              "label": "Res",
              "line": 62,
              "column": 7,
              "type": {
                "label": "Grt.Types.Std_String_Ptr",
                "docHref": "docs/grt__types___spec.html#L94C9"
              },
              "description": [
              ]
            },
            {
              "label": "Val",
              "line": 62,
              "column": 29,
              "type": {
                "label": "Grt.Types.Ghdl_I64",
                "docHref": "docs/grt__types___spec.html#L37C9"
              },
              "description": [
              ]
            },
            {
              "label": "Rti",
              "line": 62,
              "column": 45,
              "type": {
                "label": "Grt.Rtis.Ghdl_Rti_Access",
                "docHref": "docs/grt__rtis___spec.html#L138C9"
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