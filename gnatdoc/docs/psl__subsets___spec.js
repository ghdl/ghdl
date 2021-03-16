GNATdoc.Documentation = {
  "label": "PSL.Subsets",
  "qualifier": "",
  "summary": [
  ],
  "description": [
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Check_Simple",
          "qualifier": "",
          "line": 38,
          "column": 14,
          "src": "srcs/psl-subsets.ads.html",
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
                      "text": "Check_Simple",
                      "href": "docs/psl__subsets___spec.html#L38C14"
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
                      "href": "docs/psl__subsets___spec.html#L38C28"
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
                  "text": "Check that N (a property) follows the simple subset rules from\n"
                },
                {
                  "kind": "span",
                  "text": "PSL v1.1 4.4.4 Simple subset.\n"
                },
                {
                  "kind": "span",
                  "text": "Ie:\n"
                },
                {
                  "kind": "ul",
                  "children": [
                    {
                      "kind": "li",
                      "children": [
                        {
                          "kind": "span",
                          "text": "The operand of a negation operator is a Boolean.\n"
                        }
                      ]
                    },
                    {
                      "kind": "li",
                      "children": [
                        {
                          "kind": "span",
                          "text": "The operand of a 'never' operator is a Boolean or a Sequence.\n"
                        }
                      ]
                    },
                    {
                      "kind": "li",
                      "children": [
                        {
                          "kind": "span",
                          "text": "The operand of an 'eventually!' operator is a Boolean or a Sequence.\n"
                        }
                      ]
                    },
                    {
                      "kind": "li",
                      "children": [
                        {
                          "kind": "span",
                          "text": "The left-hand side operand of a logical 'and' operator is a Boolean.\n"
                        }
                      ]
                    },
                    {
                      "kind": "li",
                      "children": [
                        {
                          "kind": "span",
                          "text": "The left-hand side operand of a logical 'or' operator is a Boolean.\n"
                        }
                      ]
                    },
                    {
                      "kind": "li",
                      "children": [
                        {
                          "kind": "span",
                          "text": "The left-hand side operand of a logical implication '->' operator\n"
                        },
                        {
                          "kind": "paragraph",
                          "children": [
                            {
                              "kind": "span",
                              "text": "is a Boolean.\n"
                            }
                          ]
                        }
                      ]
                    },
                    {
                      "kind": "li",
                      "children": [
                        {
                          "kind": "span",
                          "text": "Both operands of a logical iff '<->' operator are Boolean.\n"
                        }
                      ]
                    },
                    {
                      "kind": "li",
                      "children": [
                        {
                          "kind": "span",
                          "text": "The right-hand side operand of a non-overlapping 'until*' operator is\n"
                        },
                        {
                          "kind": "paragraph",
                          "children": [
                            {
                              "kind": "span",
                              "text": "a Boolean.\n"
                            }
                          ]
                        }
                      ]
                    },
                    {
                      "kind": "li",
                      "children": [
                        {
                          "kind": "span",
                          "text": "Both operands of an overlapping 'until*' operator are Boolean.\n"
                        }
                      ]
                    },
                    {
                      "kind": "li",
                      "children": [
                        {
                          "kind": "span",
                          "text": "Both operands of a 'before*' operator are Boolean.\n"
                        }
                      ]
                    }
                  ]
                },
                {
                  "kind": "span",
                  "text": "All other operators not mentioned above are supported in the simple\n"
                },
                {
                  "kind": "span",
                  "text": "subset without restriction.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "N",
              "line": 38,
              "column": 28,
              "type": {
                "label": "PSL.Nodes.Node",
                "docHref": "docs/psl__nodes___spec.html#L450C12"
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