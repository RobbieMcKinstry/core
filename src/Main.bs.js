// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");

function isAtomicExpr(expression) {
  switch (expression.tag | 0) {
    case 0 : 
    case 1 : 
        return true;
    default:
      return false;
  }
}

console.log("Core functional language!");

var preludeDefs = /* :: */[
  /* tuple */[
    "I",
    /* :: */[
      "x",
      /* [] */0
    ],
    /* EVar */Block.__(0, ["x"])
  ],
  /* :: */[
    /* tuple */[
      "K",
      /* :: */[
        "x",
        /* :: */[
          "y",
          /* [] */0
        ]
      ],
      /* EVar */Block.__(0, ["x"])
    ],
    /* :: */[
      /* tuple */[
        "K1",
        /* :: */[
          "x",
          /* :: */[
            "y",
            /* [] */0
          ]
        ],
        /* EVar */Block.__(0, ["y"])
      ],
      /* :: */[
        /* tuple */[
          "compose",
          /* :: */[
            "f",
            /* :: */[
              "g",
              /* :: */[
                "x",
                /* [] */0
              ]
            ]
          ],
          /* EApplic */Block.__(3, [
              /* EVar */Block.__(0, ["f"]),
              /* EApplic */Block.__(3, [
                  /* EVar */Block.__(0, ["g"]),
                  /* EVar */Block.__(0, ["x"])
                ])
            ])
        ],
        /* :: */[
          /* tuple */[
            "S",
            /* :: */[
              "f",
              /* :: */[
                "g",
                /* :: */[
                  "x",
                  /* [] */0
                ]
              ]
            ],
            /* EApplic */Block.__(3, [
                /* EApplic */Block.__(3, [
                    /* EVar */Block.__(0, ["f"]),
                    /* EVar */Block.__(0, ["x"])
                  ]),
                /* EApplic */Block.__(3, [
                    /* EVar */Block.__(0, ["g"]),
                    /* EVar */Block.__(0, ["x"])
                  ])
              ])
          ],
          /* :: */[
            /* tuple */[
              "twice",
              /* :: */[
                "f",
                /* [] */0
              ],
              /* EApplic */Block.__(3, [
                  /* EApplic */Block.__(3, [
                      /* EVar */Block.__(0, ["compose"]),
                      /* EVar */Block.__(0, ["f"])
                    ]),
                  /* EVar */Block.__(0, ["f"])
                ])
            ],
            /* [] */0
          ]
        ]
      ]
    ]
  ]
];

exports.isAtomicExpr = isAtomicExpr;
exports.preludeDefs = preludeDefs;
/*  Not a pure module */