/* recursivity is a algebraic type to track
 * whether a let-binding is  recursive or not.
 */
type recursivity = Recursive | Nonrecursive;

/*  name is an alias for string */
type name = string;

/* num is an alias for int */
type num = int;

type expr('a) =
  | EVar(name)
  | ENum(num)
  | EConstr(num, num)
  | EApplic(expr('a), expr('a))
  | ELet(
      recursivity, 
      list( (('a, ) , expr('a) )),
      expr('a)
    )
  /* Each alternative contains a tag, a list of bound variables,
   * and a single expression on the right of the arrow.
   */
  | ECase( expr('a), (num,  list('a), expr('a)))
  /* A lambda is a list of bindings followed by a single expression*/
  | ELam( list('a), expr('a) )
  ;

/* Expressions which have no inner state (i.e. cannot contain
 * subexpressions are considered "atomic". There are only two
 * such atomic expressions: variables and literals.
 */
let isAtomicExpr = (expression) => {
  switch(expression) {
    | EVar(_) => true
    | ENum(_) => true
    | _ => false
  }
};

/* An alternative is an type alias for what we destructure in
 * the above expression for a switch's case.
 */
type alter ('a) = (num,  list('a), expr('a));

/* All expressions are pameterized by strings.
 * The only time this is false is when working with
 * lifting lambdas.
 */
type coreExpr = expr(name);

/* A function definition is a name followed by
 *  a list of parameters, followed by a body.
 */
type scDefn('a) = (name, list('a), expr('a));
type coreScDefn = scDefn(name);

/* A program is a series of top-level function defintions. */
type program('a) = list( scDefn('a));

type coreProgram = program(name);

let preludeDefs = [
  ("I",       ["x"],           EVar("x")),
  ("K",       ["x", "y"],      EVar("x")),
  ("K1",      ["x", "y"],      EVar("y")),
  ("compose", ["f", "g", "x"], EApplic(
                                 EVar("f"), EApplic(
                                   EVar("g"), EVar("x")
                                 ))),
  ("S",  ["f", "g", "x"], EApplic(
                            EApplic(EVar("f"), EVar("x")),
                            EApplic(EVar("g"), EVar("x")),
                          )),
  ("twice", ["f"],        EApplic(
                            EApplic(
                              EVar("compose"), 
                              EVar("f")
                            ),
                            EVar("f")
                          ))
];

Js.log("Core functional language!");
