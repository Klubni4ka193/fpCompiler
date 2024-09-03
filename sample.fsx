type Identifier = string
type Expression = 
    | Var of Identifier
    | Abstraction of Identifier * Expression
    | Application of Expression * Expression
    | IntLiteral of int
    | IfThenElse of Expression * Expression * Expression
    | LetIn of Identifier * Expression * Expression
    | Recursion of Identifier * Expression * Expression
    | FuncDefinition of Identifier
    | Operator of Identifier * int * Expression list
    | BracketExpr of Expression * Environment
    | RecBracketExpr of Expression * Environment * Identifier
and Environment = Map<Identifier, Expression>


let arity = function
  | "sin" -> 1
  | _ -> 2


let applyOperator = function
  | "+" -> (function [IntLiteral a; IntLiteral b] -> IntLiteral(a + b))
  | "-" -> (function [IntLiteral a; IntLiteral b] -> IntLiteral(a - b))
  | "*" -> (function [IntLiteral a; IntLiteral b] -> IntLiteral(a * b))
  | "/" -> (function [IntLiteral a; IntLiteral b] -> IntLiteral(a / b))
  | "=" -> (function [IntLiteral a; IntLiteral b] -> if a = b then IntLiteral(1) else IntLiteral(0))
  | ">" -> (function [IntLiteral a; IntLiteral b] -> if a > b then IntLiteral(1) else IntLiteral(0))
  | "<" -> (function [IntLiteral a; IntLiteral b] -> if a < b then IntLiteral(1) else IntLiteral(0))
  | "<=" -> (function [IntLiteral a; IntLiteral b] -> if a <= b then IntLiteral(1) else IntLiteral(0))
  | _ -> failwith "Unknown operator"


let rec evaluateExpr expr env =
    match expr with
    | Application(e1, e2) -> apply (evaluateExpr e1 env) (evaluateExpr e2 env)
    | IntLiteral n -> IntLiteral n
    | Var x -> Map.find x env
    | FuncDefinition f -> Operator(f, arity f, [])
    | Operator(op, n, args) -> Operator(op, n, args)
    | IfThenElse(e0, e1, e2) -> 
        if IntLiteral(1) = evaluateExpr e0 env then evaluateExpr e1 env else evaluateExpr e2 env
    | LetIn(id, e1, e2) ->
        let result = evaluateExpr e1 env in
        evaluateExpr e2 (Map.add id result env)
    | Recursion(id, e1, e2) -> 
        evaluateExpr e2 (Map.add id (RecBracketExpr(e1, env, id)) env)
    | Abstraction(id, ex) -> BracketExpr(expr, env)
    | BracketExpr(expr, env) -> expr
    
    
and apply e1 e2 =
    match e1 with 
      | BracketExpr(Abstraction(v, e), env) -> evaluateExpr e (Map.add v e2 env)
      | RecBracketExpr(Abstraction(v, e), env, id) -> evaluateExpr e (Map.add v e2 (Map.add id e1 env))
      | Operator(op, n, args) ->
          if n = 1 then (applyOperator op) (args @ [e2])
          else Operator(op, n - 1, args @ [e2])


let evaluate exp = evaluateExpr exp Map.empty

printfn "Sum = %A" (evaluate (Application(Application(FuncDefinition("+"), IntLiteral(6)), IntLiteral(3))))
printfn "Variable = %A" (evaluate (Application(Abstraction("x", Var("x")), IntLiteral(136))))
printfn "Lambda expression = %A" (evaluate (LetIn("y", IntLiteral(5), Application(Abstraction("x", Application(Application(FuncDefinition("+"), Var("x")), Var("y"))), IntLiteral(1)))))
printfn "Square = %A" (evaluate (LetIn("id", Abstraction("x", Var("x")),
       LetIn("sq", Abstraction("z", Application(Application(FuncDefinition("*"), Var("z")), Var("z"))),
            Application(Var("sq"), Application(Var("id"), IntLiteral(2)))
       ))))

printfn "Factorial = %A" (evaluate (Recursion("fact", Abstraction("x",
  IfThenElse(Application(Application(FuncDefinition("<="), Var("x")), IntLiteral(1)), IntLiteral(1), Application(Application(FuncDefinition("*"), Var("x")), Application(Var("fact"), Application(Application(FuncDefinition("-"), Var("x")), IntLiteral(1)))))),
  Application(Var("fact"), IntLiteral(7)))))

printfn "Sum = %A" (evaluate (Application(Application(FuncDefinition("+"), IntLiteral(6)), IntLiteral(3))))
printfn "Square = %A" (evaluate (LetIn("id", Abstraction("x", Var("x")),
       LetIn("sq", Abstraction("z", Application(Application(FuncDefinition("*"), Var("z")), Var("z"))),
            Application(Var("sq"), Application(Var("id"), IntLiteral(2)))
       ))))
printfn "Sum = %A" (evaluate (Application(Application(FuncDefinition("*"), IntLiteral(-14)), IntLiteral(4))))
