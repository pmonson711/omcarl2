include Grammar.Spec

let parse_from_string = Parser_driver.parse_from_string

let basic_parse src =
  src |> parse_from_string |> Result.get_ok |> show |> print_endline

let%expect_test "sort exp1" =
  basic_parse
    {|
    sort Natural;
    cons zero: Natural;
         succ: Natural -> Natural;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.SortSpec [(Grammar.IdList ["Natural"])]);
        (Grammar.ConsSpec
           [{ Grammar.id_list = ["zero"]; sort_expr = (Grammar.Id "Natural") };
             { Grammar.id_list = ["succ"];
               sort_expr =
               (Grammar.Function ((Grammar.Id "Natural"), (Grammar.Id "Natural")
                  ))
               }
             ])
        ];
      init = None } |}]

let%expect_test "sort exp2" =
  basic_parse
    {|
    sort Positive;
    cons one: Positive;
         cdub: Bool # Positive -> Positive;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.SortSpec [(Grammar.IdList ["Positive"])]);
        (Grammar.ConsSpec
           [{ Grammar.id_list = ["one"]; sort_expr = (Grammar.Id "Positive") };
             { Grammar.id_list = ["cdub"];
               sort_expr =
               (Grammar.Function (
                  (Grammar.Tuple (Grammar.Bool, (Grammar.Id "Positive"))),
                  (Grammar.Id "Positive")))
               }
             ])
        ];
      init = None } |}]

let%expect_test "sort exp3" =
  basic_parse
    {|
  var x, y: Int;
  eqn square_sum(x,y) = z * z whr z = x + y end;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.EqnSpec (
          (Some (Grammar.VarSpec [[(Grammar.VarsDecl (["x"; "y"], Grammar.Int))]])),
          [(Grammar.EqnDecl (None,
              (Some (Grammar.Access ((Grammar.Id "square_sum"),
                       [(Grammar.Id "x"); (Grammar.Id "y")]))),
              (Grammar.Where (
                 (Grammar.Product ((Grammar.Id "z"), (Grammar.Id "z"))),
                 [(Grammar.Assignment ("z",
                     (Grammar.Sum ((Grammar.Id "x"), (Grammar.Id "y")))))
                   ]
                 ))
              ))
            ]
          ))
        ];
      init = None } |}]

let%expect_test "sort exp4" =
  basic_parse
    {|
  var x, y: Int;
  eqn square_sum(x, y) = (lambda z: Int . z * z)(x + y);
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.EqnSpec (
          (Some (Grammar.VarSpec [[(Grammar.VarsDecl (["x"; "y"], Grammar.Int))]])),
          [(Grammar.EqnDecl (None,
              (Some (Grammar.Access ((Grammar.Id "square_sum"),
                       [(Grammar.Id "x"); (Grammar.Id "y")]))),
              (Grammar.Access (
                 (Grammar.SubExpr
                    (Grammar.Lambda ([(Grammar.VarsDecl (["z"], Grammar.Int))],
                       (Grammar.Product ((Grammar.Id "z"), (Grammar.Id "z")))))),
                 [(Grammar.Sum ((Grammar.Id "x"), (Grammar.Id "y")))]))
              ))
            ]
          ))
        ];
      init = None } |}]

let%expect_test "map eqn var exp1" =
  basic_parse
    {|
    map square_sum: Int # Int -> Int;
    var x, y: Int;
    eqn square_sum(x, y) = (x + y) * (x + y);
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.MapSpec
          [{ Grammar.id_list = ["square_sum"];
             sort_expr =
             (Grammar.Function ((Grammar.Tuple (Grammar.Int, Grammar.Int)),
                Grammar.Int))
             }
            ]);
        (Grammar.EqnSpec (
           (Some (Grammar.VarSpec
                    [[(Grammar.VarsDecl (["x"; "y"], Grammar.Int))]])),
           [(Grammar.EqnDecl (None,
               (Some (Grammar.Access ((Grammar.Id "square_sum"),
                        [(Grammar.Id "x"); (Grammar.Id "y")]))),
               (Grammar.Product (
                  (Grammar.SubExpr
                     (Grammar.Sum ((Grammar.Id "x"), (Grammar.Id "y")))),
                  (Grammar.SubExpr
                     (Grammar.Sum ((Grammar.Id "x"), (Grammar.Id "y"))))
                  ))
               ))
             ]
           ))
        ];
      init = None } |}]

let%expect_test "map eqn var exp2" =
  basic_parse
    {|
    var x, y: Int;
    eqn square_sum(x,y) = z * z whr z = x + y end;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.EqnSpec (
          (Some (Grammar.VarSpec [[(Grammar.VarsDecl (["x"; "y"], Grammar.Int))]])),
          [(Grammar.EqnDecl (None,
              (Some (Grammar.Access ((Grammar.Id "square_sum"),
                       [(Grammar.Id "x"); (Grammar.Id "y")]))),
              (Grammar.Where (
                 (Grammar.Product ((Grammar.Id "z"), (Grammar.Id "z"))),
                 [(Grammar.Assignment ("z",
                     (Grammar.Sum ((Grammar.Id "x"), (Grammar.Id "y")))))
                   ]
                 ))
              ))
            ]
          ))
        ];
      init = None } |}]

let%expect_test "map eqn var exp3" =
  basic_parse
    {|
    var x, y: Int;
    eqn square_sum(x, y) = (lambda z: Int . z * z)(x + y);
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.EqnSpec (
          (Some (Grammar.VarSpec [[(Grammar.VarsDecl (["x"; "y"], Grammar.Int))]])),
          [(Grammar.EqnDecl (None,
              (Some (Grammar.Access ((Grammar.Id "square_sum"),
                       [(Grammar.Id "x"); (Grammar.Id "y")]))),
              (Grammar.Access (
                 (Grammar.SubExpr
                    (Grammar.Lambda ([(Grammar.VarsDecl (["z"], Grammar.Int))],
                       (Grammar.Product ((Grammar.Id "z"), (Grammar.Id "z")))))),
                 [(Grammar.Sum ((Grammar.Id "x"), (Grammar.Id "y")))]))
              ))
            ]
          ))
        ];
      init = None } |}]

let%expect_test "map eqn var exp3" =
  basic_parse
    {|
    map fib: Nat -> Nat;
    var n: Nat;
    eqn n <= 1 -> fib(n) = n;
        n > 1 -> fib(n) = fib(Int2Nat(n - 1)) + fib(Int2Nat(n - 2));
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.MapSpec
          [{ Grammar.id_list = ["fib"];
             sort_expr = (Grammar.Function (Grammar.Nat, Grammar.Nat)) }
            ]);
        (Grammar.EqnSpec (
           (Some (Grammar.VarSpec [[(Grammar.VarsDecl (["n"], Grammar.Nat))]])),
           [(Grammar.EqnDecl (
               (Some (Grammar.LessThanEqual ((Grammar.Id "n"), (Grammar.Number 1)
                        ))),
               (Some (Grammar.Access ((Grammar.Id "fib"), [(Grammar.Id "n")]))),
               (Grammar.Id "n")));
             (Grammar.EqnDecl (
                (Some (Grammar.GreaterThan ((Grammar.Id "n"), (Grammar.Number 1)
                         ))),
                (Some (Grammar.Access ((Grammar.Id "fib"), [(Grammar.Id "n")]))),
                (Grammar.Access (
                   (Grammar.Sum (
                      (Grammar.Access ((Grammar.Id "fib"),
                         [(Grammar.Access ((Grammar.Id "Int2Nat"),
                             [(Grammar.Difference ((Grammar.Id "n"),
                                 (Grammar.Number 1)))
                               ]
                             ))
                           ]
                         )),
                      (Grammar.Id "fib"))),
                   [(Grammar.Access ((Grammar.Id "Int2Nat"),
                       [(Grammar.Difference ((Grammar.Id "n"), (Grammar.Number 2)
                           ))
                         ]
                       ))
                     ]
                   ))
                ))
             ]
           ))
        ];
      init = None } |}]

let%expect_test "map eqn var exp4" =
  basic_parse
    {|
    sort S;
    cons a, b: S;
    map x: Set(S);
    eqn x = {a} + {b};
        a < b = true;
        b < a = false;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.SortSpec [(Grammar.IdList ["S"])]);
        (Grammar.ConsSpec
           [{ Grammar.id_list = ["a"; "b"]; sort_expr = (Grammar.Id "S") }]);
        (Grammar.MapSpec
           [{ Grammar.id_list = ["x"]; sort_expr = (Grammar.Set (Grammar.Id "S"))
              }
             ]);
        (Grammar.EqnSpec (None,
           [(Grammar.EqnDecl (None, (Some (Grammar.Id "x")),
               (Grammar.Sum ((Grammar.Set [(Grammar.Id "a")]),
                  (Grammar.Set [(Grammar.Id "b")])))
               ));
             (Grammar.EqnDecl (None,
                (Some (Grammar.LessThan ((Grammar.Id "a"), (Grammar.Id "b")))),
                (Grammar.Bool true)));
             (Grammar.EqnDecl (None,
                (Some (Grammar.LessThan ((Grammar.Id "b"), (Grammar.Id "a")))),
                (Grammar.Bool false)))
             ]
           ))
        ];
      init = None } |}]

let%expect_test "map eqn var exp5" =
  basic_parse
    {|
      var s, t: S;
          b: Bool;
      eqn s == s -> true;
          s < s -> false;
          s <= s -> true;
          if(true, s, t) = s;
          if(false, s, t) = t;
          if(b, s, s) = s;
      |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.EqnSpec (
          (Some (Grammar.VarSpec
                   [[(Grammar.VarsDecl (["s"; "t"], (Grammar.Id "S")))];
                     [(Grammar.VarsDecl (["b"], Grammar.Bool))]])),
          [(Grammar.EqnDecl (
              (Some (Grammar.Equal ((Grammar.Id "s"), (Grammar.Id "s")))), None,
              (Grammar.Bool true)));
            (Grammar.EqnDecl (
               (Some (Grammar.LessThan ((Grammar.Id "s"), (Grammar.Id "s")))),
               None, (Grammar.Bool false)));
            (Grammar.EqnDecl (
               (Some (Grammar.LessThanEqual ((Grammar.Id "s"), (Grammar.Id "s")))),
               None, (Grammar.Bool true)));
            (Grammar.EqnDecl (None,
               (Some (Grammar.Access ((Grammar.Id "if"),
                        [(Grammar.Bool true); (Grammar.Id "s"); (Grammar.Id "t")]
                        ))),
               (Grammar.Id "s")));
            (Grammar.EqnDecl (None,
               (Some (Grammar.Access ((Grammar.Id "if"),
                        [(Grammar.Bool false); (Grammar.Id "s"); (Grammar.Id "t")
                          ]
                        ))),
               (Grammar.Id "t")));
            (Grammar.EqnDecl (None,
               (Some (Grammar.Access ((Grammar.Id "if"),
                        [(Grammar.Id "b"); (Grammar.Id "s"); (Grammar.Id "s")]))),
               (Grammar.Id "s")))
            ]
          ))
        ];
      init = None } |}]

let%expect_test "underspecification" =
  basic_parse
    {|
     sort A, B;
     cons b: B;
     map f: A;
         g: B;
         h: A -> B;
     var a: A;
     eqn h(a) = b;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.SortSpec [(Grammar.IdList ["A"; "B"])]);
        (Grammar.ConsSpec
           [{ Grammar.id_list = ["b"]; sort_expr = (Grammar.Id "B") }]);
        (Grammar.MapSpec
           [{ Grammar.id_list = ["f"]; sort_expr = (Grammar.Id "A") };
             { Grammar.id_list = ["g"]; sort_expr = (Grammar.Id "B") };
             { Grammar.id_list = ["h"];
               sort_expr =
               (Grammar.Function ((Grammar.Id "A"), (Grammar.Id "B"))) }
             ]);
        (Grammar.EqnSpec (
           (Some (Grammar.VarSpec
                    [[(Grammar.VarsDecl (["a"], (Grammar.Id "A")))]])),
           [(Grammar.EqnDecl (None,
               (Some (Grammar.Access ((Grammar.Id "h"), [(Grammar.Id "a")]))),
               (Grammar.Id "b")))
             ]
           ))
        ];
      init = None } |}]

let%expect_test "xor" =
  basic_parse
    {|
    map xor: Bool # Bool -> Bool;
    eqn xor(false, false) = false;
        xor(false, true) = true;
        xor(true, false) = true;
        xor(true, true) = false;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.MapSpec
          [{ Grammar.id_list = ["xor"];
             sort_expr =
             (Grammar.Function ((Grammar.Tuple (Grammar.Bool, Grammar.Bool)),
                Grammar.Bool))
             }
            ]);
        (Grammar.EqnSpec (None,
           [(Grammar.EqnDecl (None,
               (Some (Grammar.Access ((Grammar.Id "xor"),
                        [(Grammar.Bool false); (Grammar.Bool false)]))),
               (Grammar.Bool false)));
             (Grammar.EqnDecl (None,
                (Some (Grammar.Access ((Grammar.Id "xor"),
                         [(Grammar.Bool false); (Grammar.Bool true)]))),
                (Grammar.Bool true)));
             (Grammar.EqnDecl (None,
                (Some (Grammar.Access ((Grammar.Id "xor"),
                         [(Grammar.Bool true); (Grammar.Bool false)]))),
                (Grammar.Bool true)));
             (Grammar.EqnDecl (None,
                (Some (Grammar.Access ((Grammar.Id "xor"),
                         [(Grammar.Bool true); (Grammar.Bool true)]))),
                (Grammar.Bool false)))
             ]
           ))
        ];
      init = None } |}]

let%expect_test "xor2" =
  basic_parse
    {|
     map xor: Bool # Bool -> Bool;
     var a, b: Bool;
     eqn xor(a, b) = a != b;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.MapSpec
          [{ Grammar.id_list = ["xor"];
             sort_expr =
             (Grammar.Function ((Grammar.Tuple (Grammar.Bool, Grammar.Bool)),
                Grammar.Bool))
             }
            ]);
        (Grammar.EqnSpec (
           (Some (Grammar.VarSpec
                    [[(Grammar.VarsDecl (["a"; "b"], Grammar.Bool))]])),
           [(Grammar.EqnDecl (None,
               (Some (Grammar.Access ((Grammar.Id "xor"),
                        [(Grammar.Id "a"); (Grammar.Id "b")]))),
               (Grammar.NotEqual ((Grammar.Id "a"), (Grammar.Id "b")))))
             ]
           ))
        ];
      init = None } |}]

let%expect_test "xor3" =
  basic_parse
    {|
     map xor: Bool # Bool -> Bool;
     var a, b: Bool;
     eqn a == b -> xor(a, b) = false;
         a != b -> xor(a, b) = true;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.MapSpec
          [{ Grammar.id_list = ["xor"];
             sort_expr =
             (Grammar.Function ((Grammar.Tuple (Grammar.Bool, Grammar.Bool)),
                Grammar.Bool))
             }
            ]);
        (Grammar.EqnSpec (
           (Some (Grammar.VarSpec
                    [[(Grammar.VarsDecl (["a"; "b"], Grammar.Bool))]])),
           [(Grammar.EqnDecl (
               (Some (Grammar.Equal ((Grammar.Id "a"), (Grammar.Id "b")))),
               (Some (Grammar.Access ((Grammar.Id "xor"),
                        [(Grammar.Id "a"); (Grammar.Id "b")]))),
               (Grammar.Bool false)));
             (Grammar.EqnDecl (
                (Some (Grammar.NotEqual ((Grammar.Id "a"), (Grammar.Id "b")))),
                (Some (Grammar.Access ((Grammar.Id "xor"),
                         [(Grammar.Id "a"); (Grammar.Id "b")]))),
                (Grammar.Bool true)))
             ]
           ))
        ];
      init = None } |}]

let%expect_test "fib2" =
  basic_parse
    {|
     map fib: Nat -> Nat;
     var n: Nat;
     eqn fib(0) = 0;
         fib(1) = 1;
         fib(n + 2) = fib(n) + fib(n + 1);
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.MapSpec
          [{ Grammar.id_list = ["fib"];
             sort_expr = (Grammar.Function (Grammar.Nat, Grammar.Nat)) }
            ]);
        (Grammar.EqnSpec (
           (Some (Grammar.VarSpec [[(Grammar.VarsDecl (["n"], Grammar.Nat))]])),
           [(Grammar.EqnDecl (None,
               (Some (Grammar.Access ((Grammar.Id "fib"), [(Grammar.Number 0)]))),
               (Grammar.Number 0)));
             (Grammar.EqnDecl (None,
                (Some (Grammar.Access ((Grammar.Id "fib"), [(Grammar.Number 1)]))),
                (Grammar.Number 1)));
             (Grammar.EqnDecl (None,
                (Some (Grammar.Access ((Grammar.Id "fib"),
                         [(Grammar.Sum ((Grammar.Id "n"), (Grammar.Number 2)))]))),
                (Grammar.Access (
                   (Grammar.Sum (
                      (Grammar.Access ((Grammar.Id "fib"), [(Grammar.Id "n")])),
                      (Grammar.Id "fib"))),
                   [(Grammar.Sum ((Grammar.Id "n"), (Grammar.Number 1)))]))
                ))
             ]
           ))
        ];
      init = None } |}]

let%expect_test "remove" =
  basic_parse
    {|
     map remove: List(Nat) # Nat -> List(Nat);
     var x, y: Nat;
         l: List(Nat);
     eqn remove([], x) = [];
         x == y -> remove(x |> l, y) = l;
         x != y -> remove(x |> l, y) = x |> remove(l, y);
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.MapSpec
          [{ Grammar.id_list = ["remove"];
             sort_expr =
             (Grammar.Function (
                (Grammar.Tuple ((Grammar.List Grammar.Nat), Grammar.Nat)),
                (Grammar.List Grammar.Nat)))
             }
            ]);
        (Grammar.EqnSpec (
           (Some (Grammar.VarSpec
                    [[(Grammar.VarsDecl (["x"; "y"], Grammar.Nat))];
                      [(Grammar.VarsDecl (["l"], (Grammar.List Grammar.Nat)))]])),
           [(Grammar.EqnDecl (None,
               (Some (Grammar.Access ((Grammar.Id "remove"),
                        [(Grammar.List []); (Grammar.Id "x")]))),
               (Grammar.List [])));
             (Grammar.EqnDecl (
                (Some (Grammar.Equal ((Grammar.Id "x"), (Grammar.Id "y")))),
                (Some (Grammar.Access ((Grammar.Id "remove"),
                         [(Grammar.Snoc ((Grammar.Id "x"), (Grammar.Id "l")));
                           (Grammar.Id "y")]
                         ))),
                (Grammar.Id "l")));
             (Grammar.EqnDecl (
                (Some (Grammar.NotEqual ((Grammar.Id "x"), (Grammar.Id "y")))),
                (Some (Grammar.Access ((Grammar.Id "remove"),
                         [(Grammar.Snoc ((Grammar.Id "x"), (Grammar.Id "l")));
                           (Grammar.Id "y")]
                         ))),
                (Grammar.Snoc ((Grammar.Id "x"),
                   (Grammar.Access ((Grammar.Id "remove"),
                      [(Grammar.Id "l"); (Grammar.Id "y")]))
                   ))
                ))
             ]
           ))
        ];
      init = None } |}]

let%expect_test "simple tree" =
  basic_parse {|
    sort Tree = struct leaf(A) | node(Tree, Tree);
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.SortSpec
          [(Grammar.SortType
              { Grammar.id = "Tree";
                signature =
                (Grammar.Struct
                   [{ Grammar.const_id = "leaf";
                      proj_decls =
                      [{ Grammar.proj_id = None; sort_expr = (Grammar.Id "A") }];
                      guard = None };
                     { Grammar.const_id = "node";
                       proj_decls =
                       [{ Grammar.proj_id = None; sort_expr = (Grammar.Id "Tree")
                          };
                         { Grammar.proj_id = None;
                           sort_expr = (Grammar.Id "Tree") }
                         ];
                       guard = None }
                     ])
                })
            ])
        ];
      init = None } |}]

let%expect_test "simple tree" =
  basic_parse
    {|
    sort Tree = struct leaf(value: A) ? is_leaf
                     | node(left: Tree, right: Tree) ? is_node;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.SortSpec
          [(Grammar.SortType
              { Grammar.id = "Tree";
                signature =
                (Grammar.Struct
                   [{ Grammar.const_id = "leaf";
                      proj_decls =
                      [{ Grammar.proj_id = (Some "value");
                         sort_expr = (Grammar.Id "A") }
                        ];
                      guard = (Some "is_leaf") };
                     { Grammar.const_id = "node";
                       proj_decls =
                       [{ Grammar.proj_id = (Some "left");
                          sort_expr = (Grammar.Id "Tree") };
                         { Grammar.proj_id = (Some "right");
                           sort_expr = (Grammar.Id "Tree") }
                         ];
                       guard = (Some "is_node") }
                     ])
                })
            ])
        ];
      init = None } |}]

let%expect_test "pairs" =
  basic_parse {|
    sort Pair = struct pair(fst: A, snd: B);
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.SortSpec
          [(Grammar.SortType
              { Grammar.id = "Pair";
                signature =
                (Grammar.Struct
                   [{ Grammar.const_id = "pair";
                      proj_decls =
                      [{ Grammar.proj_id = (Some "fst");
                         sort_expr = (Grammar.Id "A") };
                        { Grammar.proj_id = (Some "snd");
                          sort_expr = (Grammar.Id "B") }
                        ];
                      guard = None }
                     ])
                })
            ])
        ];
      init = None } |}]

let%expect_test "glob" =
  basic_parse {|
    glob x: Nat;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.GlobalVarSpec [[(Grammar.VarsDecl (["x"], Grammar.Nat))]])];
      init = None } |}]

let%expect_test "act" =
  basic_parse {|
    act a, b, c ;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs = [(Grammar.ActSpec [(Grammar.IdList ["a"; "b"; "c"])])];
      init = None } |}]

let%expect_test "act" =
  basic_parse {|
    act a, b, c : Bool # Bool ;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.ActSpec
          [(Grammar.SortProduct (["a"; "b"; "c"],
              (Grammar.Tuple (Grammar.Bool, Grammar.Bool))))
            ])
        ];
      init = None } |}]

let%expect_test "act exp1" =
  basic_parse
    {|
    act send: Message;
        print: Letter # Screen;
        Shake_hand;
    |} ;
  [%expect
    {|
    { Grammar.Spec.specs =
      [(Grammar.ActSpec
          [(Grammar.SortProduct (["send"], (Grammar.Id "Message")));
            (Grammar.SortProduct (["print"],
               (Grammar.Tuple ((Grammar.Id "Letter"), (Grammar.Id "Screen")))));
            (Grammar.IdList ["Shake_hand"])])
        ];
      init = None } |}]
