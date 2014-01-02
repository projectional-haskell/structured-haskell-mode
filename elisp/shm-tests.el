(defvar shm-tests
  (list
   (list :name "goto-parent"
         :end-buffer-content "main = x
"
         :start-cursor 9
         :finish-cursor 6
         :current-node-overlay '(6 9)
         :start-buffer-content "main = x
"
         :kbd "\341")
   (list :name "goto-parent-in-do-notation"
         :end-buffer-content "main = do return ()
          print ()
"
         :start-cursor 39
         :finish-cursor 8
         :current-node-overlay '(8 39)
         :start-buffer-content "main = do return ()
          print ()
"
         :kbd "\341\341")
   (list :name "goto-parent-end"
         :end-buffer-content "main = return ()
"
         :start-cursor 16
         :finish-cursor 17
         :current-node-overlay '(8 17)
         :start-buffer-content "main = return ()
"
         :kbd ")")
   (list :name "goto-parent-in-function-application"
         :start-buffer-content "main = foo bar mu zot
"
         :start-cursor 19
         :finish-cursor 8
         :current-node-overlay '(8 22)
         :end-buffer-content "main = foo bar mu zot
"
         :kbd "\341")
   (list :name "newline-indent-in-do"
         :end-buffer-content "main = do foo bar
          mu zot
"
         :start-cursor 18
         :finish-cursor 35
         :current-node-overlay '(32 35)
         :start-buffer-content "main = do foo bar
"
         :kbd ")
mu zot")
   (list :name "make-string"
         :start-buffer-content "main = putStrLn
"
         :start-cursor 16
         :finish-cursor 18
         :current-node-overlay '(17 19)
         :end-buffer-content "main = putStrLn \"\"
"
         :kbd "\"")
   (list :name "in-string-delete-empty"
         :start-buffer-content "main = putStrLn \"\"
"
         :start-cursor 18
         :finish-cursor 16
         :current-node-overlay 'nil
         :end-buffer-content "main = putStrLn
"
         :kbd [backspace backspace])
   (list :name "open-double-quote"
         :start-buffer-content "main = return"
         :start-cursor 14
         :finish-cursor 16
         :current-node-overlay '(15 17)
         :end-buffer-content "main = return ()"
         :kbd "(")
   (list :name "wrap-parens"
         :start-buffer-content "main = return
"
         :start-cursor 8
         :finish-cursor 9
         :current-node-overlay '(9 15)
         :end-buffer-content "main = (return)
"
         :kbd "\250")
   (list :name "open-bracket"
         :start-buffer-content "main = print
"
         :start-cursor 13
         :finish-cursor 15
         :current-node-overlay '(14 16)
         :end-buffer-content "main = print []
"
         :kbd "[")
   (list :name "delete-parens"
         :start-buffer-content "main = ()"
         :start-cursor 9
         :finish-cursor 8
         :current-node-overlay '(8 8)
         :end-buffer-content "main = "
         :kbd [backspace])
   (list :name "delete-brackets"
         :start-buffer-content "main = []"
         :start-cursor 9
         :finish-cursor 8
         :current-node-overlay '(8 8)
         :end-buffer-content "main = "
         :kbd [backspace])
   (list :name "delete-braces"
         :start-buffer-content "main = {}"
         :start-cursor 9
         :finish-cursor 8
         :current-node-overlay '(8 8)
         :end-buffer-content "main = "
         :kbd [backspace])
   (list :name "open-brace"
         :start-buffer-content "foo = Foo
"
         :start-cursor 10
         :finish-cursor 12
         :current-node-overlay '(7 13)
         :end-buffer-content "foo = Foo {}
"
         :kbd "{")
   (list :name "comma-in-list"
         :start-buffer-content "main = print [foo]
"
         :start-cursor 18
         :finish-cursor 36
         :current-node-overlay '(33 36)
         :end-buffer-content "main = print [foo
             ,bar]
"
         :kbd "
bar")
   (list :name "new-list-item-on-single-line"
         :start-buffer-content "main = print [a,b,c]
"
         :start-cursor 20
         :finish-cursor 36
         :current-node-overlay '(35 36)
         :end-buffer-content "main = print [a,b,c
             ,x]
"
         :kbd "
x")
   (list :name "newline-indent-function-app"
         :start-buffer-content "main = foo bar
"
         :start-cursor 15
         :finish-cursor 30
         :current-node-overlay '(27 30)
         :end-buffer-content "main = foo bar
           zot
"
         :kbd "
zot")
   (list :name "delete-node"
         :start-buffer-content "main = do return ()
"
         :start-cursor 19
         :finish-cursor 17
         :current-node-overlay '(11 17)
         :end-buffer-content "main = do return
"
         :kbd [delete backspace])
   (list :name "kill-line"
         :start-buffer-content "main = do putStrLn (foo bar mu)
          case x y z of
            Just p -> x
"
         :start-cursor 21
         :finish-cursor 38
         :current-node-overlay '(38 38)
         :end-buffer-content "main = do putStrLn ()
          case  of
            Just p -> x
"
         :kbd "")
   (list :name "isearch"
         :start-buffer-content "main = do foo
          bar
"
         :start-cursor 11
         :finish-cursor 32
         :current-node-overlay '(29 32)
         :end-buffer-content "main = do foo
          bar bob
"
         :kbd "bar bob")
   (list :name "auto-reindentation"
         :start-buffer-content "main = do foo
          bar
"
         :start-cursor 5
         :finish-cursor 3
         :current-node-overlay '(1 3)
         :end-buffer-content "ma = do foo
        bar
"
         :kbd [97 98 99 backspace backspace backspace backspace backspace])
   (list :name "raise"
         :start-buffer-content "main = case x of
         p -> foo (bar bu)
"
         :start-cursor 37
         :finish-cursor 8
         :current-node-overlay '(8 11)
         :end-buffer-content "main = foo bar bu
"
         :kbd "\341\362\341\362")
   (list :name "kill-line"
         :start-buffer-content "main = do foo
          bar
          mu
"
         :start-cursor 11
         :finish-cursor 11
         :current-node-overlay '(11 14)
         :end-buffer-content "main = do foo
          bar
          mu
"
         :kbd "")
   (list :name "kill-line-gobble"
         :start-buffer-content "foo = do
  bar mu
      zot
  bob bill
      ben
  lal dat
      bob

"
         :start-cursor 12
         :finish-cursor 12
         :current-node-overlay '(12 15)
         :end-buffer-content "foo = do
  bar mu
      zot
  bob bill
      ben
  lal dat
      bob

"
         :kbd "")
   (list :name "kill-line-case-example"
         :start-buffer-content "parseModulePragma :: ParseMode -> String -> ParseResult (ModulePragma SrcSpanInfo)
parseModulePragma mode code =
  case parseModuleWithMode mode (code ++ \"\\nmodule X where\") of
    ParseOk (Module _ _ [p] _ _) -> return p
    ParseOk _ -> ParseFailed noLoc \"parseModulePragma\"
    ParseFailed x y -> ParseFailed x y
"
         :start-cursor 182
         :finish-cursor 182
         :current-node-overlay '(182 189)
         :end-buffer-content "parseModulePragma :: ParseMode -> String -> ParseResult (ModulePragma SrcSpanInfo)
parseModulePragma mode code =
  case parseModuleWithMode mode (code ++ \"\\nmodule X where\") of
    ParseOk (Module _ _ [p] _ _) -> return p
    ParseOk _ -> ParseFailed noLoc \"parseModulePragma\"
    ParseFailed x y -> ParseFailed x y
"
         :kbd "")
   (list :name "add-operand"
         :start-buffer-content "main = a <|> b <|> c
"
         :start-cursor 14
         :finish-cursor 15
         :current-node-overlay '(14 15)
         :end-buffer-content "main = a <|> x <|> b <|> c
"
         :kbd [67108907 120])
   (list :name "wrapping-parens-reindent"
         :start-buffer-content "main = foo bar
           mu
"
         :start-cursor 8
         :finish-cursor 9
         :current-node-overlay '(9 12)
         :end-buffer-content "main = (foo bar
            mu)
"
         :kbd "\341\250")
   (list :name "copy/paste"
         :start-buffer-content "main = foo (bar mu
                zot)
  where g = y
"
         :start-cursor 12
         :finish-cursor 26
         :current-node-overlay '(26 57)
         :end-buffer-content "main = foo
  where g = y (bar mu
                   zot)
"
         :kbd [11 backspace 14 5 32 25])
   (list :name "auto-insert-type-sig"
         :start-buffer-content "main
"
         :start-cursor 5
         :finish-cursor 10
         :current-node-overlay '(9 10)
         :end-buffer-content "main :: X
"
         :kbd ":X")
   (list :name "slide-over-used-equal"
         :start-buffer-content "main = x
"
         :start-cursor 8
         :finish-cursor 5
         :current-node-overlay '(1 5)
         :end-buffer-content "main = x
"
         :kbd [backspace])
   (list :name "swing-up"
         :start-buffer-content "hai = do
  foo bar
  mu zot
"
         :start-cursor 8
         :finish-cursor 10
         :current-node-overlay '(10 13)
         :end-buffer-content "hai = do foo bar
         mu zot
"
         :kbd "")
   (list :name "pragmas"
         :start-buffer-content "{}
"
         :start-cursor 2
         :finish-cursor 5
         :current-node-overlay 'nil
         :end-buffer-content "{-#  #-}
"
         :kbd "-#")))

(provide 'shm-tests)
