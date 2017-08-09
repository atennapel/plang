:nil (@Nil ())
:cons (\ht @Cons (.+head h $ .+tail t ()))

::sum
	(?Nil (\_ 0) $
	 ?Cons (\r + (.head r) (sum $ .tail r))
	 end)

:head (?Cons (\r .head r) end)

:test (head $ cons 1 $ cons 2 $ cons 3 nil)

(
	^x (!get ())
	^_ (!set 10)
	^y (!get ())
	return (+ x y)
)
