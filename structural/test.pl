:nil (@Nil ())
:cons (\ht @Cons (.+head h $ .+tail t ()))

::sum
	(?Nil (\_ zero) $
	 ?Cons (\r + (.head r) (sum $ .tail r))
	 end)

:head (?Cons (\r .head r) end)

(head $ cons zero $ cons zero $ cons zero nil)
