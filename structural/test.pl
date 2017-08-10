:Nothing (@Nothing ())
:Just (\v @Just v)

:handleFail (\r (#fail (\_k ret Nothing) $ final (\v ret (Just v)) $ r))

:fail (!fail ())

:program1 (
	^_ fail
	ret 100
)

(handleFail program1)
