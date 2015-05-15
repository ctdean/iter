#
# Makefile
# 
# Chris Dean


test:
	lein test

.PHONY: test

# Run test refresh with the correct profile and injections.
test_refresh:
	lein with-profile +test trampoline test-refresh :growl
