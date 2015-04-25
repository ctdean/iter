#
# Makefile
# 
# Chris Dean


# Run test refresh with the correct profile and injections.
test_refresh:
	lein with-profile +test trampoline test-refresh :growl
