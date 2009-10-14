#
# $Id: Makefile 131597 2009-04-09 06:34:51Z norton $
#


MODULES = \
	ubf_jsonrpc

PLUGINS = \

EUNITTESTS = \

MY_UBF_EBIN = ./example-symlink-to-ubf-ebin
MY_UBF_INCLUDE = ./example-symlink-to-ubf-include

# include erl make
include ../priv/make/erl_make.mk

ERL_COMPILE_FLAGS += \
	-I $(MY_UBF_INCLUDE) \
	-pz $(MY_UBF_EBIN)

ERL_FLAGS += \
	-pz $(MY_UBF_EBIN)

# standard targets
check: all
	-rm -f ./*.log
	$(RUNERL1) -noinput -noshell -pz $(EUNITTEST_DIR) \
		-s erlang halt \
		> ./check.log
