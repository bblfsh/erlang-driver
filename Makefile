-include .sdk/Makefile

$(if $(filter true,$(sdkloaded)),,$(error You must install bblfsh-sdk))

DIR="_build/default/rel";
RELEASE="pre-alpha";

test-native:
	cd native; \
	rebar3 eunit

build-native:
	cd native; \
	rebar3 relase \
	awk '{gsub("-noinput","");print}' $DIR/$RELEASE/bin/$RELEASE > $DIR/$RELEASE/bin/tmp; \
	cat $DIR/$RELEASE/bin/tmp > $DIR/$RELEASE/bin/$RELEASE; \
	rm _build/default/rel/pre-alpha/bin/tmp; \
	echo -e "#!/bin/bash\necho 'not implemented'" > $(BUILD_PATH)/native
	cp native/_build $(BUILD_PATH); \
	cp native/native.sh $(BUILD_PATH)/native; \
	chmod +x $(BUILD_PATH)/native
