-include .sdk/Makefile

$(if $(filter true,$(sdkloaded)),,$(error You must install bblfsh-sdk))

DIR="_build/default/rel"
RELEASE="pre-alpha"

test-native-internal:
	cd native; \
	rebar3 eunit

build-native-internal:

	cd native; \
	rebar3 release
	awk '{gsub("-noinput","");print}' native/$(DIR)/$(RELEASE)/bin/$(RELEASE) > native/$(DIR)/$(RELEASE)/bin/tmp
	cat native/$(DIR)/$(RELEASE)/bin/tmp > native/$(DIR)/$(RELEASE)/bin/$(RELEASE)
	rm native/$(DIR)/$(RELEASE)/bin/tmp
	cp -r native/_build $(BUILD_PATH); \
	cp native/native.sh $(BUILD_PATH)/native; \
	chmod +x $(BUILD_PATH)/native
