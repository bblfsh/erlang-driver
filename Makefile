-include .sdk/Makefile

$(if $(filter true,$(sdkloaded)),,$(error You must install bblfsh-sdk))

DIR="_build/default/rel"
RELEASE="pre-alpha"

test-native-internal:
	cd native; \
	rebar3 eunit

build-native-internal:

  if [ -d "build" ]; then rm -r build; fi;
	cd native; \
	if [ -d "_build"]; then rm -r _build; fi;\
  rebar3 release; \
	awk '{gsub("-noinput","");print}' $(DIR)/$(RELEASE)/bin/$(RELEASE) > $(DIR)/$(RELEASE)/bin/tmp;\
	cat $(DIR)/$(RELEASE)/bin/tmp > $(DIR)/$(RELEASE)/bin/$(RELEASE); \
  rm $(DIR)/$(RELEASE)/bin/tmp; \
	cp -r _build $(BUILD_PATH); \
  cp native.sh $(BUILD_PATH)/native; \
	chmod +x $(BUILD_PATH)/native;
	rm  -r .cache
