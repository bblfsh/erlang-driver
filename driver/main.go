package main

import (
	"github.com/bblfsh/sdk/protocol/cmd"

	"github.com/bblfsh/erlang-driver/driver/normalizer"
)

var version string
var build string

func main() {
	cmd.DriverMain(version, build,
		normalizer.NativeToNoder,
		normalizer.AnnotationRules)
}
