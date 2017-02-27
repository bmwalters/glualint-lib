#!/bin/sh
java -jar closure_compiler.jar \
	--js all.js \
	--js_output_file compiled.js \
	--compilation_level ADVANCED \
	--output_wrapper "(function(global) {%output%})(typeof global !== 'undefined' ? global : this);"
