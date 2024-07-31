#!/bin/sh
# To get rid of annoying OpenJDK deprecation warning, see:
# https://github.com/technomancy/leiningen/issues/2611#issuecomment-577288859
export LEIN_JVM_OPTS="-XX:TieredStopAtLevel=1"
lein test