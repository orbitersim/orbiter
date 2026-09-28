#!/bin/sh
# not upstream: Linux counterpart of createAPI_References.bat, HTML output instead of CHM
# usage: createAPI_References.sh [output folder], default out/doc/VulkanClient in the source tree
cd "$(dirname "$0")" || exit 1
command -v doxygen >/dev/null || { echo "doxygen is not installed (https://doxygen.org)"; exit 1; }
command -v dot >/dev/null || { echo "dot is not installed (graphviz, https://graphviz.org)"; exit 1; }
out="${1:-../../../out/doc/VulkanClient}"
mkdir -p "$out" || exit 1

# settings after the Doxyfile override it: the Windows paths, CHM and LaTeX output
{ cat Doxyfile; cat <<EOF
PROJECT_NAME = "Orbiter Visualisation Project - VulkanClient"
STRIP_FROM_PATH = ../../..
INPUT = .. ../../../Orbitersdk/include/DrawAPI.h ../../../Orbitersdk/include/GraphicsAPI.h ../../../Orbitersdk/include/ModuleAPI.h
OUTPUT_DIRECTORY = $out
HTML_OUTPUT = VulkanClient_API_Reference
GENERATE_HTMLHELP = NO
GENERATE_LATEX = NO
EOF
} | doxygen - || exit 1

{ cat Doxyfile-gcAPI; cat <<EOF
STRIP_FROM_PATH = ../../..
INPUT = ../gcCore.h ../gcConst.h ../gcGUI.h ../../../Orbitersdk/include/DrawAPI.h
OUTPUT_DIRECTORY = $out
HTML_OUTPUT = gcAPI_Reference
GENERATE_HTMLHELP = NO
GENERATE_LATEX = NO
EOF
} | doxygen - || exit 1

echo "API references: $out/VulkanClient_API_Reference/index.html, $out/gcAPI_Reference/index.html"
