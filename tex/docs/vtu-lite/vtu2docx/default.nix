# Build the vtu-lite Word manuscript from the LaTeX sources.
#
# The paper's prose becomes real Word text; mathematics, listings, the grammar
# table and the note boxes are typeset with the paper's own LaTeX preamble and
# embedded as 600 dpi PNG images.
{ lib
, stdenv
, python3
, poppler-utils
, latexTools
, src
, common
, name ? "vtu-lite"
}:
stdenv.mkDerivation {
  pname = "${name}-docx";
  version = "0.1";

  inherit src;

  nativeBuildInputs = latexTools.deps ++ [ python3 poppler-utils ];

  buildPhase = ''
    runHook preBuild
    # shellcheck source=/dev/null
    . ${latexTools.latexShellInit}
    export TEXINPUTS="${common}:''${TEXINPUTS-}"
    export BIBINPUTS="${common}:''${BIBINPUTS-}"
    mkdir -p build
    python3 ${./vtu2docx.py} \
      --paper main.tex \
      --common ${common} \
      --template ${./template/sample-mcse-eng.docx} \
      --out build/${name}.docx
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm644 build/${name}.docx $out/${name}.docx
    runHook postInstall
  '';

  meta = {
    description = "MCSE journal Word manuscript of the vtu-lite paper";
    license = lib.licenses.cc-by-40;
    platforms = lib.platforms.linux;
  };
}
