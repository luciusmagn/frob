{ pkgs, src }:

let
  lib = pkgs.lib;
  expectedSbclVersion = lib.removeSuffix "\n" (builtins.readFile "${src}/sbcl.version");
  expectedSbclSourceHash = lib.removeSuffix "\n" (builtins.readFile "${src}/sbcl-source.sha256");

  clinedi = pkgs.sbcl.buildASDFSystem {
    pname = "clinedi";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "clinedi";
      rev = "a5d7d519b413bde2e046e1c049b750c62502ca8b";
      hash = "sha256-nRGK2ZDi6cDUrjCy3rGZtWLBytZhFCkCjY0Po60GVmg=";
    };
  };

  autolithSystem = pkgs.sbcl.buildASDFSystem {
    pname = "autolith";
    version = "0.9.9";
    inherit src;
    systems = [ "autolith" "autolith/tests" ];
    lispLibs = with pkgs.sbclPackages; [
      alexandria
      bordeaux-threads
      cl-base64
      closer-mop
      dexador
      quri
      serapeum
      yason
      clinedi
    ];
    nativeBuildInputs = [ pkgs.git ];

    postInstall = ''
      # Upstream launchers load .qlot/setup.lisp. Map that tiny interface to
      # the Nix-provided ASDF registry so startup and image builds stay offline.
      mkdir -p "$out/.qlot"
      cat > "$out/.qlot/setup.lisp" <<'LISP'
      (require :asdf)
      (defpackage #:ql
        (:use #:cl)
        (:export #:quickload))
      (in-package #:ql)
      (defun quickload (system &key silent &allow-other-keys)
        (declare (ignore silent))
        (asdf:load-system system))
      LISP

      rm -f "$out/.gitignore"
      cp ${src}/.gitignore "$out/.gitignore"
      chmod u+w "$out/.gitignore"
      printf '\n/nix-support/\n' >> "$out/.gitignore"

      # Autolith records source provenance with Git. Flake source archives do
      # not contain .git, so create a deterministic, read-only repository.
      git init --quiet --initial-branch=master "$out"
      git -C "$out" config user.name "Autolith Nix build"
      git -C "$out" config user.email "nix-build@localhost"
      git -C "$out" add --all
      GIT_AUTHOR_DATE='2000-01-01T00:00:00Z' \
        GIT_COMMITTER_DATE='2000-01-01T00:00:00Z' \
        git -C "$out" commit --quiet --message "Autolith source"

      # A stat-less index does not need refreshing when Git reads it from the
      # immutable Nix store at runtime.
      rm "$out/.git/index"
      git -C "$out" read-tree HEAD
    '';
  };

  runtime = pkgs.sbcl.withPackages (_: [ autolithSystem ]);

  sbclSource = pkgs.runCommand "autolith-sbcl-${expectedSbclVersion}-source" {
    nativeBuildInputs = [ pkgs.bzip2 pkgs.coreutils pkgs.gnutar ];
  } ''
    actual_hash=$(sha256sum ${pkgs.sbcl.src} | cut -d ' ' -f 1)
    if [ "$actual_hash" != "${expectedSbclSourceHash}" ]; then
      echo "SBCL source hash mismatch: expected ${expectedSbclSourceHash}, got $actual_hash" >&2
      exit 1
    fi

    mkdir -p "$out"
    tar -xjf ${pkgs.sbcl.src} --strip-components=1 -C "$out"
    test -f "$out/version.lisp-expr"
    test -f "$out/src/code/list.lisp"
  '';

in
assert pkgs.stdenv.hostPlatform.isx86_64;
assert pkgs.sbcl.version == expectedSbclVersion;
pkgs.writeShellApplication {
  name = "autolith";
  runtimeInputs = [
    pkgs.bash
    pkgs.coreutils
    pkgs.git
    pkgs.gnugrep
    runtime
  ];
  text = ''
    if [ -z "''${AUTOLITH_HOME:-}" ]; then
      AUTOLITH_HOME="''${HOME:-/home/user}/.autolith"
    fi

    export XDG_STATE_HOME="$AUTOLITH_HOME/state"
    export XDG_DATA_HOME="$AUTOLITH_HOME/data"
    export XDG_CACHE_HOME="$AUTOLITH_HOME/cache"
    export CODEX_HOME="$AUTOLITH_HOME/codex"
    export AUTOLITH_SBCL="${runtime}/bin/sbcl"
    export AUTOLITH_SBCL_SOURCE_ROOT="${sbclSource}"

    # The packaged source repository is root-owned in /nix/store. Permit Git
    # provenance reads without weakening safe.directory globally.
    export GIT_CONFIG_COUNT=1
    export GIT_CONFIG_KEY_0=safe.directory
    export GIT_CONFIG_VALUE_0="${autolithSystem}"
    export GIT_OPTIONAL_LOCKS=0

    mkdir -p \
      "$XDG_STATE_HOME" \
      "$XDG_DATA_HOME" \
      "$XDG_CACHE_HOME" \
      "$CODEX_HOME"

    runtime_root="$XDG_DATA_HOME/autolith/runtimes/${expectedSbclVersion}"
    mkdir -p "$runtime_root"

    if [ "$(readlink "$runtime_root/source" 2>/dev/null || true)" != "${sbclSource}" ]; then
      rm -rf "$runtime_root/source"
      ln -s "${sbclSource}" "$runtime_root/source"
    fi
    printf '%s %s\n' \
      "${expectedSbclVersion}" \
      "${expectedSbclSourceHash}" > "$runtime_root/source.identity"
    printf '%s\n' "${runtime}/bin/sbcl" > "$runtime_root/command"

    recovery_core="$XDG_DATA_HOME/autolith/recovery/autolith-recovery.core"
    active_core="$XDG_DATA_HOME/autolith/active/autolith-active.core"

    # Build the fast recovery + active images whenever the packaged Autolith
    # changes (its store path is the upgrade signal) or an image is missing.
    # Upstream's own bootstrap fetches SBCL and deps over the network, so we
    # drive the offline build scripts directly with the Nix-provided runtime.
    # We gate on the store path rather than re-deriving upstream's image-
    # manifest validity check: a core that is corrupt without a version change
    # is caught by the launcher we exec below, which re-validates and falls
    # back to a (slower) source load.
    build_marker="$XDG_DATA_HOME/autolith/images.built-for"
    if [ ! -f "$recovery_core" ] || [ ! -f "$active_core" ] ||
       [ "$(cat "$build_marker" 2>/dev/null || true)" != "${autolithSystem}" ]; then
      echo "Building Autolith images (first run or guest upgrade)..." >&2
      "$AUTOLITH_SBCL" --script "${autolithSystem}/script/build-recovery.lisp"
      "$AUTOLITH_SBCL" --script "${autolithSystem}/script/build-active.lisp"
      printf '%s\n' "${autolithSystem}" > "$build_marker"
    fi

    exec ${pkgs.bash}/bin/bash "${autolithSystem}/bin/autolith" "$@"
  '';

  meta = {
    description = "A live, self-modifying Common Lisp agent";
    homepage = "https://github.com/luciusmagn/autolith";
    license = lib.licenses.mit;
    mainProgram = "autolith";
    platforms = [ "x86_64-linux" ];
  };

  passthru = {
    inherit autolithSystem runtime sbclSource;
  };
}
