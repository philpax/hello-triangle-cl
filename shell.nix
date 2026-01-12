{ pkgs ? import <nixpkgs> {} }:

let
  libs = with pkgs; [
    glfw
    libGL
    libffi
  ];
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    sbcl
    rlwrap
    curl
  ] ++ libs;

  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;

  shellHook = ''
    # Install Quicklisp if not present
    if [ ! -f ~/quicklisp/setup.lisp ]; then
      echo "Installing Quicklisp..."
      curl -sO https://beta.quicklisp.org/quicklisp.lisp
      sbcl --non-interactive --load quicklisp.lisp \
           --eval '(quicklisp-quickstart:install)'
      rm quicklisp.lisp
      # Add to init file manually (avoiding interactive prompt)
      if ! grep -q "quicklisp/setup.lisp" ~/.sbclrc 2>/dev/null; then
        cat >> ~/.sbclrc << 'INITEOF'

;;; Load Quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
INITEOF
      fi
      echo "Quicklisp installed!"
    fi
    echo ""
    echo "Common Lisp OpenGL environment ready!"
    echo "  Verify setup: sbcl --load setup.lisp --eval '(verify-setup)' --quit"
    echo "  Run triangle: sbcl --load main.lisp --eval '(run)'"
    echo ""
  '';
}
