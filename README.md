
# My .emacs.d folder

Nothing spectacular - just a way to share a single config across machines.

To install:

- replace existing `~/.emacs.d/init.el` file with the following:

        (load (concat (file-name-as-directory user-emacs-directory)
                      (file-name-as-directory "rp-init")
                      "main.el") nil)

- `ln -s <absolute path to rp-init> ~/.emacs.d`
- `ln -s <absolute path to rp-lisp> ~/.emacs.d`

All initializations are in folder `rp-init` required by the initial `init.el`

Files not under source control:

- `rp-init/custom.el` containing the `customize` entries
- `rp-init/local.el` containing local initializations

Files in `./rp-lisp` are my personal ELisp code projects. They may or may not be useful to you.


## References

https://www.masteringemacs.org/reading-guide

https://www.gnu.org/software/emacs/manual/html_mono/emacs.html

https://www.gnu.org/software/emacs/manual/html_mono/elisp.html

