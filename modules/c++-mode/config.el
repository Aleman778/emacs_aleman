;;; config.el --- configs for C++ major-mode -*- lexical-binding: t; -*-


;; FIXME(alexander): should not be hardcoded like this, maybe use projectile to setup this.
(setq cpp-compiler "cl") ; The compiler to use.
(setq cpp-build-bin "build.bat") ; The build binary.
(setq cpp-build-file "build.bat") ; The build file, directory search is used to find this file.
(setq cpp-build-output-bin "build/") ; The output from building project.
(setq cpp-run-default-file "win32_platform.exe") ; The default filename for built executable.
(setq cpp-build-directory "") ; This is automatically updated each build.
(setq cpp-generate-project-bin "generate_project.bat gmake") ; Generate build files binary e.g. Makefiles.
(setq cpp-generate-project-file "generate_project.bat") ; Searches for this specific file.
(setq cpp-executable-ext "") ; The command used to run file prefix, e.g. unix use "./".
(setq cpp-executable-ext ".exe") ; The command used to run file prefix, e.g. windows has ".exe".
(defvar msbuild-old-path-var (getenv "PATH"))

;; TODO(alexander): Scan directories, ugh too much work, just change these manually for now
(defconst windows-kits-version-number "10.0.18362.0")
(defconst msvc-version-number "14.27.29110")
(defconst windows-kits-path "C:/Program Files (x86)/Windows Kits/10")
(defconst windows-kits-include-path (concat "C:/Program Files (x86)/Windows Kits/10/Include/" 
                                            windows-kits-version-number))
(defconst ms-visual-studio-path "C:/Program Files (x86)/Microsoft Visual Studio/2020/Community")
(defconst msvc-tools-path (concat "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/MSVC/"
                                  msvc-version-number))

(defun vcvarsall-x64-setup ()
  "Set enviorment variables to load Microsoft Visual C++ Compiler (MSVC) 64 bits"
  (interactive)
  (message "Setting microsoft visual studio X64 environment variables.")
  (setenv "PATH" msbuild-old-path-var)
  (setenv "INCLUDE"
        (concat
             msvc-tools-path "/atlmfc/include"
         ";" msvc-tools-path "/include"
         ";" windows-kits-include-path "/ucrt"
         ";" windows-kits-include-path "/shared"
         ";" windows-kits-include-path "/um"
         ";" windows-kits-include-path "/winrt"
         ))

  (setenv "LIB"
        (concat
             msvc-tools-path "/atlmfc/lib/x64"
         ";" msvc-tools-path "/lib/x64"
         ";" windows-kits-path "/lib/" windows-kits-version-number "/ucrt/x64"
         ";" windows-kits-path "/lib/" windows-kits-version-number "/um/x64"             
         ))

  (setenv  "LIBPATH"
         (concat
              msvc-tools-path "/atlmfc/lib/x64"
          ";" msvc-tools-path "/lib/x64"
          ";" ms-visual-studio-path "/VC/Tools/MSVC/" msvc-version-number "/lib/x64/store/references"
          ";" windows-kits-path "/UnionMetadata/" windows-kits-version-number
          ";" windows-kits-path "/References/" windows-kits-version-number
          ))

  (setenv "PATH"
        (concat
         (getenv "PATH")
         ";" msvc-tools-path "/bin/HostX86/x64"
         ";" ms-visual-studio-path "/Common7/IDE/VC/VCPackages"
         ";" ms-visual-studio-path "/Common7/IDE/CommonExtensions/Microsoft/TestWindow"
         ";" ms-visual-studio-path "/Common7/IDE/CommonExtensions/Microsoft/TeamFoundation/Team Explorer"
         ";" ms-visual-studio-path "/MSBuild/Current/Bin"
         ";" ms-visual-studio-path "/Team Tools/Performance Tools"
         ;; ";" "C:/Program Files (x86)/Microsoft Visual Studio/Shared/Common/VSPerfCollectionTools/"
         ;; ";" "C:/Program Files (x86)/Microsoft SDKs/Windows/v10.0A/bin/NETFX 4.6.1 Tools/"
         ;; ";" "C:/Program Files (x86)/Microsoft SDKs/F#/4.1/Framework/v4.0/"
         ";" windows-kits-path "/bin/x64"
         ";" windows-kits-path "/bin/" windows-kits-version-number "/x64"
         ";" ms-visual-studio-path "//MSBuild/15.0/bin"
         ";" ms-visual-studio-path "/Common7/IDE/"
         ";" ms-visual-studio-path "/Common7/Tools/"
         )))

(defun cpp-get-exe (file)
  (am-file-extension file cpp-executable-ext))

(defun cpp-build-dir ()
  (am-locate-file (am-buffer-path) cpp-build-file))

(defun cpp-generate-dir ()
  (am-locate-file (am-buffer-path) cpp-generate-project-file))

(defun cpp-compile-buffer-file ()
  (interactive)
  (am-run cpp-compiler (am-buffer-file-name) "-o" (cpp-get-exe (am-buffer-file-name))))

(defun cpp-run-buffer-file ()
  (interactive)
  (am-run (cpp-get-exe (am-buffer-file-name))))

(defun cpp-compile-run-buffer-file ()
  (interactive)
  (setq compilation-exit-callback (cpp-run-buffer-file))
  (cpp-compile-buffer-file))

(defun cpp-generate-build-files ()
  (interactive)
  (am-run-in (cpp-generate-dir) cpp-generate-project-bin))

(defun cpp-build-project (options)
  (interactive)
  (am-run-in (cpp-build-dir) cpp-build-bin options))

(defun cpp-run-project ()
  (interactive)
  (am-run-in (concat (cpp-build-dir) cpp-build-output-bin) cpp-run-default-file))

(defun cpp-build-project-default ()
  (interactive)
  (am-run-in (cpp-build-dir) cpp-build-bin))


(defun cpp-build-run-default ()
  (interactive)
  ;; (setq compilation-exit-callback 'cpp-run-project)
  (cpp-build-project-default))

;; Setup indentation style C++
(c-add-style "mystyle"
             '("gnu"
               (c-offsets-alist
                (case-label . +))))

(setq-default c-basic-offset 4)           
(setq c-default-style "mystyle")

;; C++ specific keybinds
(add-hook 'c++-mode-hook
          (lambda ()

            ;; Build and run project (Alt-Ctrl-p)
            (local-set-key (kbd "M-C-p") 'cpp-build-run-default)

            ;; Kill the currently running program (Ctrl-c Ctrl-m)
            (local-set-key (kbd "C-c C-m") 'kill-compilation)

            ;; Generate project files (Alt-Ctrl-u)
            (local-set-key (kbd "M-C-u") 'cpp-generate-build-files)

            ;; Run the specific file (Alt-p)
            (local-set-key (kbd "M-p") 'cpp-compile-run-buffer-file)))
