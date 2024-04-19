;;; lsp-docker.el --- LSP Docker integration         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; URL: https://github.com/emacs-lsp/lsp-docker
;; Keywords: languages langserver
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (dash "2.14.1") (lsp-mode "6.2.1") (f "0.20.0") (s "1.13.0") (yaml "0.2.0") (ht "2.0"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run language servers in containers

;;; Code:
(require 'lsp-mode)
(require 'dash)
(require 'f)
(require 's)
(require 'yaml)
(require 'ht)

(defgroup lsp-docker nil
  "Language Server Protocol dockerized servers support."
  :group 'lsp-mode
  :tag "Language Server in docker (lsp-docker)")

(defcustom lsp-docker-log-docker-supplemental-calls nil
  "If non-nil, all docker command supplemental-calls will be logged to a buffer."
  :group 'lsp-docker
  :type 'boolean)

(defcustom lsp-docker-log-docker-supplemental-calls-buffer-name "*lsp-docker-supplemental-calls*"
  "Log docker supplemental calls using this particular buffer."
  :group 'lsp-docker
  :type 'string)

;; top node keys
(defconst lsp-docker--lsp-key 'lsp
  "Main key associated to the root-node of the containerized language servers")

;; 1st sub-node keys
(defconst lsp-docker--server-key 'server
  "LSP sub-key holding a single (or a group of) server(s)")

(defconst lsp-docker--mappings-key 'mappings
  "Collection of mappings between host-paths and
containerized-paths (host paths must be within the project)")

;; 2nd sub-node keys
;; supported keys in YAML configuration file(s)
(defconst lsp-docker--srv-cfg-type-key 'type
  "The type of server (at the moment only `docker' is supported).")

(defconst lsp-docker--srv-cfg-subtype-key 'subtype
  "For type container it can be:
- `container': attach to an already running container
- `image': when the image does not exist, try to build it based on the dockerfile
  found in the project-scope An image might feature an optional tag, i.e.
  `<image>:<tag>'. the If a tagless image is indicated `latest' will be assumed")

(defconst lsp-docker--srv-cfg-name-key 'name
  "Depending on the `lsp-docker--srv-cfg-subtype-key' it holds the
name of the container/image for the described language server.")

(defconst lsp-docker--srv-cfg-server-key 'server
  "Server ID of a registered LSP server. You can find the list of
registered servers evaluating: `(ht-keys lsp-clients)'.")

(defconst lsp-docker--srv-cfg-launch-parameters-key 'launch_parameters
  "Command parameters (docker or podman) to launch the language server with.
Pay attention that these parameters have to be supported by the selected subtype.")

(defconst lsp-docker--srv-cfg-launch-command-key 'launch_command
  "Command to launch the language server in stdio mode. This key is
not used when the `lsp-docker--srv-cfg-subtype-key' is set to
container, as the server command shall be the entrypoint.")

(defun lsp-docker--log-docker-supplemental-calls-p ()
  "Return non-nil if should log docker invocation commands"
  lsp-docker-log-docker-supplemental-calls)

(defun lsp-docker--uri->path (path-mappings docker-container-name uri)
  "Turn docker URI into host path.
Argument PATH-MAPPINGS dotted pair of (host-path . container-path).
Argument DOCKER-CONTAINER-NAME name to use when running container.
Argument URI the uri to translate."
  (let ((path (lsp--uri-to-path-1 uri)))
    (-if-let ((local . remote) (-first (-lambda ((_ . docker-path))
                                         (s-contains? docker-path path))
                                       path-mappings))
        (replace-regexp-in-string (format "\\(%s\\).*" remote) local path nil nil 1)
      (format "/docker:%s:%s" docker-container-name path))))

(defun lsp-docker--path->uri (path-mappings path)
  "Turn host PATH into docker uri.
Argument PATH-MAPPINGS dotted pair of (host-path . container-path).
Argument PATH the path to translate."
  (lsp--path-to-uri-1
   (-if-let ((local . remote) (-first (-lambda ((local-path . _))
                                        (s-contains? local-path path))
                                      path-mappings))
       (replace-regexp-in-string (format "\\(%s\\).*" local) remote path nil nil 1)
     (user-error "The path %s is not under path mappings" path))))

(defvar lsp-docker-container-name-suffix 0
  "Used to prevent collision of container names.")

(defvar lsp-docker-command "docker"
  "The docker command to use.")

(defun lsp-docker-launch-new-container (docker-container-name path-mappings launch-parameters docker-image-id server-command)
  "Return the docker command to be executed on host.
Argument DOCKER-CONTAINER-NAME name to use for container.
Argument PATH-MAPPINGS dotted pair of (host-path . container-path).
Argument DOCKER-IMAGE-ID the docker container to run language servers with.
Argument LAUNCH-PARAMETERS parameters (for docker or podman) to run language servers with.
Argument SERVER-COMMAND the language server command to run inside the container."
  (-remove #'s-blank?
           (split-string
            (format "%s run --name %s --rm -i %s %s %s %s"
                    lsp-docker-command
                    docker-container-name
                    (->> path-mappings
                         (-map (-lambda ((path . docker-path))
                                 (format "-v %s:%s" path docker-path)))
                         (s-join " "))
                    (s-join " " launch-parameters)
                    docker-image-id
                    server-command)
            " ")))

(defun lsp-docker-exec-in-container (docker-container-name server-command)
  "Return command to exec into running container.
Argument DOCKER-CONTAINER-NAME name of container to exec into.
Argument SERVER-COMMAND the command to execute inside the running container."
(split-string
 (format "%s exec -i %s %s" lsp-docker-command docker-container-name server-command)))

(defun lsp-docker--attach-container-name-global-suffix (container-name)
  "Attach a user-specified or a default suffix (properly changing it) to the container name"
  (if lsp-docker-container-name-suffix
      (format "%s-%d"
              container-name
              (if (numberp lsp-docker-container-name-suffix)
                  (cl-incf lsp-docker-container-name-suffix)
                lsp-docker-container-name-suffix))
    container-name))

(cl-defun lsp-docker-register-client (&key server-id
                                           docker-server-id
                                           path-mappings
                                           launch-parameters
                                           docker-image-id
                                           docker-container-name
                                           priority
                                           server-command
                                           launch-server-cmd-fn)
  "Registers docker clients with lsp"
  (if-let ((client (copy-lsp--client (gethash server-id lsp-clients))))
      (progn
        (let ((docker-container-name-full (lsp-docker--attach-container-name-global-suffix docker-container-name)))
          (setf (lsp--client-server-id client) docker-server-id
                (lsp--client-uri->path-fn client) (-partial #'lsp-docker--uri->path
                                                            path-mappings
                                                            docker-container-name-full)
                (lsp--client-path->uri-fn client) (-partial #'lsp-docker--path->uri path-mappings)
                (lsp--client-new-connection client) (plist-put
                                                     (lsp-stdio-connection
                                                      (lambda ()
                                                        (funcall (or launch-server-cmd-fn #'lsp-docker-launch-new-container)
                                                                 docker-container-name-full
                                                                 path-mappings
                                                                 launch-parameters
                                                                 docker-image-id
                                                                 server-command)))
                                                     :test? (lambda (&rest _)
                                                              (-any?
                                                               (-lambda ((dir))
                                                                 (f-ancestor-of? dir (buffer-file-name)))
                                                               path-mappings)))
                (lsp--client-priority client) (or priority (lsp--client-priority client))))
        (lsp-register-client client))
    (user-error "No such client %s" server-id)))

(defvar lsp-docker-default-client-packages
  '(lsp-bash
    lsp-clangd
    lsp-css
    lsp-dockerfile
    lsp-go
    lsp-html
    lsp-javascript
    lsp-pylsp)
  "Default list of client packages to load.")

(defvar lsp-docker-default-client-configs
  (list
   (list :server-id 'bash-ls :docker-server-id 'bashls-docker :server-command "bash-language-server start")
   (list :server-id 'clangd :docker-server-id 'clangd-docker :server-command "ccls")
   (list :server-id 'css-ls :docker-server-id 'cssls-docker :server-command "css-languageserver --stdio")
   (list :server-id 'dockerfile-ls :docker-server-id 'dockerfilels-docker :server-command "docker-langserver --stdio")
   (list :server-id 'gopls :docker-server-id 'gopls-docker :server-command "gopls")
   (list :server-id 'html-ls :docker-server-id 'htmls-docker :server-command "html-languageserver --stdio")
   (list :server-id 'pylsp :docker-server-id 'pyls-docker :server-command "pylsp")
   (list :server-id 'ts-ls :docker-server-id 'tsls-docker :server-command "typescript-language-server --stdio"))
  "Default list of client configurations.")

(cl-defun lsp-docker-init-clients (&key
                                        path-mappings
                                        (docker-image-id "emacslsp/lsp-docker-langservers")
                                        (docker-container-name "lsp-container")
                                        (priority 10)
                                        (client-packages lsp-docker-default-client-packages)
                                        (client-configs lsp-docker-default-client-configs))
  "Loads the required client packages and registers the required clients to run with docker.

:path-mappings is an alist of local paths and their mountpoints
in the docker container.
Example: '((\"/path/to/projects\" . \"/projects\"))

:docker-image-id is the identifier for the docker image to be
used for all clients, as a string.

:docker-container-name is the name to use for the container when
it is started.

:priority is the priority with which to register the docker
clients with lsp.  (See the library ‘lsp-clients’ for details.)

:client-packages is a list of libraries to load before registering the clients.

:client-configs is a list of configurations for the various
clients you wish to use with ‘lsp-docker’.  Each element takes
the form
'(:server-id 'example-ls
  :docker-server-id 'examplels-docker
  :docker-image-id \"examplenamespace/examplels-docker:x.y\"
  :docker-container-name \"examplels-container\"
  :server-command \"run_example_ls.sh\")
where
:server-id is the ID of the language server, as defined in the
library ‘lsp-clients’.

:docker-server-id is any arbitrary unique symbol used internally
by ‘lsp’ to distinguish it from non-docker clients for the same
server.

:docker-image-id is an optional property to override this
function's :docker-image-id argument for just this client.  If
you specify this, you MUST also specify :docker-container-name.

:docker-container-name is an optional property to override this
function's :docker-container-name argument for just this client.
This MUST be specified if :docker-image-id is specified, but is
otherwise optional.

:server-command is a string specifying the command to run inside
the docker container to run the language server."
  (seq-do (lambda (package) (require package nil t)) client-packages)
  (let ((default-docker-image-id docker-image-id)
        (default-docker-container-name docker-container-name))
    (seq-do (-lambda ((&plist :server-id :docker-server-id :docker-image-id :docker-container-name :server-command))
        (when (and docker-image-id (not docker-container-name))
          (user-error "Invalid client definition for server ID %S. You must specify a container name when specifying an image ID."
                 server-id))
        (lsp-docker-register-client
         :server-id server-id
         :priority priority
         :docker-server-id docker-server-id
         :docker-image-id (or docker-image-id default-docker-image-id)
         :docker-container-name (if docker-image-id
                                    docker-container-name
                                  default-docker-container-name)
         :server-command server-command
         :path-mappings path-mappings
         :launch-parameters nil
         :launch-server-cmd-fn #'lsp-docker-launch-new-container))
      client-configs)))

(defvar lsp-docker-default-priority
  100
  "Default lsp-docker containerized servers priority (it needs to
be bigger than default servers in order to override them)")

(defcustom lsp-docker-persistent-default-config
  (ht (lsp-docker--server-key (ht (lsp-docker--srv-cfg-type-key "docker")
                                  (lsp-docker--srv-cfg-subtype-key "image")
                                  (lsp-docker--srv-cfg-name-key "emacslsp/lsp-docker-langservers")
                                  (lsp-docker--srv-cfg-server-key nil)
                                  (lsp-docker--srv-cfg-launch-command-key nil)))
      (lsp-docker--mappings-key (vector
                                 (ht ('source ".")
                                     ('destination "/projects")))))
  "Default configuration for all language servers with persistent configurations"
  :type 'hash-table
  :group 'lsp-docker)

(defun lsp-docker-get-config-from-project-config-file (project-config-file-path)
  "Get the LSP configuration based on a project configuration file"
  (if (f-exists? project-config-file-path)
      (if-let* ((whole-config (yaml-parse-string (f-read project-config-file-path)))
                (lsp-config (gethash lsp-docker--lsp-key whole-config)))
          ;; use default values for missing fields in the provided configuration
          (if (vectorp (gethash lsp-docker--server-key lsp-config))
            lsp-config        ; DO NOT merge to the persistent configuration when a multi-server one is detected
            (ht-merge (ht-copy lsp-docker-persistent-default-config) lsp-config)))))

(defun lsp-docker--find-project-config-file-from-lsp ()
  "Get the LSP configuration file path (project-local configuration, using lsp-mode)"
  (let ((config-file-path-candidates (list)))
    (when (lsp-workspace-root)
      (push (f-join (lsp-workspace-root) ".lsp-docker.yml") config-file-path-candidates)
      (push (f-join (lsp-workspace-root) ".lsp-docker.yaml") config-file-path-candidates)
      (push (f-join (f-join (lsp-workspace-root) ".lsp-docker") ".lsp-docker.yml") config-file-path-candidates)
      (push (f-join (f-join (lsp-workspace-root) ".lsp-docker") ".lsp-docker.yaml") config-file-path-candidates)
      (push (f-join (f-join (lsp-workspace-root) ".lsp-docker") "lsp-docker.yml") config-file-path-candidates)
      (push (f-join (f-join (lsp-workspace-root) ".lsp-docker") "lsp-docker.yaml") config-file-path-candidates)
      (push (f-join (f-join (lsp-workspace-root) ".lsp-docker") "config.yml") config-file-path-candidates)
      (push (f-join (f-join (lsp-workspace-root) ".lsp-docker") "config.yaml") config-file-path-candidates)
      (--first (f-exists? it) config-file-path-candidates))))

(defun lsp-docker--find-project-dockerfile-from-lsp ()
  "Get the LSP server building Dockerfile path using lsp-mode"
  (let ((dockerfile-path-candidates (list)))
    (when (lsp-workspace-root)
      (push (f-join (f-join (lsp-workspace-root) ".lsp-docker") "Dockerfile") dockerfile-path-candidates)
      (push (f-join (f-join (lsp-workspace-root) ".lsp-docker") "Dockerfile.lsp") dockerfile-path-candidates)
      (--first (f-exists? it) dockerfile-path-candidates))))

(defun lsp-docker--find-building-path-from-dockerfile (dockerfile-path)
  "Get the LSP server building folder path using an explicit dockerfile path"
  (when dockerfile-path
    (f-dirname (f-dirname dockerfile-path))))

(defun lsp-docker-get-config-from-lsp ()
  "Get the LSP configuration based on a project-local configuration (using lsp-mode)"
  (let ((project-config-file-path (lsp-docker--find-project-config-file-from-lsp)))
    (if project-config-file-path
        (lsp-docker-get-config-from-project-config-file project-config-file-path)
      (ht-copy lsp-docker-persistent-default-config))))

(defvar lsp-docker-supported-server-types-subtypes
  (ht ('docker (list 'container 'image)))
  "A list of all supported server types and subtypes, currently only docker is supported")

(defun lsp-docker-get-server-type-subtype (server-config)
  "Get the server type & sub-type from the SERVER-CONFIG hash-table"
  (let* ((lsp-server-type (gethash lsp-docker--srv-cfg-type-key server-config))
         (lsp-server-subtype (gethash lsp-docker--srv-cfg-subtype-key server-config)))
    (cons (if (stringp lsp-server-type)
              (intern lsp-server-type)
            lsp-server-type)
          (if (stringp lsp-server-subtype)
              (intern lsp-server-subtype)
            lsp-server-subtype))))

(defun lsp-docker-get-server-container-name (server-config)
  "Get the server container name from the SERVER-CONFIG hash-table"
  (let* ((lsp-server-subtype (gethash 'subtype server-config)))
    (if (equal lsp-server-subtype "container")
        (gethash 'name server-config))))

(defun lsp-docker-get-server-image-name (server-config)
  "Get the server image name from the SERVER-CONFIG hash-table"
  (let* ((lsp-server-subtype (gethash 'subtype server-config)))
    (if (equal lsp-server-subtype "image")
        (gethash 'name server-config))))

(defun lsp-docker--get-server-launch-parameters (server-config)
  "Get the server launch parameters from the SERVER-CONFIG hash-table"
  (let ((launch-parameters (gethash lsp-docker--srv-cfg-launch-parameters-key server-config)))
    (if (or (vectorp launch-parameters)
            (not launch-parameters))
        launch-parameters
      (user-error "Cannot find the right launch parameters"))))

(defun lsp-docker-get-server-id (server-config)
  "Get the server id from the SERVER-CONFIG hash-table"
  (let ((server-id (gethash lsp-docker--srv-cfg-server-key server-config)))
    (if (stringp server-id)
        (intern server-id)
      server-id)))

(defun lsp-docker--get-base-client (base-server-id)
  "Get the base lsp client associated to BASE-SERVER-ID key for
dockerized client to be built upon"
  (if-let* ((base-client (gethash base-server-id lsp-clients)))
      base-client
    (user-error "Cannot find the specified base lsp client (%s)!
Make sure the '%s' sub-key is set to one of the lsp registered clients:\n\n%s"
                base-server-id lsp-docker--srv-cfg-server-key (ht-keys lsp-clients))))

(defun lsp-docker-get-path-mappings (config project-directory)
  "Get the server path mappings from the top project hash-table CONFIG"
  (if-let ((lsp-mappings-info (gethash lsp-docker--mappings-key config)))
      (--map (cons (f-canonical (f-expand (gethash 'source it)
                                          project-directory))
                   (gethash 'destination it))
             lsp-mappings-info)
    (user-error "No path mappings specified!")))

(defun lsp-docker-get-launch-command (server-config)
  "Get the server launch command from the SERVER-CONFIG hash-table"
  (gethash lsp-docker--srv-cfg-launch-command-key server-config))

(defun lsp-docker-check-server-type-subtype (supported-server-types-subtypes server-type-subtype)
  "Verify that the combination of server (type . subtype) is supported by the current implementation"
  (if (not server-type-subtype)
      (user-error "No server type and subtype specified!"))
  (if (ht-find (lambda (type subtypes)
                 (let ((server-type (car server-type-subtype))
                       (server-subtype (cdr server-type-subtype)))
                   (and (equal server-type type)
                        (-contains? subtypes server-subtype))))
               supported-server-types-subtypes)
      server-type-subtype
    (user-error "No compatible server type and subtype found!")))

(defun lsp-docker-check-path-mappings (path-mappings)
  "Verify that specified path mappings are all inside the project directory"
  (--all? (or (f-descendant-of? (f-canonical (car it)) (f-canonical (lsp-workspace-root)))
              (f-same? (f-canonical (car it)) (f-canonical (lsp-workspace-root))))
          path-mappings))

(defun lsp-docker-launch-existing-container (docker-container-name &rest _unused)
  "Return the docker command to be executed on host.
Argument DOCKER-CONTAINER-NAME name to use for container."
  (split-string
   (format "%s start -ia %s"
           lsp-docker-command
           docker-container-name)
   " "))

(defun lsp-docker-create-activation-function-by-project-dir (project-dir)
  `(lambda (&rest unused)
     (let ((current-project-root (lsp-workspace-root))
           (registered-project-root ,project-dir))
       (f-same? current-project-root registered-project-root))))

(defun lsp-docker--create-activation-function-by-project-dir-and-base-client (project-dir base-lsp-client)
  `(lambda (current-file-name current-major-mode)
     (let ((current-project-root (lsp-workspace-root))
           (registered-project-root ,project-dir)
           (base-activation-fn ,(lsp--client-activation-fn base-lsp-client))
           (base-major-modes ',(lsp--client-major-modes base-lsp-client)))
       (and (f-same? current-project-root registered-project-root)
            (or (if (functionp base-activation-fn)
                    (funcall base-activation-fn current-file-name current-major-mode)
                  nil)
                (-contains? base-major-modes current-major-mode))))))

(defun lsp-docker-generate-docker-server-id (server-config project-root)
  "Generate the docker-server-id from the SERVER-CONFIG"
  (let ((original-server-id (symbol-name (lsp-docker-get-server-id server-config)))
         (project-path-server-id-part (s-chop-prefix "-" (s-replace-all '(("/" . "-") ("." . "")) project-root))))
    (intern (s-join "-" (list project-path-server-id-part original-server-id "docker")))))

(defun lsp-docker--generate-docker-server-container-name (server-config project-root)
  "Generate the docker-container-name from the SERVER-CONFIG"
  (let ((docker-server-id (lsp-docker-generate-docker-server-id server-config project-root)))
    (if (symbolp docker-server-id)
        (symbol-name docker-server-id)
      docker-server-id)))

(defun lsp-docker--finalize-docker-server-container-name (config-specified-server-name server-config project-root)
  "Get or generate the container name.

If CONFIG-SPECIFIED-SERVER-NAME is non-nil, return it as
container name. Otherwise generate a unique container name from
SERVER-CONFIG and PROJECT-ROOT.
"
  (cond ((stringp config-specified-server-name) config-specified-server-name)
         ('t (lsp-docker--attach-container-name-global-suffix (lsp-docker--generate-docker-server-container-name server-config project-root)))))

(defun lsp-docker--encode-single-quoted-parameters (raw-token-command)
  "Encode single quoted tokens (with base64 encoding) so they won't be split"
  (let* ((tokens-to-encode (--remove (s-blank-str? (cadr it)) (s-match-strings-all "'\\([^']+\\)'" raw-token-command)))
         (replacement-pairs (--mapcat (list (cons (car it) (format "'%s'" (base64-encode-string (cadr it))))) tokens-to-encode)))
    (s-replace-all replacement-pairs raw-token-command)))

(defun lsp-docker--decode-single-quoted-parameters (encoded-token-command)
  "Decode single quoted tokens (base64-encoded) so they can be used again"
  (let* ((tokens-to-decode (--remove (s-blank-str? (cadr it)) (s-match-strings-all "'\\([^']+\\)'" encoded-token-command)))
         (replacement-pairs (--mapcat (list (cons (car it) (format "'%s'" (base64-decode-string (cadr it))))) tokens-to-decode)))
    (s-replace-all replacement-pairs encoded-token-command)))

(defun lsp-docker--decode-single-quoted-tokens (command-tokens)
  "Decode single quoted tokens (base64-encoded) from a token list"
  (--map-when (s-match "'\\([^']+\\)'" it) (format "'%s'" (base64-decode-string (cadr (s-match "'\\([^']+\\)'" it)))) command-tokens))

(defun lsp-docker--run-docker-command (command-arguments)
  "Run a command (with a configurable command itself: docker or
podman) and get its exit code and output as a pair (exit-code .
output)"
  (lsp-docker--run-external-command (format "%s %s" lsp-docker-command command-arguments)))

(defun lsp-docker--get-build-command (image-name dockerfile-path)
  "Get a building command string"
  (format "%s build --tag %s --file %s %s" lsp-docker-command image-name dockerfile-path (lsp-docker--find-building-path-from-dockerfile dockerfile-path)))

(defun lsp-docker--run-image-build (image-name dockerfile-path buffer-name)
  "Build the specified image using a particular dockerfile (with its output redirected to a specified buffer)"
  (-let ((
          (command-program . command-arguments)
          (lsp-docker--decode-single-quoted-tokens (s-split " " (lsp-docker--encode-single-quoted-parameters (lsp-docker--get-build-command image-name dockerfile-path))))))
    (with-current-buffer (get-buffer-create buffer-name)
      (message "Building the image %s, please open the %s buffer for details" image-name buffer-name)
      (apply #'call-process command-program nil (current-buffer) nil command-arguments))))

(defun lsp-docker--run-external-command (command)
  "Run a command and get its output and exit code"
  (-let ((
          (command-program . command-arguments)
          (lsp-docker--decode-single-quoted-tokens (s-split " " (lsp-docker--encode-single-quoted-parameters command)))))
    (progn
      (lsp-docker--conditionally-log-docker-supplemental-call command-program command-arguments)
      (lsp-docker--launch-command-internal command-program command-arguments))))

(defun lsp-docker--launch-command-internal (command-program command-arguments)
  "Run a command using 'call-process' function and return a pair of exit code and raw output"
  (with-temp-buffer
    (cons
     (apply #'call-process command-program nil (current-buffer) nil command-arguments)
     (buffer-string))))

(defun lsp-docker--conditionally-log-docker-supplemental-call (command-program command-arguments)
  "Log a command into a buffer set in lsp-docker settings group"
  (if (lsp-docker--log-docker-supplemental-calls-p)
      (with-current-buffer (get-buffer-create lsp-docker-log-docker-supplemental-calls-buffer-name)
        (goto-char (point-max))
        (insert (format "LOG: calling %s %s\n" command-program (s-join " " command-arguments))))))

(defun lsp-docker--get-existing-images ()
  "Get available docker images already existing on the host"
  (-let ((
          (exit-code . raw-output)
          (lsp-docker--run-docker-command "image list --format '{{.Repository}}:{{.Tag}}'")))
    (if (equal exit-code 0)
        ;; filter out the list of tagged images from cmd output
        (--remove (s-blank? it) (--map (s-chop-suffix "'" (s-chop-prefix "'" it)) (s-lines raw-output)))
      (user-error "Cannot get the existing images list from the host, exit code: %d" exit-code))))

(defun lsp-docker--get-existing-containers ()
  "Get available docker images already existing on the host"
  (-let ((
          (exit-code . raw-output)
          (lsp-docker--run-docker-command "container list --all --format '{{.Names}}'")))
    (if (equal exit-code 0)
        (--remove (s-blank? it) (--map (s-chop-suffix "'" (s-chop-prefix "'" it)) (s-lines raw-output)))
      (user-error "Cannot get the existing containers list from the host, exit code: %d" exit-code))))

(defun lsp-docker--check-image-exists (image-name)
  "Check that the specified image already exists on the host"
  ;; automatically add "latest" tag when `image-name' is an untagged image name
  (let ((target-image (if (not (string-match "[:]" image-name))
                          (format "%s:latest" image-name)
                        image-name)))
    (--any? (s-equals? it target-image) (lsp-docker--get-existing-images))))

(defun lsp-docker--check-container-exists (container-name)
  "Check that the specified container already exists on the host"
  (--any? (s-equals? it container-name) (lsp-docker--get-existing-containers)))

(defun lsp-docker--generate-build-buffer-name (image-name dockerfile-path)
  "Generate a buffer name used when building the specified image"
  (let ((image-part image-name)
        (dockerfile-path-part (s-chop-prefix "-" (s-replace-all '(("/" . "-") ("." . "")) dockerfile-path))))
    (s-join "-" (list image-part dockerfile-path-part "build"))))

(defun lsp-docker--build-image-if-necessary (image-name dockerfile-path)
  "Check that the specified image exists, otherwise build it (if possible)"
  (unless (lsp-docker--check-image-exists image-name)
    (if dockerfile-path
        (if (y-or-n-p (format "Image %s is missing but can be built (Dockerfile was found), do you want to build it?" image-name))
            (let ((build-buffer-name (lsp-docker--generate-build-buffer-name image-name dockerfile-path)))
              (lsp-docker--run-image-build image-name dockerfile-path build-buffer-name))
          (user-error "Cannot register a server with a missing image!"))
      (user-error "Cannot find the image %s but cannot build it too (missing Dockerfile)" image-name))))

(defun lsp-docker--create-building-process-sentinel (
                                                     server-id
                                                     docker-server-id
                                                     path-mappings
                                                     launch-parameters
                                                     image-name
                                                     docker-container-name
                                                     activation-fn
                                                     server-command)
  `(lambda (proc _message)
     (when (eq (process-status proc) 'exit)
       (lsp-docker-register-client-with-activation-fn
        :server-id ',server-id
        :docker-server-id ',docker-server-id
        :path-mappings ',path-mappings
        :launch-parameters ,launch-parameters
        :docker-image-id ',image-name
        :docker-container-name ',docker-container-name
        :activation-fn ,activation-fn
        :priority lsp-docker-default-priority
        :server-command ',server-command
        :launch-server-cmd-fn #'lsp-docker-launch-new-container))))

(cl-defun lsp-docker--build-image-and-register-server-async (&key image-name
                                                                  dockerfile-path
                                                                  server-id
                                                                  docker-server-id
                                                                  path-mappings
                                                                  launch-parameters
                                                                  docker-container-name
                                                                  activation-fn
                                                                  server-command
                                                                  ;; TODO: keep these inputs for future feature
                                                                  ;; implementation, see
                                                                  ;; https://github.com/sfavazza/lsp-docker/pull/1#discussion_r1367081991
                                                                  ;; project-root
                                                                  ;; docker-image-id
                                                                  ;; priority
                                                                  ;; launch-server-cmd-fn
                                                                  )
  "Build an image asynchronously and register it afterwards"
  (unless (lsp-docker--check-image-exists image-name) ;; Check again whether we have to build a new image
    (if dockerfile-path
        (if (y-or-n-p (format "Image %s is missing but can be built (Dockerfile was found), do you want to build it?" image-name))
            (let* ((build-buffer-name (lsp-docker--generate-build-buffer-name image-name dockerfile-path))
                   (build-command (lsp-docker--get-build-command image-name dockerfile-path))
                   (build-command-decoded (lsp-docker--decode-single-quoted-tokens (s-split " " (lsp-docker--encode-single-quoted-parameters build-command)))))
              (with-current-buffer (get-buffer-create build-buffer-name)
                (lsp-docker--conditionally-log-docker-supplemental-call (car build-command-decoded) (cdr build-command-decoded))
                (message "Building the image %s, please open the %s buffer for details" image-name build-buffer-name)
                (make-process
                 :name "lsp-docker-build"
                 :buffer (current-buffer)
                 :command build-command-decoded
                 :sentinel (lsp-docker--create-building-process-sentinel
                            server-id
                            docker-server-id
                            path-mappings
                            launch-parameters
                            image-name
                            docker-container-name
                            activation-fn
                            server-command))))
          (user-error "Cannot register a server with a missing image!"))
      (user-error "Cannot find the image %s but cannot build it too (missing Dockerfile)" image-name))))

(cl-defun lsp-docker-register-client-with-activation-fn (&key server-id
                                                              docker-server-id
                                                              path-mappings
                                                              launch-parameters
                                                              docker-image-id
                                                              docker-container-name
                                                              activation-fn
                                                              priority
                                                              server-command
                                                              launch-server-cmd-fn)
  "Registers docker clients with lsp (by persisting configuration)"
  (if-let ((client (copy-lsp--client (gethash server-id lsp-clients))))
      (progn
        (setf (lsp--client-server-id client) docker-server-id
              (lsp--client-uri->path-fn client) (-partial #'lsp-docker--uri->path
                                                          path-mappings
                                                          docker-container-name)
              (lsp--client-activation-fn client) activation-fn
              (lsp--client-path->uri-fn client) (-partial #'lsp-docker--path->uri path-mappings)
              (lsp--client-new-connection client) (plist-put
                                                   (lsp-stdio-connection
                                                    (lambda ()
                                                      (funcall (or launch-server-cmd-fn #'lsp-docker-launch-new-container)
                                                               docker-container-name
                                                               path-mappings
                                                               launch-parameters
                                                               docker-image-id
                                                               server-command)))
                                                   :test? (lambda (&rest _)
                                                            t))
              (lsp--client-priority client) (or priority (lsp--client-priority client)))
        (lsp-register-client client)
        (message "Registered a language server with id: %s and container name: %s" docker-server-id docker-container-name))
  (user-error "No such client %s" server-id)))

(defun lsp-docker--register-single-server (server-config project-root path-mappings)
  "Register a single dockerized language server.

Its description is provided via the SERVER-CONFIG hash-table. It
must represents the fields defined under the `server' (single
server configuration) or `multi-server/<dockerized-server-name>'
(multi-server configuration) node. The PROJECT-ROOT must be a
path pointing to the top-level folder of the project the
configuration file resides into. The PATH-MAPPINGS provides a
hash-table to translate the paths between the host and the
dockerized server."
  (let* ((server-type-subtype (lsp-docker-get-server-type-subtype server-config))
         (config-specified-server-container-name (lsp-docker-get-server-container-name server-config))
         (server-image-name (lsp-docker-get-server-image-name server-config))
         (regular-server-id (lsp-docker-get-server-id server-config))
         (server-id (lsp-docker-generate-docker-server-id server-config (lsp-workspace-root)))
         (server-launch-parameters (lsp-docker--get-server-launch-parameters server-config))
         (server-launch-command (lsp-docker-get-launch-command server-config))
         (base-client (lsp-docker--get-base-client regular-server-id))
         (activation-fn (lsp-docker--create-activation-function-by-project-dir-and-base-client
                         (lsp-workspace-root)
                         base-client))
         (server-container-name (lsp-docker--finalize-docker-server-container-name
                                 config-specified-server-container-name server-config project-root)))

    (if (and (lsp-docker-check-server-type-subtype lsp-docker-supported-server-types-subtypes
                                                   server-type-subtype)
             (lsp-docker-check-path-mappings path-mappings))
        (let ((container-type (car server-type-subtype))
              (container-subtype (cdr server-type-subtype)))
          (pcase container-type
            ('docker (pcase container-subtype
                       ('image (if (lsp-docker--check-image-exists server-image-name)
                                   (lsp-docker-register-client-with-activation-fn
                                    :server-id regular-server-id
                                    :docker-server-id server-id
                                    :path-mappings path-mappings
                                    :launch-parameters server-launch-parameters
                                    :docker-image-id server-image-name
                                    :docker-container-name server-container-name
                                    :activation-fn activation-fn
                                    :priority lsp-docker-default-priority
                                    :server-command server-launch-command
                                    :launch-server-cmd-fn #'lsp-docker-launch-new-container)
                                 (lsp-docker--build-image-and-register-server-async
                                  :image-name server-image-name
                                  :dockerfile-path (lsp-docker--find-project-dockerfile-from-lsp)
                                  :server-id regular-server-id
                                  :docker-server-id server-id
                                  :path-mappings path-mappings
                                  :launch-parameters server-launch-parameters
                                  :docker-container-name server-container-name
                                  :activation-fn activation-fn
                                  :server-command server-launch-command)))
                       ('container (if (lsp-docker--check-container-exists server-container-name)
                                       (lsp-docker-register-client-with-activation-fn
                                        :server-id regular-server-id
                                        :docker-server-id server-id
                                        :path-mappings path-mappings
                                        :launch-parameters nil
                                        :docker-image-id nil
                                        :docker-container-name server-container-name
                                        :activation-fn activation-fn
                                        :priority lsp-docker-default-priority
                                        :server-command server-launch-command
                                        :launch-server-cmd-fn #'lsp-docker-launch-existing-container)
                                     (user-error "Invalid LSP docker config: cannot find the specified container: %s" server-container-name)))
                       (user-error "Invalid LSP docker config: unsupported server type and/or subtype")))
            (user-error "Invalid LSP docker config: unsupported server type and/or subtype")))
      (user-error "Language server registration failed, check input parameters"))))

(defun lsp-docker-register ()
  "Register one or more dockerized language servers for the current project"
  (interactive)
  (if (lsp-workspace-root)
      (let* ((config (lsp-docker-get-config-from-lsp))
             (project-root (lsp-workspace-root))
             (path-mappings (lsp-docker-get-path-mappings config (lsp-workspace-root)))
             (server-config (gethash lsp-docker--server-key config)))

        ;; check whether a single or multiple servers are described in the configuration
        (cond
         ((vectorp server-config)
          (message "registering multiple servers")
          ;; NOTE: if multiple language server descriptions share the same name "server" field, the latest entry
          ;; will be enforced.
          (--map (lsp-docker--register-single-server it
                                                     project-root
                                                     path-mappings)
                 server-config))
         (server-config
          (message "registering a single server")
          (lsp-docker--register-single-server server-config
                                              project-root
                                              path-mappings))
         (t
          (user-error "no `%s' node found in configuration, see README for reference"
                      lsp-docker--server-key))))
   (user-error
     (format (concat "Current file: %s is not in a registered project! "
                     "Try adding your project with `lsp-workspace-folders-add'")
      (buffer-file-name)))))

(defun lsp-docker-start ()
  "Register and launch a server to use LSP mode in a container for the current project"
  (interactive)
  (lsp-docker-register)
  (lsp))

(provide 'lsp-docker)
;;; lsp-docker.el ends here
