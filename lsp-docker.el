;;; lsp-docker.el --- LSP Docker integration         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; URL: https://github.com/emacs-lsp/lsp-docker
;; Keywords: languages langserver
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (dash "2.14.1") (lsp-mode "6.2.1") (f "0.20.0") (yaml "0.2.0") (ht "2.0"))


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
(require 'yaml)
(require 'ht)

(defun lsp-docker--uri->path (path-mappings docker-container-name uri)
  "Turn docker URI into host path.
Argument PATH-MAPPINGS dotted pair of (host-path . container-path).
Argument DOCKER-CONTAINER-NAME name to use when running container.
Argument URI the uri to translate."
  (let ((path (lsp--uri-to-path-1 uri)))
    (-if-let ((local . remote) (-first (-lambda ((_ . docker-path))
                                         (s-contains? docker-path path))
                                       path-mappings))
        (s-replace remote local path)
      (format "/docker:%s:%s" docker-container-name path))))

(defun lsp-docker--path->uri (path-mappings path)
  "Turn host PATH into docker uri.
Argument PATH-MAPPINGS dotted pair of (host-path . container-path).
Argument PATH the path to translate."
  (lsp--path-to-uri-1
   (-if-let ((local . remote) (-first (-lambda ((local-path . _))
                                        (s-contains? local-path path))
                                      path-mappings))
       (s-replace local remote path)
     (user-error "The path %s is not under path mappings" path))))


(defvar lsp-docker-container-name-suffix 0
  "Used to prevent collision of container names.")

(defvar lsp-docker-command "docker"
  "The docker command to use.")

(defun lsp-docker-launch-new-container (docker-container-name path-mappings docker-image-id server-command)
  "Return the docker command to be executed on host.
Argument DOCKER-CONTAINER-NAME name to use for container.
Argument PATH-MAPPINGS dotted pair of (host-path . container-path).
Argument DOCKER-IMAGE-ID the docker container to run language servers with.
Argument SERVER-COMMAND the language server command to run inside the container."
  (split-string
   (--doto (format "%s run --name %s --rm -i %s %s %s"
		   lsp-docker-command
		   docker-container-name
		   (->> path-mappings
			(-map (-lambda ((path . docker-path))
				(format "-v %s:%s" path docker-path)))
			(s-join " "))
		   docker-image-id
		   server-command))
   " "))

(defun lsp-docker-exec-in-container (docker-container-name server-command)
  "Return command to exec into running container.
Argument DOCKER-CONTAINER-NAME name of container to exec into.
Argument SERVER-COMMAND the command to execute inside the running container."
(split-string
   (format "docker exec -i %s %s" docker-container-name server-command)))

(cl-defun lsp-docker-register-client (&key server-id
                                           docker-server-id
                                           path-mappings
                                           docker-image-id
                                           docker-container-name
                                           priority
                                           server-command
                                           launch-server-cmd-fn)
  "Registers docker clients with lsp"
  (if-let ((client (copy-lsp--client (gethash server-id lsp-clients))))
      (progn
        (let ((docker-container-name-full
	       (if lsp-docker-container-name-suffix
		   (format "%s-%d"
			   docker-container-name
			   (if (numberp lsp-docker-container-name-suffix)
			       (cl-incf lsp-docker-container-name-suffix)
			     lsp-docker-container-name-suffix))
		 docker-container-name)))
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
    lsp-pyls)
  "Default list of client packages to load.")

(defvar lsp-docker-default-client-configs
  (list
   (list :server-id 'bash-ls :docker-server-id 'bashls-docker :server-command "bash-language-server start")
   (list :server-id 'clangd :docker-server-id 'clangd-docker :server-command "ccls")
   (list :server-id 'css-ls :docker-server-id 'cssls-docker :server-command "css-languageserver --stdio")
   (list :server-id 'dockerfile-ls :docker-server-id 'dockerfilels-docker :server-command "docker-langserver --stdio")
   (list :server-id 'gopls :docker-server-id 'gopls-docker :server-command "gopls")
   (list :server-id 'html-ls :docker-server-id 'htmls-docker :server-command "html-languageserver --stdio")
   (list :server-id 'pyls :docker-server-id 'pyls-docker :server-command "pyls")
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
         :launch-server-cmd-fn #'lsp-docker-launch-new-container))
      client-configs)))

(defvar lsp-docker-default-priority
  100
  "Default lsp-docker containerized servers priority (it needs to be bigger than default servers in order to override them)")

(defcustom lsp-docker-persistent-default-config
  (ht ('server (ht ('type "docker")
                   ('subtype "image")
                   ('name "emacslsp/lsp-docker-langservers")
                   ('server nil)
                   ('launch_command nil)))
      ('mappings [
                  (ht ('source ".")
                      ('destination "/projects"))
                  ]))
  "Default configuration for all language servers with persistent configurations"
  :type 'hash-table
  :group 'lsp-docker
)

(defun lsp-docker-get-config-from-project-config-file (project-config-file-path)
  "Get the LSP configuration based on a project configuration file"
  (if (f-exists? project-config-file-path)
      (if-let* ((whole-config (yaml-parse-string (f-read project-config-file-path)))
               (lsp-config (gethash 'lsp whole-config)))
          (ht-merge (ht-copy lsp-docker-persistent-default-config) lsp-config))))

(defun lsp-docker-get-config-from-lsp ()
  "Get the LSP configuration based on a project-local configuration (using lsp-mode)"
  (let ((project-config-file-path (if (f-exists? (f-join (lsp-workspace-root) ".lsp-docker.yml"))
                                       (f-join (lsp-workspace-root) ".lsp-docker.yml")
                                    (f-join (lsp-workspace-root) ".lsp-docker.yaml"))))
    (or (lsp-docker-get-config-from-project-config-file project-config-file-path)
        (ht-copy lsp-docker-persistent-default-config))))

(defvar lsp-docker-supported-server-types-subtypes
  (ht ('docker (list 'container 'image)))
  "A list of all supported server types and subtypes, currently only docker is supported")

(defun lsp-docker-get-server-type-subtype (config)
  "Get the server type"
  (let* ((lsp-server-info (gethash 'server config))
         (lsp-server-type (gethash 'type lsp-server-info))
         (lsp-server-subtype (gethash 'subtype lsp-server-info)))
    (cons (if (stringp lsp-server-type)
              (intern lsp-server-type)
            lsp-server-type)
          (if (stringp lsp-server-subtype)
              (intern lsp-server-subtype)
            lsp-server-subtype))))

(defun lsp-docker-get-server-container-name (config)
  "Get the server container name"
  (let* ((lsp-server-info (gethash 'server config))
         (lsp-server-subtype (gethash 'subtype lsp-server-info)))
    (if (equal lsp-server-subtype "container")
        (gethash 'name lsp-server-info))))

(defun lsp-docker-get-server-image-name (config)
  "Get the server image name"
  (let* ((lsp-server-info (gethash 'server config))
         (lsp-server-subtype (gethash 'subtype lsp-server-info)))
    (if (equal lsp-server-subtype "image")
        (gethash 'name lsp-server-info))))

(defun lsp-docker-get-server-id (config)
  "Get the server id"
  (let ((lsp-server-info (gethash 'server config)))
    (if (stringp (gethash 'server lsp-server-info))
        (intern (gethash 'server lsp-server-info))
      (gethash 'server lsp-server-info))))

(defun lsp-docker-get-path-mappings (config project-directory)
  "Get the server path mappings"
  (if-let ((lsp-mappings-info (gethash 'mappings config)))
      (--map (cons (f-canonical (f-expand (gethash 'source it) project-directory)) (gethash 'destination it)) lsp-mappings-info)
    (user-error "No path mappings specified!")))

(defun lsp-docker-get-launch-command (config)
  "Get the server launch command"
  (let ((lsp-server-info (gethash 'server config)))
    (gethash 'launch_command lsp-server-info)))

(defun lsp-docker-check-server-type-subtype (supported-server-types-subtypes server-type-subtype)
  "Verify that the combination of server (type . subtype) is supported by the current implementation"
  (if (not server-type-subtype)
      (user-error "No server type and subtype specified!"))
  (if (ht-find (lambda (type subtypes)
                 (let ((server-type (car server-type-subtype))
                       (server-subtype (cdr server-type-subtype)))
                   (if (and (equal server-type type) (-contains? subtypes server-subtype))
                       t)))
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
   (format "%s start -i %s"
           lsp-docker-command
           docker-container-name)
   " "))

(defmacro lsp-docker-create-activation-function-by-project-dir (project-dir)
  `(lambda (&rest unused)
     (let ((current-project-root (lsp-workspace-root))
           (registered-project-root ,project-dir))
       (f-same? current-project-root registered-project-root))))

(defun lsp-docker-generate-docker-server-id (config project-root)
  "Generate the docker-server-id from the project config"
  (let ((original-server-id (symbol-name (lsp-docker-get-server-id config)))
         (project-path-server-id-part (s-chop-prefix "-" (s-replace-all '(("/" . "-") ("." . "")) project-root))))
    (intern (s-join "-" (list project-path-server-id-part original-server-id "docker")))))

(cl-defun lsp-docker-register-client-with-activation-fn (&key server-id
                                                              docker-server-id
                                                              path-mappings
                                                              docker-image-id
                                                              docker-container-name
                                                              (docker-container-name-suffix lsp-docker-container-name-suffix)
                                                              activation-fn
                                                              priority
                                                              server-command
                                                              launch-server-cmd-fn)
  "Registers docker clients with lsp (by persisting configuration)"
  (if-let ((client (copy-lsp--client (gethash server-id lsp-clients))))
      (let ((docker-container-name-full
             (if docker-container-name-suffix
                 (format "%s-%d"
                         docker-container-name
                         (if (numberp docker-container-name-suffix)
                             (cl-incf docker-container-name-suffix)
                           docker-container-name-suffix))
               docker-container-name)))
        (setf (lsp--client-server-id client) docker-server-id
              (lsp--client-uri->path-fn client) (-partial #'lsp-docker--uri->path
                                                          path-mappings
                                                          docker-container-name-full)
              (lsp--client-activation-fn client) activation-fn
              (lsp--client-path->uri-fn client) (-partial #'lsp-docker--path->uri path-mappings)
              (lsp--client-new-connection client) (plist-put
                                                   (lsp-stdio-connection
                                                    (lambda ()
                                                      (funcall (or launch-server-cmd-fn #'lsp-docker-launch-new-container)
                                                               docker-container-name-full
                                                               path-mappings
                                                               docker-image-id
                                                               server-command)))
                                                   :test? (lambda (&rest _)
                                                            t))
              (lsp--client-priority client) (or priority (lsp--client-priority client)))
        (lsp-register-client client)
        (message "Registered a language server with id: %s and container name: %s" docker-server-id docker-container-name-full))
    (user-error "No such client %s" server-id)))

(defun lsp-docker-register ()
  "Register a server to use LSP mode in a container for the current project"
  (interactive)
  (if (lsp-workspace-root)
      (let* (
             (config (lsp-docker-get-config-from-lsp))
             (server-type-subtype (lsp-docker-get-server-type-subtype config))
             (server-container-name (lsp-docker-get-server-container-name config))
             (server-image-name (lsp-docker-get-server-image-name config))
             (path-mappings (lsp-docker-get-path-mappings config (lsp-workspace-root)))
             (regular-server-id (lsp-docker-get-server-id config))
             (server-id (lsp-docker-generate-docker-server-id config (lsp-workspace-root)))
             (server-launch-command (lsp-docker-get-launch-command config)))
        (if (and (lsp-docker-check-server-type-subtype lsp-docker-supported-server-types-subtypes server-type-subtype)
                 (lsp-docker-check-path-mappings path-mappings))
            (let ((container-type (car server-type-subtype))
                  (container-subtype (cdr server-type-subtype)))
                (pcase container-type
                  ('docker (pcase container-subtype
                             ('image (lsp-docker-register-client-with-activation-fn
                                      :server-id regular-server-id
                                      :docker-server-id server-id
                                      :path-mappings path-mappings
                                      :docker-image-id server-image-name
                                      :docker-container-name server-container-name
                                      :docker-container-name-suffix nil
                                      :activation-fn (lsp-docker-create-activation-function-by-project-dir (lsp-workspace-root))
                                      :priority lsp-docker-default-priority
                                      :server-command server-launch-command
                                      :launch-server-cmd-fn #'lsp-docker-launch-new-container))
                             ('container (lsp-docker-register-client-with-activation-fn
                                          :server-id regular-server-id
                                          :docker-server-id server-id
                                          :path-mappings path-mappings
                                          :docker-image-id nil
                                          :docker-container-name server-container-name
                                          :docker-container-name-suffix nil
                                          :activation-fn (lsp-docker-create-activation-function-by-project-dir (lsp-workspace-root))
                                          :priority lsp-docker-default-priority
                                          :server-command server-launch-command
                                          :launch-server-cmd-fn #'lsp-docker-launch-existing-container))))))
          (user-error "Invalid LSP docker config: unsupported server type and/or subtype")))
    (user-error (format "Current file: %s is not in a registered project!" (buffer-file-name)))))

(defun lsp-docker-start ()
  "Register and launch a server to use LSP mode in a container for the current project"
  (interactive)
  (lsp-docker-register)
  (lsp))

(provide 'lsp-docker)
;;; lsp-docker.el ends here
