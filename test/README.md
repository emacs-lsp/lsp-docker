# Scope #

This folder contain the files to manually perform a manual test lsp-docker and support development.

# Howto #

The user shall have `projectile` installed such that this folder is recognized as a project.

To test the various scenarios follow the steps below:
```shell
# build the test image and create a container for a language server
docker compose up -d

# create a link to describe the configuration of the language server(s), choose one of the scenarios
rm -f .lsp-docker.yml
ln -s [multi-server|single-server-container|single-server-image].yml .lsp-docker.yml
```

In Emacs:
1. open one (or both) the example sources (`hellowold.[py|cpp]`)
1. add `test` folder to workspace project list via `lsp-workspace-folders-add`
1. register the language server(s) invoking `lsp-docker-register`
1. revert the opened buffer
1. check that the expected language server is used via `lsp-describe-session`

