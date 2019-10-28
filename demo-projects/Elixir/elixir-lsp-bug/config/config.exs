# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :hd_backend,
  ecto_repos: [HdBackend.Repo]

# Configures the endpoint
config :hd_backend, HdBackendWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "UCHU/AE22R1q8fuG/IQy3fqjIb7zyJa1W8V9J5N506C1lPD05mTtZPFFORHVDKkp",
  render_errors: [view: HdBackendWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: HdBackend.PubSub, adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
