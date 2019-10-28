use Mix.Config

# Configure your database
config :hd_backend, HdBackend.Repo,
  username: "headsdown_user",
  password: "password",
  database: "hd_backend_test",
  hostname: "127.0.0.1",
  pool: Ecto.Adapters.SQL.Sandbox

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :hd_backend, HdBackendWeb.Endpoint,
  http: [port: 4002],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn
