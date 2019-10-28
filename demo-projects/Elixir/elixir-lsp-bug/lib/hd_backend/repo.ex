defmodule HdBackend.Repo do
  use Ecto.Repo,
    otp_app: :hd_backend,
    adapter: Ecto.Adapters.Postgres
end
