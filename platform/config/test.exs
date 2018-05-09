use Mix.Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :platform, PlatformWeb.Endpoint,
  http: [port: 4001],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn

# Configure your database
config :platform, Platform.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "dfehr1",
  password: "admin",
  database: "Elixir-Elm-Practice-Test",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox

# Reduce bcrypt rounds to speed up tests
config :bcrypt_elixir, :log_rounds, 4
