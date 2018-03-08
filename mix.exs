defmodule Deferred.MixProject do
  use Mix.Project

  def project do
    [
      app: :defer,
      description: "Deferrables for Elixir",
      files: ["lib", "priv", "mix.exs", "README.md", "CHANGELOG.md", ".formatter.exs"],
      maintainers: [
        "Jaap Frolich"
      ],
      licenses: ["MIT"],
      links: %{github: "https://github.com/jfrolich/defer"},
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    []
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    []
  end
end