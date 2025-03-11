defmodule I18n.MixProject do
  use Mix.Project

  def project do
    [
      app: :i18n,
      version: "0.1.0",
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {Main, []},
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:datix, "~> 0.3.2"},
      {:tz, "~> 0.28"}
    ]
  end
end
