defmodule BeardedWookie.Mixfile do
  use Mix.Project

  def project do
    [elixir: "~> 0.15",
      default_task: "compile",
      app: :beardedwookie,
      version: "0.0.1",
      deps: deps
      ]
  end

  def application do
    [ registered: [],
      applications: [:cowboy, :riakc_poolboy],
      mod: { :bw_app, [] } ]
  end

  defp deps do
    [
      {:meck, github: "eproxus/meck", tag: "0.8.2", override: true},
      {:riakc_poolboy, github: "puzza007/riakc_poolboy", branch: "master"},
      {:cowboy, github: "extend/cowboy", tag: "1.0.0"},
      {:exrm, "~> 0.14.0"}]
  end
end
