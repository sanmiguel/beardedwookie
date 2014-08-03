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
      applications: [:cowboy],
      mod: { :bw_app, [] } ]
  end

  defp deps do
    [
      {:cowboy, github: "extend/cowboy", tag: "0.9.0"},
      {:exrm, "~> 0.14.0"}]
  end
end
