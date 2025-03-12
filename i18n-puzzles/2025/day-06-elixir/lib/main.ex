# Disable the "redefining module" warning that `mix run` prints for some reason
Code.put_compiler_option(:ignore_module_conflict, true)

defmodule Main do
  use Application

  def start(_type, _args) do
    case System.argv() |> List.first() do
      # When "mix test" is run, Main.start/2 gets called, and "test" becomes the
      # first argument. In this case, we don't want to run the actual code, just
      # the tests.
      "test" ->
        :ok

      filename ->
        case File.read(filename) do
          {:ok, content} ->
            I18n.start(content) |> IO.puts()

          {:error, reason} ->
            IO.puts("Error reading input file \"#{filename}\" (#{reason})")
            System.halt(1)
        end
    end

    # Elixir is usually used for long-running services, therefore the start
    # function of an Application needs to return either a Task or a Supervisor.
    # See: https://stackoverflow.com/a/30688873/183582
    Supervisor.start_link([], strategy: :one_for_one)
  end
end
