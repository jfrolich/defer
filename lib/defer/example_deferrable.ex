defmodule Defer.ExampleDeferrable do
  alias __MODULE__
  defstruct evaluated?: false, value: nil, callback: nil

  def new(_opts \\ []) do
    %ExampleDeferrable{}
  end

  defimpl Deferrable do
    def get_value(val, opts \\ [])
    def get_value(%ExampleDeferrable{value: value, evaluated?: true}, _opts), do: value
    def get_value(val, opts), do: run(val, opts) |> get_value(opts)

    def run_once(val, opts \\ [])

    def run_once(val = %ExampleDeferrable{evaluated?: true}, _), do: val

    def run_once(%ExampleDeferrable{callback: callback}, opts), do: callback.(opts[:prev])

    def run(val = %{evaluated?: true}), do: val

    def run(val, opts) do
      val
      |> Deferrable.run_once(opts[:prev])
      |> Deferrable.run([[prev: val] | opts])
    end

    def then(val = %{callback: nil}, callback) do
      %{val | callback: callback}
    end

    def then(val = %{callback: previous_callback}, callback) do
      %ExampleDeferrable{
        val
        | callback: fn prev ->
            then(previous_callback.(prev), callback)
          end
      }
    end

    def then(val, callback) do
      callback.(val)
    end
  end
end
