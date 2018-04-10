defmodule Defer.ExampleDeferrable do
  alias __MODULE__
  defstruct callback: nil

  def new(_opts \\ []) do
    %ExampleDeferrable{}
  end

  defimpl Deferrable do
    def run_once(val, context \\ [])
    def run_once(%{callback: nil}, context), do: {nil, context}
    def run_once(%{callback: callback}, context), do: {callback.(context[:prev]), context}

    def run(val, context) do
      {deferrable, context} = Deferrable.run_once(val, context)
      Deferrable.run(deferrable, Keyword.put(context, :prev, val))
    end

    def then(val, callback)

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

    def deferrable?(_), do: true
  end
end
