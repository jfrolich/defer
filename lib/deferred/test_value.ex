defmodule Deferred.TestValue do
  defstruct evaluated?: false, value: nil, callback: nil, chained_callbacks: []

  defimpl Deferred.Value do
    def run_chained_callbacks(val, []), do: evaluate(val)

    def run_chained_callbacks(val, [callback | callbacks]) do
      evaluate(run_chained_callbacks(callback.(val), callbacks))
    end

    def get_value(%Deferred.TestValue{value: value, evaluated?: true}), do: value

    def evaluate(val = %Deferred.TestValue{evaluated?: true}), do: val

    def evaluate(%Deferred.TestValue{callback: callback, chained_callbacks: chained_callbacks}) do
      evaluate(run_chained_callbacks(callback.(), chained_callbacks))
    end

    def evaluate(other) do
      %Deferred.TestValue{evaluated?: true, value: other}
    end

    def add_then(val = %{}, func) do
      %{val | chained_callbacks: val.chained_callbacks ++ [func]}
    end
  end
end