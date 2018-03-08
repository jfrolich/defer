defmodule Defer.ExampleDeferredValue do
  defstruct evaluated?: false, value: nil, callback: nil

  defimpl Deferred do
    def get_value(%Defer.ExampleDeferredValue{value: value, evaluated?: true}), do: value

    def evaluate_once(val, prev \\ nil)

    def evaluate_once(val = %Defer.ExampleDeferredValue{evaluated?: true}, _), do: val

    def evaluate_once(%Defer.ExampleDeferredValue{callback: callback}, prev), do: callback.(prev)

    def evaluate_once(other, _) do
      other
    end

    def evaluate(val, prev \\ nil)

    def evaluate(val = %Defer.ExampleDeferredValue{}, prev) do
      new_val = evaluate_once(val, prev)
      evaluate(new_val, val)
    end

    def evaluate(val, _), do: val

    def add_then(val = %{callback: nil}, callback) do
      %{val | callback: callback}
    end

    def add_then(val = %{callback: previous_callback}, callback) do
      %Defer.ExampleDeferredValue{
        val
        | callback: fn prev ->
            add_then(previous_callback.(prev), callback)
          end
      }
    end

    def add_then(val, callback) do
      callback.(val)
    end
  end
end