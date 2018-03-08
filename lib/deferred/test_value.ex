defmodule Deferred.TestValue do
  defstruct evaluated?: false, value: nil, callback: nil

  defimpl DeferredValue do
    def get_value(%Deferred.TestValue{value: value, evaluated?: true}), do: value

    def evaluate_once(val, prev \\ nil)

    def evaluate_once(val = %Deferred.TestValue{evaluated?: true}, _), do: val

    def evaluate_once(%Deferred.TestValue{callback: callback}, prev), do: callback.(prev)

    def evaluate_once(other, _) do
      other
    end

    def evaluate(val, prev \\ nil)

    def evaluate(val = %Deferred.TestValue{}, prev) do
      new_val = evaluate_once(val, prev)
      evaluate(new_val, val)
    end

    def evaluate(val, _), do: val

    def add_then(val = %{callback: nil}, callback) do
      %{val | callback: callback}
    end

    def add_then(val = %{callback: previous_callback}, callback) do
      %Deferred.TestValue{
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