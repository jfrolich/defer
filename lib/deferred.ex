defmodule Deferred do
  @moduledoc """
  Documentation for Deferred.
  """

  defp add_then(deferred_value, left_side, rest) do
    rest = rewrite_await(rest)

    quote do
      then(unquote(deferred_value), fn prev ->
        unquote(left_side) = prev
        unquote(rest)
      end)
    end
  end

  # recursively rewrite await statements
  def rewrite_await([
        {:=, _context_eq,
         [
           left_side,
           {:await, _await_context, [deferred_value]}
         ]}
        | rest
      ]) do
    add_then(deferred_value, left_side, rest)
  end

  def rewrite_await([line]), do: line

  def rewrite_await([line | other_lines]) do
    [line | rewrite_await(other_lines)]
  end

  # a single line with an await invocation
  defmacro deferred(
             {:def, fun_ctx, [fun_name]},
             do: await = {:await, _, [_]}
           ) do
    new_func = {:def, fun_ctx, [fun_name, [do: rewrite_await([await])]]}

    quote do
      unquote(new_func)
    end
  end

  # multiple lines
  defmacro deferred(
             {:def, fun_ctx, [fun_name]},
             do: {:__block__, context_do, def_contents}
           ) do
    new_func =
      {:def, fun_ctx, [fun_name, [do: {:__block__, context_do, [rewrite_await(def_contents)]}]]}

    IO.puts("\nNew Function: \n")
    IO.puts(Macro.to_string(new_func))

    quote do
      unquote(new_func)
    end
  end

  defmacro deferred(_, _) do
    raise "For some reason we cannot handle this deferred function, please file an issue with the defer function."
  end

  defmacro await(deferred_value) do
    quote do
      unquote(deferred_value)
    end
  end

  defmacro then(deferred_value, func) do
    quote do
      Deferred.Value.add_then(
        unquote(deferred_value),
        unquote(func)
      )
    end
  end
end