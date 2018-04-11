defprotocol Deferrable do
  @fallback_to_any true

  @spec run(t, list) :: {t, list}
  def run(deferrable, context \\ [])

  @spec run_once(t, list) :: {t, list}
  def run_once(deferrable, context \\ [])

  @spec then(t, callback: (any -> any)) :: t
  def then(deferrable, callback)

  @spec deferrable?(t) :: boolean
  def deferrable?(deferrable)
end

defmodule DeferredList do
  @moduledoc """
  This module is purely there to make a list thenable
  """
  defstruct list: nil, then: nil, context: nil

  def new(list) do
    %__MODULE__{
      list: list
    }
  end

  defimpl Deferrable do
    def run(deferred_list, context \\ [])

    def run(%{list: list, then: nil}, context) do
      Deferrable.run(list, context)
    end

    def run(%{list: list, then: then}, context) do
      {list, context} = Deferrable.run(list, context)
      result = then.(list)
      Deferrable.run(result, context)
    end

    def run_once(deferred_list, context \\ [])
    def run_once(%{list: list}, context), do: Deferrable.run_once(list, context)

    def then(deferred_list, callback)

    def then(deferred_list = %{then: nil}, callback) do
      %{
        deferred_list
        | then: callback
      }
    end

    def then(deferred_list = %{then: old_callback}, new_callback) do
      %{
        deferred_list
        | then: fn prev ->
            then(old_callback.(prev), new_callback)
          end
      }
    end

    def deferrable?(_), do: true
  end
end

defimpl Deferrable, for: List do
  defp module_from_list(list = [%{__struct__: module} | _]) do
    if Enum.any?(list, fn
         %{__struct__: m} -> m != module
         _ -> false
       end) do
      raise("Cannot work with lists of mixed deferrables yet")
    end

    module
  end

  defp module_from_list(_), do: nil

  def run(list, context \\ [])
  def run([], _), do: []

  def run(list, context) do
    module = module_from_list(list)

    if module && Defer.deferrable?(struct(module)) do
      module.run(list, context)
    else
      {list, context}
    end
  end

  def run_once(list, context \\ [])
  def run_once([], _), do: []

  def run_once(list, context) do
    module = module_from_list(list)

    if list && Defer.deferrable?(struct(module)) do
      module.run_once(list, context)
    else
      {list, context}
    end
  end

  def then(list, callback)

  def then(list, callback) do
    DeferredList.new(list) |> Defer.then(callback)
  end

  def deferrable?(list) do
    Enum.any?(list, &Defer.deferrable?(&1))
  end
end

defimpl Deferrable, for: Any do
  def run(val, context \\ [])
  def run(val, context), do: {val, context}

  def run_once(val, context \\ [])
  def run_once(val, context), do: {val, context}

  def then(val, callback)
  def then(val, callback) when is_function(callback, 1), do: callback.(val)
  def then(_, _), do: raise("Not a valid callback provided")

  def deferrable?(_), do: false
end

defmodule Defer do
  defp get_await(expression = {_, _, _}, deep, env) do
    do_get_await(Macro.expand(expression, env), deep, env)
  end

  defp get_await(expression, deep, env), do: do_get_await(expression, deep, env)

  defp do_get_await(expression, deep, env)
  defp do_get_await(expr = {:await, _ctx, _args}, _, _env), do: expr
  defp do_get_await({:__block__, _, _}, false, _env), do: nil

  defp do_get_await({:case, _ctx, _args}, false, _env) do
    nil
  end

  defp do_get_await([], _, _env), do: nil

  defp do_get_await([arg | args], deep, env),
    do: get_await(arg, deep, env) || get_await(args, deep, env)

  defp do_get_await({_fun, _ctx, args}, deep, env) when is_list(args),
    do: get_await(args, deep, env)

  defp do_get_await({:do, block}, deep, env), do: get_await(block, deep, env)
  defp do_get_await(_expr, _, _env), do: nil

  defp replace_expression(await, await, replacement), do: replacement

  defp replace_expression({:do, block}, await, replacement),
    do: {:do, replace_expression(block, await, replacement)}

  defp replace_expression({:->, _ctx, [_pattern, block]}, await, replacement),
    do: {:do, replace_expression(block, await, replacement)}

  defp replace_expression({fun, ctx, args}, await, replacement),
    do: {fun, ctx, replace_expression(args, await, replacement)}

  defp replace_expression([arg | args], await, replacement),
    do: [
      replace_expression(arg, await, replacement) | replace_expression(args, await, replacement)
    ]

  defp replace_expression(other, _, _), do: other

  defp md5_hash(binary) do
    :crypto.hash(:md5, binary)
  end

  # stable variable name from an ast
  defp ast_to_hash(ast) do
    ast
    |> Macro.to_string()
    |> :erlang.term_to_binary()
    |> md5_hash
    |> Base.encode16()
    |> String.downcase()
    |> String.slice(0, 5)
  end

  defp expression_to_var_name(expr) do
    :"defer_#{ast_to_hash(expr)}"
  end

  defp wrap_with([clause | clauses], await, env, []) do
    {:await, _, [deferred_val]} = await

    var = Macro.var(expression_to_var_name(deferred_val), nil)
    substituted_clause = replace_expression(clause, await, var)

    quote do
      then(unquote(deferred_val), fn unquote(var) ->
        unquote(rewrite_with_clauses([substituted_clause | clauses], env))
      end)
    end
  end

  defp wrap_with([clause | clauses], await, env, coll) do
    {:await, _, [deferred_val]} = await

    var = Macro.var(expression_to_var_name(deferred_val), nil)
    substituted_clause = replace_expression(clause, await, var)

    quote do
      with(unquote_splicing(coll)) do
        then(unquote(deferred_val), fn unquote(var) ->
          unquote(rewrite_with_clauses([substituted_clause | clauses], env))
        end)
      end
    end
  end

  defp wrap(entry, await, env) do
    {:await, _, [deferred_val]} = await
    var = Macro.var(expression_to_var_name(deferred_val), nil)

    entry = entry |> replace_expression(await, var) |> rewrite_fun(env)

    quote do
      then(unquote(deferred_val), fn unquote(var) ->
        unquote(entry)
      end)
    end
  end

  # a await expression has been found in the current line,
  # and it can be directly wrapped
  defp wrap_lines([line | lines], await, env) do
    {:await, _, [deferred_val]} = await

    var = Macro.var(expression_to_var_name(deferred_val), nil)

    lines =
      [line | lines]
      |> replace_expression(await, var)
      |> rewrite_lines(env)

    quote do
      then(unquote(deferred_val), fn unquote(var) ->
        (unquote_splicing(lines))
      end)
    end
  end

  defp remove_binding({:=, _ctx, [_binding, expr]}), do: expr
  defp remove_binding(other), do: other

  defp to_wrapped_line({:=, ctx, [binding, _expr]}, var) do
    {:=, ctx, [binding, var]}
  end

  defp to_wrapped_line(_line, var), do: var

  defp wrap_lines_deep([line | lines], _await, env) do
    line = rewrite(line, env)
    var = Macro.var(expression_to_var_name(line), nil)
    lines = rewrite_lines([to_wrapped_line(line, var) | lines], env)

    quote do
      then(unquote(remove_binding(line)), fn unquote(var) ->
        (unquote_splicing(lines))
      end)
    end
  end

  defp do_rewrite_lines(await, await_deep, lines, env)

  defp do_rewrite_lines(nil, nil, [line | lines], env),
    do: [rewrite(line, env) | rewrite_lines(lines, env)]

  defp do_rewrite_lines(nil, await_deep, [line | lines], env),
    do: [wrap_lines_deep([line | lines], await_deep, env)]

  defp do_rewrite_lines(await, _, [line | lines], env),
    do: [wrap_lines([line | lines], await, env)]

  defp rewrite_lines([], _), do: []

  # single line or last line
  defp rewrite_lines([line], env) do
    [rewrite(line, env)]
  end

  defp rewrite_lines([line | lines], env) do
    await_shallow = get_await(line, false, env)
    await_deep = get_await(line, true, env)

    do_rewrite_lines(await_shallow, await_deep, [line | lines], env)
  end

  defp rewrite_matches([], _env), do: []

  defp rewrite_matches([{:->, ctx, [pattern, block]} | matches], env) do
    [{:->, ctx, [pattern, rewrite(block, env)]} | rewrite_matches(matches, env)]
  end

  defp do_rewrite_with_clauses(await, await_deep, clauses, env, coll)

  defp do_rewrite_with_clauses(nil, nil, [clause | clauses], env, coll),
    do: rewrite_with_clauses(clauses, env, coll ++ [rewrite(clause, env)])

  defp do_rewrite_with_clauses(nil, _await, [_clause | _clauses], _env, _coll),
    do: raise("Wrap deep not yet implemented for with statement")

  defp do_rewrite_with_clauses(await, _, [clause | clauses], env, coll),
    do: wrap_with([clause | clauses], await, env, coll)

  defp rewrite_with_clauses(clauses, env, coll \\ [])
  defp rewrite_with_clauses([], env, [{:do, do_block}]), do: rewrite(do_block, env)
  defp rewrite_with_clauses([], _env, coll), do: {:with, [], coll}

  defp rewrite_with_clauses([{:do, do_block} | other_clauses], env, coll) do
    rewrite_with_clauses([], env, coll ++ [{:do, rewrite(do_block, env)} | other_clauses])
  end

  defp rewrite_with_clauses([clause | clauses], env, coll) do
    await_shallow = get_await(clause, false, env)
    await_deep = get_await(clause, true, env)
    do_rewrite_with_clauses(await_shallow, await_deep, [clause | clauses], env, coll)
  end

  defp rewrite({:__block__, ctx, lines}, env) do
    {:__block__, ctx, rewrite_lines(lines, env)}
  end

  defp rewrite({:with, _ctx, clauses}, env) do
    rewrite_with_clauses(clauses, env)
  end

  defp rewrite({:case, ctx, [test, [do: lines]]}, env) do
    {:case, ctx, [rewrite(test, env), [do: rewrite_matches(lines, env)]]}
  end

  defp rewrite({:->, env, lines}, env) when is_list(lines), do: rewrite_lines(lines, env)
  defp rewrite({:->, env, lines}, env), do: rewrite(lines, env)
  defp rewrite([{:do, block}], env), do: [{:do, rewrite(block, env)}]

  defp rewrite({:await, _, [expr]}, _env) do
    expr
  end

  defp rewrite(expression = {form, ctx, args}, env) do
    expanded_expression = Macro.expand(expression, env)

    if expanded_expression == expression do
      rewrite_fun({form, ctx, args}, env)
    else
      rewrite(expanded_expression, env)
    end
  end

  defp rewrite(expr, _env), do: expr

  def rewrite(
        {:def, fun_ctx, [fun_name]},
        [do: do_block],
        env
      ) do
    {:def, fun_ctx, [fun_name, [do: rewrite(do_block, env)]]}
  end

  defp do_rewrite_fun(nil, {fun, ctx, args}, env) when is_list(args),
    do: {fun, ctx, rewrite_args(args, env)}

  defp do_rewrite_fun(nil, expr, _), do: expr

  defp do_rewrite_fun(await, expr, env) do
    wrap(expr, await, env)
  end

  defp rewrite_fun(expr, env) do
    await_shallow = get_await(expr, false, env)
    do_rewrite_fun(await_shallow, expr, env)
  end

  defp rewrite_args([], _env), do: []

  defp rewrite_args([arg | args], env) do
    [rewrite(arg, env) | rewrite_args(args, env)]
  end

  defmacro defer(
             definition,
             do_block
           ) do
    rewrite(definition, do_block, __CALLER__)
  end

  def then(deferred_value, func) do
    Deferrable.then(
      deferred_value,
      func
    )
  end

  def run_once(val, context \\ []) do
    {val, _context} = Deferrable.run_once(val, context)
    val
  end

  def run(val, context \\ []) do
    {val, _context} = Deferrable.run(val, context)
    val
  end

  def deferrable?(val), do: Deferrable.deferrable?(val)
end
