defprotocol Deferrable do
  @fallback_to_any true

  @spec evaluate(t, list) :: t
  def evaluate(deferrable, opts \\ [])

  @spec evaluate_once(t, list) :: t
  def evaluate_once(deferrable, opts \\ [])

  @spec get_value(t, list) :: any | nil
  def get_value(deferrable, opts \\ [])

  @spec then(t, callback: (any -> any)) :: t
  def then(deferrable, callback)
end

defimpl Deferrable, for: Any do
  defp module_from_list(list) do
    [%{__struct__: module} | _] = list

    if Enum.any?(list, fn %{__struct__: m} -> m != module end),
      do: raise("Cannot evaluated mixed deferrables")

    module
  end

  def new(_opts \\ []), do: nil

  def evaluate(val, opts \\ [])

  def evaluate(val, opts) when is_list(val) do
    module_from_list(val).evaluate(val, opts)
  end

  def evaluate(val, _opts), do: val

  def evaluate_once(val, opts \\ [])

  def evaluate_once(val, _opts) when is_list(val) do
    module_from_list(val).evaluate_once(val)
  end

  def evaluate_once(val, _opts), do: val

  def get_value(val, opts \\ [])

  def get_value(vals, opts) when is_list(vals) do
    module_from_list(vals).get_value(vals, opts)
  end

  def get_value(val, _opts), do: val

  def then(val, callback) when is_function(callback, 1), do: callback.(val)
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

  defp replace_expression({fun, ctx, args}, await, replacement),
    do: {fun, ctx, replace_expression(args, await, replacement)}

  defp replace_expression({:do, block}, await, replacement),
    do: {:do, replace_expression(block, await, replacement)}

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
    substituted_clause = substitute(clause, await, var)

    quote do
      then(unquote(deferred_val), fn unquote(var) ->
        unquote(rewrite_with_clauses([substituted_clause | clauses], env))
      end)
    end
  end

  defp wrap_with([clause | clauses], await, env, coll) do
    {:await, _, [deferred_val]} = await

    var = Macro.var(expression_to_var_name(deferred_val), nil)
    substituted_clause = substitute(clause, await, var)

    quote do
      with(unquote_splicing(coll)) do
        then(unquote(deferred_val), fn unquote(var) ->
          unquote(rewrite_with_clauses([substituted_clause | clauses], env))
        end)
      end
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

  defp substitute(ast, find, replace, coll \\ [])

  defp substitute(find, find, replace, _) do
    replace
  end

  defp substitute({fun, ctx, args}, find, replace, _) do
    {fun, ctx, substitute(args, find, replace)}
  end

  defp substitute([find | args], find, replace, coll) do
    coll ++ [replace | args]
  end

  defp substitute([arg | args], find, replace, coll) do
    substitute(args, find, replace, coll ++ [arg])
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

  defp rewrite({:await, _, [fun]}, _env) do
    # just remove the keyword
    fun
  end

  defp rewrite(expression = {form, ctx, args}, env) when is_list(args) do
    expanded_expression = Macro.expand(expression, env)

    if expanded_expression == expression do
      {form, ctx, rewrite_args(args, env)}
    else
      rewrite(expanded_expression, env)
    end
  end

  defp rewrite(expression = {form, ctx, args}, env) do
    expanded_expression = Macro.expand(expression, env)

    if expanded_expression == expression do
      {form, ctx, args}
    else
      rewrite(expanded_expression, env)
    end
  end

  defp rewrite(exp, _env) do
    exp
  end

  defp rewrite_args([], _env), do: []

  defp rewrite_args([arg | args], env) do
    [rewrite(arg, env) | rewrite_args(args, env)]
  end

  def rewrite(
        {:def, fun_ctx, [fun_name]},
        do_block,
        env
      ) do
    {:def, fun_ctx, [fun_name, rewrite(do_block, env)]}
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

  def evaluate_once(val, opts \\ []) do
    Deferrable.evaluate_once(val, opts)
  end

  def evaluate(val, opts \\ []) do
    Deferrable.evaluate(val, opts)
  end

  def get_value(deferred_value, opts \\ []), do: Deferrable.get_value(deferred_value, opts)
end
