defmodule DeferTest do
  use ExUnit.Case
  doctest Defer
  import Defer
  alias Defer.ExampleDeferrable

  defp do_rewrite(fun_ast) do
    {:def, def_ctx, [fun_name, do_block]} = fun_ast
    rewrite({:def, def_ctx, [fun_name]}, do_block, __ENV__)
  end

  def ast_to_string(ast) do
    "#{ast |> Macro.to_string() |> Code.format_string!()}"
  end

  defp assert_output(input, expected_output) do
    if ast_to_string(do_rewrite(input)) != ast_to_string(expected_output) do
      IO.puts("\n\nAssertion error:")
      IO.puts("\n\nInput:\n\n")
      IO.puts(ast_to_string(input))
      IO.puts("\n\nExpected:\n\n")
      IO.puts(ast_to_string(expected_output))
      # IO.inspect(expected_output)
      IO.puts("\n\nInstead got:\n\n")
      IO.puts(ast_to_string(do_rewrite(input)))
      # IO.inspect(do_rewrite(input))
    end

    assert ast_to_string(expected_output) == ast_to_string(do_rewrite(input))
  end

  test "then macro" do
    first_callback = fn _ ->
      1
    end

    second_callback = fn val ->
      val + 2
    end

    then_val =
      then(
        %ExampleDeferrable{
          callback: first_callback
        },
        second_callback
      )

    assert %ExampleDeferrable{} = then_val
  end

  defer def test do
    val_1 = await %ExampleDeferrable{callback: fn _ -> 10 end}
    val_2 = await %ExampleDeferrable{callback: fn _ -> 20 end}

    val_1 + val_2
  end

  test "defer macro" do
    assert %ExampleDeferrable{evaluated?: false} = test()
    assert evaluate(test()) == 30
  end

  test "test correct evaluation" do
    deferred_value =
      then(
        %ExampleDeferrable{
          callback: fn _ ->
            1
          end
        },
        fn deferred_1__ ->
          val1 = deferred_1__

          then(
            %ExampleDeferrable{
              callback: fn _ ->
                2
              end
            },
            fn deferred_2__ ->
              val2 = deferred_2__

              val1 + val2
            end
          )
        end
      )

    assert evaluate(deferred_value) == 3
  end

  test "a case statement" do
    input =
      quote do
        def test(input) do
          test = 1

          test2 =
            case test do
              input = 1 -> await(%ExampleDeferrable{callback: fn -> input + 2 end})
              input -> 100
            end

          test + test2
        end
      end

    expected_output =
      quote do
        def test(input) do
          test = 1

          then(
            case test do
              input = 1 -> %ExampleDeferrable{callback: fn -> input + 2 end}
              input -> 100
            end,
            fn defer_4062a ->
              test2 = defer_4062a

              test + test2
            end
          )
        end
      end

    assert assert_output(input, expected_output)
  end

  test "an if statement" do
    input =
      quote do
        def test(input) do
          test = 1

          test2 =
            if test == 1 do
              await(%ExampleDeferrable{callback: fn -> input + 2 end})
            else
              100
            end

          test + test2
        end
      end

    expected_output =
      quote do
        def test(input) do
          test = 1

          then(
            case(test == 1) do
              x when Kernel.in(x, [false, nil]) ->
                100

              _ ->
                %ExampleDeferrable{callback: fn -> input + 2 end}
            end,
            fn defer_027ad ->
              test2 = defer_027ad
              test + test2
            end
          )
        end
      end

    assert assert_output(input, expected_output)
  end

  test "simple with statement" do
    input =
      quote do
        def test(input) do
          with 1 <- await(%ExampleDeferrable{callback: fn _ -> 1 end}) do
            :ok
          end
        end
      end

    expected_output =
      quote do
        def test(input) do
          then(%ExampleDeferrable{callback: fn _ -> 1 end}, fn defer_9fffe ->
            with 1 <- defer_9fffe do
              :ok
            end
          end)
        end
      end

    assert assert_output(input, expected_output)
  end

  test "complex with statement" do
    input =
      quote do
        def test(input) do
          with :ok <- :ok,
               1 <- await(%ExampleDeferrable{callback: fn _ -> 1 end}),
               :ok <- :ok,
               1 <- await(%ExampleDeferrable{callback: fn _ -> 2 end}),
               :ok <- :ok do
            :ok
          end
        end
      end

    expected_output =
      quote do
        def test(input) do
          with :ok <- :ok do
            then(%ExampleDeferrable{callback: fn _ -> 1 end}, fn defer_9fffe ->
              with 1 <- defer_9fffe,
                   :ok <- :ok do
                then(%ExampleDeferrable{callback: fn _ -> 2 end}, fn defer_c327e ->
                  with 1 <- defer_c327e,
                       :ok <- :ok do
                    :ok
                  end
                end)
              end
            end)
          end
        end
      end

    assert assert_output(input, expected_output)
  end

  test "await in a nested block" do
    input =
      quote do
        def test(input) do
          test = 1

          bla =
            if input == 3 do
              test2 = await %ExampleDeferrable{callback: fn _ -> input + 4 end}
              test2 + test
            end

          bla + test
        end
      end

    expected_output =
      quote do
        def test(input) do
          test = 1

          then(
            case(input == 3) do
              x when Kernel.in(x, [false, nil]) ->
                nil

              _ ->
                then(%ExampleDeferrable{callback: fn _ -> input + 4 end}, fn defer_bcc0f ->
                  test2 = defer_bcc0f
                  test2 + test
                end)
            end,
            fn defer_144b3 ->
              bla = defer_144b3
              bla + test
            end
          )
        end
      end

    assert assert_output(input, expected_output)
  end

  test "two awaits in function call" do
    input =
      quote do
        def test(input) do
          bla = compare(await(@test_value1), await(@test_value2))
          IO.inspect(bla)
        end
      end

    expected_output =
      quote do
        def test(input) do
          then(@test_value1, fn defer_03693 ->
            then(@test_value2, fn defer_da5b1 ->
              bla = compare(defer_03693, defer_da5b1)
              IO.inspect(bla)
            end)
          end)
        end
      end

    assert assert_output(input, expected_output)
  end

  test "multiple awaits" do
    input =
      quote do
        def test do
          val_1 = await @test_value1
          val_2 = await @test_value2

          val_1 + val_2
        end
      end

    expected_output =
      quote do
        def test do
          then(@test_value1, fn defer_03693 ->
            val_1 = defer_03693

            then(@test_value2, fn defer_da5b1 ->
              val_2 = defer_da5b1
              val_1 + val_2
            end)
          end)
        end
      end

    assert assert_output(input, expected_output)
  end

  test "one value" do
    input =
      quote do
        def test do
          await %ExampleDeferrable{
            callback: fn _ ->
              5
            end
          }
        end
      end

    expected_output =
      quote do
        def test do
          %ExampleDeferrable{
            callback: fn _ ->
              5
            end
          }
        end
      end

    assert assert_output(input, expected_output)
  end

  test "nested invocations of deferred functions" do
    input =
      quote do
        def test do
          val_1 = await nested_1()

          val_2 = await @test_value2

          val_1 + val_2
        end
      end

    expected_output =
      quote do
        def test do
          then(nested_1(), fn defer_858d4 ->
            val_1 = defer_858d4

            then(@test_value2, fn defer_da5b1 ->
              val_2 = defer_da5b1
              val_1 + val_2
            end)
          end)
        end
      end

    assert assert_output(input, expected_output)
  end
end
