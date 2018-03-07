defmodule DeferredTest do
  use ExUnit.Case
  doctest Deferred
  import Deferred
  alias Deferred.TestValue

  deferred def test_input do
    val =
      await(%TestValue{
        callback: fn ->
          1
        end
      })

    val
  end

  def test_output do
    then(
      %TestValue{
        callback: fn ->
          1
        end
      },
      fn prev ->
        val = prev
        val
      end
    )
  end

  test "then macro" do
    first_callback = fn ->
      1
    end

    second_callback = fn val ->
      val + 2
    end

    then_val =
      then(
        %TestValue{
          callback: first_callback
        },
        second_callback
      )

    assert %TestValue{} = then_val
    assert then_val.chained_callbacks == [second_callback]
  end

  test "deferred function" do
    assert Deferred.Value.evaluate(test_input()) == Deferred.Value.evaluate(test_output())
  end

  deferred def multiple_awaits do
    val_1 =
      await(%TestValue{
        callback: fn ->
          1
        end
      })

    val_2 =
      await(%TestValue{
        callback: fn ->
          4
        end
      })

    val_1 + val_2
  end

  test "multiple awaits" do
    assert multiple_awaits() |> Deferred.Value.evaluate() |> Deferred.Value.get_value() == 5
  end

  deferred def nested_1 do
    await(%TestValue{
      callback: fn ->
        5
      end
    })
  end

  deferred def nested_2 do
    val_1 = await(nested_1())

    val_2 =
      await(%TestValue{
        callback: fn ->
          4
        end
      })

    val_1 + val_2
  end

  test "nested invocations of deferred functions" do
    assert nested_2() |> Deferred.Value.evaluate() |> Deferred.Value.get_value() == 9
  end
end