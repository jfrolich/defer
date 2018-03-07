defprotocol Deferred.Value do
  @spec evaluate(t) :: t
  def evaluate(deferred_value)

  @spec get_value(t) :: any | nil
  def get_value(deferred_value)

  @spec add_then(t, callback: (any -> any)) :: t
  def add_then(deferred_value, callback)
end