#-------------------------------------------------------------------
# @copyright (C) 2025, Erlang Solutions Ltd.
# @doc Create a long running process which consumes CPU periodically
# @end
#-------------------------------------------------------------------
defmodule Cpu do
  @doc """
  Create a long running process which consumes CPU periodically. Run start() a few times to create
  more processes with different sleep times.
  """

  @spec start() :: pid()
  def start() do
    # Create a process which sleeps a random (fixed for this process) amount of time
    spawn(fn -> process_loop(:rand.uniform(100) + 50) end)
  end

  defp process_loop(t) do
    counter(100_000)
    Process.sleep(t)
    process_loop(t)
  end

  defp counter(0), do: :ok
  defp counter(n) when n > 0, do: counter(n - 1)
end
