#-------------------------------------------------------------------
# @copyright (C) 2025, Erlang Solutions Ltd.
# @doc Create a long running process which consumes 1 GB of memory
# @end
#-------------------------------------------------------------------
defmodule Mem do
  @moduledoc "Creates a long-running process that consumes approximately 1 GB of memory."

  @doc "Starts the memory-consuming process."
  @spec start() :: pid
  def start do
    spawn(fn -> process_loop() end)
  end

  defp process_loop do
    big_data_list = for _ <- 1..1024, do: create_big_binary(1_048_576)
    :erlang.garbage_collect()

    # Keep the process alive indefinitely
    # There is a bug here: infinite receive is considered end of program by the optimizing compiler.
    receive do
    after
      :infinity -> :ok
    end

    # 'Returning' the value ensures `big_data_list` is considered referenced and not garbage collected.
    big_data_list
  end

  # Creates a binary of `num_bytes` filled with zeros.
  defp create_big_binary(num_bytes) when is_integer(num_bytes) and num_bytes >= 0 do
    <<0::size(num_bytes)-unit(8)>>
  end
end
