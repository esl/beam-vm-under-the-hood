#-------------------------------------------------------------------
# @copyright (C) 2025, Erlang Solutions Ltd.
# @doc Create a client that sends random data to a server
# @end
#-------------------------------------------------------------------
defmodule Client do
  require Logger

  @connect_port 11111
  @num_clients 10

  def start() do
    Logger.info("Starting #{@num_clients} clients...")
    for _i <- 1..@num_clients do
      spawn_client()
    end
    :timer.sleep(:infinity) # Keep main process alive to see logs, or handle termination differently
  end

  defp spawn_client() do
    # 10b-10kb random data size (1000 * 10 bytes max)
    data_size = :rand.uniform(1000) * 10
    # sleep 50..150 ms
    sleep_duration = :rand.uniform(100) + 50

    spawn(fn -> start_client_process(data_size, sleep_duration) end)
  end

  defp start_client_process(data_size, sleep_duration) do
    # Erlang's {127,0,0,1} is an IP tuple, Elixir uses charlist or string for hostname
    case :gen_tcp.connect('127.0.0.1', @connect_port, [:binary, active: true], 5000) do
      {:ok, socket} ->
        Logger.debug("Client #{inspect self()} connected. DataSize: #{data_size}, Sleep: #{sleep_duration}ms")
        client_loop(data_size, sleep_duration, socket)
      {:error, reason} ->
        Logger.error("Client #{inspect self()} connection failed: #{reason}")
    end
  end

  defp client_loop(data_size, sleep_duration, socket) do
    # Create binary data of specified size (all zeros)
    # <<0:(DataSize)/unit:8>> in Erlang
    data_to_send = :binary.copy(<<0>>, data_size)

    case :gen_tcp.send(socket, data_to_send) do
      :ok ->
        # Logger.debug("Client #{inspect self()} sent #{byte_size(data_to_send)} bytes")
        # Flushing messages as in Erlang version to see echoes or other socket messages
        case flush(socket) do
          :done ->
            Logger.info("Client #{inspect self()} disconnected (detected in flush).")
            :gen_tcp.close(socket)
          :not_done ->
            :timer.sleep(sleep_duration)
            client_loop(data_size, sleep_duration, socket)
        end
      {:error, reason} ->
        Logger.error("Client #{inspect self()} failed to send data: #{reason}")
        :gen_tcp.close(socket)
    end
  end

  # @doc Receive everything that's arrived
  defp flush(socket) do
    receive do
      {:tcp, ^socket, _data} ->
        # Logger.debug("Client #{inspect self()} received echo of #{byte_size(data)} bytes")
        flush(socket) # Continue flushing
      {:tcp_closed, ^socket} ->
        Logger.info("Client #{inspect self()} detected TCP closed.")
        :done
      {:tcp_error, ^socket, reason} ->
        Logger.error("Client #{inspect self()} TCP error: #{reason}")
        :done # Consider it done on error as well
      other ->
        Logger.warn("Client #{inspect self()} flush received unexpected: #{inspect other}")
        flush(socket)
    after
      0 -> :not_done # If nothing in mailbox, proceed
    end
  end
end
