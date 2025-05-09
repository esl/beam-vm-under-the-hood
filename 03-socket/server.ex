defmodule Server do
  require Logger

  @listen_port 11111

  def start() do
    spawn(fn -> server(@listen_port) end)
    Logger.info("Server started, listening on port #{@listen_port}")
  end

  defp server(port) do
    # {:ok, socket} = :gen_tcp.listen(port, [:binary, packet: :line, active: false])
    # The :line option is not directly available for passive sockets in Elixir's :gen_tcp
    # We will use active mode and handle messages.
    {:ok, listen_socket} = :gen_tcp.listen(port, [:binary, active: false, reuseaddr: true])
    Logger.info("Server listening on port #{port}")
    accept_loop(listen_socket)
  end

  defp accept_loop(listen_socket) do
    case :gen_tcp.accept(listen_socket) do
      {:ok, client_socket} ->
        Logger.info("Accepted connection from #{inspect :inet.peername(client_socket)}")
        # Spawn a process to handle this client
        handler_pid = spawn(fn -> handle_client(client_socket) end)
        # Set the controlling process for the client socket to the new handler
        :gen_tcp.controlling_process(client_socket, handler_pid)
        accept_loop(listen_socket) # Continue accepting more connections
      {:error, reason} ->
        Logger.error("Error accepting connection: #{reason}")
    end
  end

  defp handle_client(socket) do
    receive do
      {:tcp, ^socket, data} ->
        # Echo the data back
        :gen_tcp.send(socket, data)
        handle_client(socket) # Continue handling messages from this client
      {:tcp_closed, ^socket} ->
        Logger.info("Client #{inspect :inet.peername(socket)} disconnected (closed)")
      {:tcp_error, ^socket, reason} ->
        Logger.error("TCP error from #{inspect :inet.peername(socket)}: #{reason}")
      other_message ->
        Logger.warn("Handler received unexpected message: #{inspect other_message}")
        handle_client(socket) # Continue, though this might indicate an issue
    after
      # Optional: Add a timeout if clients might go silent
      # :infinity # No timeout by default
      60_000 -> Logger.warn("Client handler timeout for socket #{inspect socket}")
    end
  end
end
