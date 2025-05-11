# Finding the Busiest Network Client

## 1. Understanding the Scenario

You have:

- A **server** (`server.erl` or `server.ex`): A simple echo server that listens for connections and sends back any data it receives.
- A **client** (`client.erl` or `client.ex`): This application will spawn multiple client processes (10 by default). Each client connects to the server and periodically sends a random (fixed for that process) amount of data.

Determine which of these client connections is responsible for sending the largest volume of data to the server.

## 2. Running the Simulation

You'll need two separate terminal/shell windows: one for the server and one for the client.

### Erlang

- **Start the Server (in Terminal 1):**
  Open an Erlang shell and start the server:
  ```erlang
  $ erl
  1> c(server).
  2> server:start().
  % You should see a log message indicating the server has started.
  ```
- **Start the Client (in Terminal 2):**
  Open another Erlang shell and start the clients:
  ```erlang
  $ erl
  1> c(client).
  2> client:start().
  % This will spawn multiple client processes.
  ```

### Elixir

- **Start the Server (in Terminal 1):**
  Open an IEx session and start the server:
  ```elixir
  $ iex
  iex> c("server.ex") # Compile if not already done or if changed
  iex> c("client.ex") # Compile if not already done or if changed
  iex> Server.start()
  # You should see a log message indicating the server has started.
  ```
- **Start the Client (in Terminal 2):**
  Open another IEx session and start the clients:
  ```elixir
  $ iex
  iex> c("client.ex") # Ensure client module is compiled/loaded
  iex> Client.start()
  # This will spawn multiple client processes.
  ```

Allow the clients to run for a few seconds to record some traffic counters in the stats.

## 3. Investigation Techniques

The key to solving this problem lies in inspecting the properties of the network "ports" that the Erlang VM uses to manage TCP connections.

### General Approach (Both Erlang & Elixir)

1.  **List Active Ports:** You first need to get a list of all currently active ports managed by the Erlang runtime where your clients are running.
2.  **Inspect Each Port:** For each port, you need to retrieve detailed information, specifically looking for statistics related to data transfer.
3.  **Identify Client Ports:** Determine which of these ports correspond to your client connections to the echo server.
4.  **Find the Busiest:** Compare the relevant statistics across all client ports to find the one that has sent the most data.

### Using the Erlang/Elixir Shell

This is best done in the shell where the **client** processes are running.

#### Erlang Shell (`erl`)

1.  **List Ports:**
    The Erlang function `erlang:ports/0` will return a list of all active ports.

    ```erlang
    1> AllPorts = erlang:ports().
    ```

2.  **Get Port Information:**
    For each port in `AllPorts`, you can get detailed information using `erlang:port_info/1`.

    ```erlang
    2> erlang:port_info(hd(AllPorts)). % Example for the first port
    ```

    This function returns a property list (a list of tuples). Examine the output.
    _Hint:_ Look for keys related to "output" or some byte counts.

3.  **Process all Client Ports:**
    You'll want to iterate over all ports and extract the relevant statistic for each.
    Transform the list of ports into a list of `{Statistic, Port}` tuples, sort to find the largest.

#### Elixir Interactive Shell (`iex`)

1.  **List Ports:**
    Use the Erlang function from Elixir:

    ```elixir
    iex> all_ports = :erlang.ports()
    ```

2.  **Get Port Information:**
    Similarly, use `:erlang.port_info/1`:

    ```elixir
    iex> :erlang.port_info(List.first(all_ports)) # Example for the first port
    ```

    This returns a proplist. Examine it to find the statistic representing data sent by the client.

3.  **Process all Client Ports:**
    Elixir's `Enum` module (e.g., `Enum.map/2`, `Enum.sort/2`) provides tools for transforming and sorting this data. Convert the list of ports into a list and sort it based on the data-sent statistic.

### Visually (Using Observer)

Observer is a graphical tool that provides a wealth of information about a running Erlang/Elixir system.

1.  **Start Observer:**
    In the Erlang or IEx shell (preferably the one running the clients):

    ```erlang
    % Erlang
    1> observer:start().
    ```

    ```elixir
    # Elixir
    iex> :observer.start()
    ```

2.  **Navigate to the Ports Tab:**
    Once Observer opens, find the "Ports" tab. This tab lists all active ports.

3.  **Inspect Port Information:**
    - The table displays various details for each port. Look for columns related to input/output byte counts.
    - You can click on column headers to sort the list. This might quickly help you find the port with the highest output.
    - You can select a port and click "Port Info" (or similar button/menu) to get even more detailed information, similar to what `erlang:port_info/1` provides.
