# Investigating High Memory Usage: A Hands-on Exercise (Elixir)

This exercise guides you through identifying a process that consumes a significant amount of memory in an Elixir application.

## The Scenario

You are given a module (`mem.ex`) designed to simulate a process that allocates a large amount of data (approximately 1GB). Your task is to run this code, observe its behavior, and use system tools and shell commands to pinpoint the memory-hungry process and understand its memory footprint.

## Part 1: Running the Code and Initial Observation

1. Start the process: `{:ok, pid} = Mem.start()` (if `start/0` returns `{:ok, pid}`) or `pid = Mem.start()` (if it returns `pid`). Check the function signature.

2. Verify if the process is alive: `Process.alive?(pid)`
   What do you observe? Does the process crash or stay alive as expected?
   Does it seem to consume a large amount of memory immediately?

3. **A Potential Pitfall - Compiler Optimizations:**
   Similar to Erlang, the Elixir compiler can optimize code. The `receive do after :infinity -> :ok end` block might be subject to optimizations.

   - Examine the `process_loop/0` function in `mem.ex`.
   - Pay attention to the `receive` block. The code includes `_ = big_data_list` _after_ the `receive` block, which is a good practice to signal that `big_data_list` is used. However, the `receive` block itself is still crucial. If the compiler sees the `receive` is infinite, it might optimize aggressively.
   - **Hint:** To ensure the process genuinely maintains its state (including `big_data_list`), the `receive` block should have some exit condition. Modify the `receive` block to have a timeout or an exit condition?

## Part 2: Investigating Memory Usage (After Addressing Part 1)

Once you've modified the code so the process starts correctly, stays alive, and is expected to hold the large data structure in memory:

### Using the Observer

The Observer is a powerful graphical tool for inspecting running Elixir systems.

1. **Start Observer:**

   - In your IEx session where your process is running, execute `:observer.start().`

2. **Explore:**
   - Navigate to the "Processes" tab.
   - Try to find your `mem` process. You might need to sort by "Memory" or look for processes associated with the `mem` module.
   - Examine its memory usage (columns like "Memory", "Heap Size", etc.). What do you observe?

### Using Shell Commands

You can also programmatically inspect processes from the shell. Your goal is to write a command that helps you find the process with the largest heap size.

1. **List Processes:** You can get a list of all PIDs using `Process.list()`.
2. **Get Process Info:** For a given `pid`, `Process.info(pid, :heap_size)` returns `{:heap_size, size_in_words}` or `nil` if the information isn't available or the process is gone.
3. **Challenge:** Write an Elixir expression (likely a pipeline) to:
   - Get all currently running processes.
   - For each process, retrieve its `:heap_size`.
   - Transform this into a list of tuples, like `{heap_size_in_words, pid}`. Make sure to handle processes for which `:heap_size` might be `nil`.
   - Sort this list in descending order based on `heap_size_in_words`.
   - Display the top few entries or the full sorted list.
   - **Hints:** You'll likely use functions from the `Enum` module such as `Enum.map/2`, `Enum.filter/2` or `Enum.reject/2`, and `Enum.sort/2` (you might need `Enum.sort_by/3` or `Enum.sort/2` with a custom sorting function).
