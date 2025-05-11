# Investigating High Memory Usage: A Hands-on Exercise (Erlang)

This exercise guides you through identifying a process that consumes a significant amount of memory in an Erlang application.

## The Scenario

You are given a module (`mem.erl`) designed to simulate a process that allocates a large amount of data (approximately 1GB). Your task is to run this code, observe its behavior, and use system tools and shell commands to pinpoint the memory-hungry process and understand its memory footprint.

## Part 1: Running the Code and Initial Observation

1. Run `mem:start().`

2. Verify if the process is alive: `is_pid(Pid), erlang:is_process_alive(Pid).`
   What do you observe? Does the process crash or stay alive as expected?
   Does it seem to consume a large amount of memory immediately?

3. **A Pitfall - Compiler Optimizations:**
   The Erlang compiler is quite smart and aims to produce efficient code. Sometimes, it can optimize code in ways that might be unexpected, especially with constructs intended to keep a process alive indefinitely without active message handling.

   - Examine the `process_loop/0` function in `mem.erl`.
   - Pay attention to the `receive` block: `receive after infinity -> ok end`. This construct, while intending to pause the process indefinitely, can be interpreted by the compiler as an infinite loop and considered an end of a function as the compiler believes the process will never go past it. This leads to `BigData` not being held in memory as intended.
   - **Hint:** To ensure the `BigData` is retained as part of its ongoing state, the `receive` statement must have some exit condition (doesn't matter which, as long as the optimizer doesn't see it as termination or infinite loop). Modify the `receive after infinity` construct to include some time limit or an exit condition.

## Part 2: Investigating Memory Usage (After Addressing Part 1)

Once you've modified the code so the process starts correctly, stays alive, and is expected to hold the large data structure in memory:

### Using the Observer

The Observer is a powerful graphical tool for inspecting running Erlang systems.

1. **Start Observer:**

   - In the Erlang shell where your process is running, execute `observer:start().`

2. **Explore:**
   - Navigate to the "Processes" tab.
   - Try to find your `mem` process. You might need to sort by "Memory" or look for processes associated with the `mem` module.
   - Examine its memory usage (columns like "Memory", "Heap Size", etc.). What do you observe?

### Using Shell Commands

You can also programmatically inspect processes from the shell. Your goal is to write a command that helps you find the process with the largest heap size.

1. **List Processes:** You can get a list of all process identifiers (PIDs) using `erlang:processes().`
2. **Get Process Info:** For any given `Pid`, you can get information about it using `erlang:process_info(Pid, ItemOrItems)`. For memory, `erlang:process_info(Pid, heap_size)` is particularly relevant. It typically returns `[{heap_size, SizeInWords}]` or `[]` if the information isn't available.
3. **Challenge:** Construct an Erlang expression to:
   - Get all currently running processes.
   - For each process, retrieve its `heap_size`.
   - Create a list of tuples, like `{HeapSizeInWords, Pid}`.
   - Sort this list in descending order based on `HeapSizeInWords`.
   - Display the top few entries or the full sorted list to identify the largest memory consumers.
   - **Hints:** You will likely use functions like `lists:map/2`, `lists:sort/2` (with a custom sorting function or by structuring data for default sort, which compares tuple elements from left to right), and `erlang:processes/0`. Remember to handle cases where `erlang:process_info/2` might not return the `heap_size` (e.g., for system processes or if the process just died).
