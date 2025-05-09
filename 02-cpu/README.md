# CPU Problem Investigation

## The Scenario

The provided code (`problem_cpu.erl` for Erlang, `problem_cpu.ex` for Elixir) will spawn one or more processes. These processes are designed to perform calculations that will utilize CPU cycles. Your goal is not just to observe that the CPU is being used, but to understand _which_ processes contribute more and _how_ to measure their activity.

## Running the Code

### For Erlang:

In the Erlang shell, start the process(es) by calling the exported `start/0` function:

```erlang
problem_cpu:start().
```

Call this function several times to simulate a busier system.

### For Elixir:

In your IEx session, start the process(es) by calling the `start/0` function:

```elixir
ProblemCpu.start()
```

Call this function several times to simulate a busier system.

## Investigation Steps

The key to this exercise is to use the built-in tools and functions provided by Erlang/OTP (and thus by Elixir) to inspect running processes.

### General Guidance:

- **Via the Observer:** Both Erlang and Elixir environments provide access to Observer, a graphical tool for inspecting running systems. Run it with `observer:start().` in Erlang. Look for tabs or views related to process information and CPU load.
- **Via Process Information:** You will need to programmatically access information about running processes.

### Erlang Shell Investigation:

1.  Get a list of all running PIDs (Process Identifiers) in the system.
2.  **Inspecting Individual Processes:** Once you have a PID, how can you get detailed information about it? The `erlang:process_info/2` function will be crucial here.
3.  **Finding the Culprit:**
    - What specific piece of information returned by `erlang:process_info/2` might indicate high CPU usage?
    - Iterate through all processes, get this metric for each, and then identify the busiest ones.
      Collect this data, sort it, and display it in a readable format.

### Elixir (IEx Shell) Investigation:

1.  Use `Process` module to get list of all running processes.
2.  **Inspecting Individual Processes:** Similar to Erlang, once you have a PID, the `Process.info/2` function will provide details.
3.  **Finding the Culprit:**
    - What key in the map returned by `Process.info/2` corresponds to the CPU usage metric you're looking for?
    - Get this metric for all processes and then sort or filter them to find the most active ones.

## Your Goal

You should be able to see which PIDs are the "hotspots" in the system.
