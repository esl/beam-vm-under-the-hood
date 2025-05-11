# CPU Problem Investigation (Erlang)

## The Scenario

The provided code (`cpu.erl`) will spawn one or more processes. These processes are designed to perform calculations that will utilize CPU cycles. Your goal is not just to observe that the CPU is being used, but to understand _which_ processes contribute more and _how_ to measure their activity.

## Running the Code

In the Erlang shell, start the process(es) by calling the exported `cpu:start()` function.

Call this function several times to simulate a busier system.

## Investigation Steps

The key to this exercise is to use the built-in tools and functions provided by Erlang/OTP to inspect running processes.

### General Guidance:

- **Via the Observer:** Erlang provides access to Observer, a graphical tool for inspecting running systems. Run it with `observer:start().` Look for tabs or views related to process information and CPU load.
- **Via Process Information:** You will need to programmatically access information about running processes.

### Erlang Shell Investigation:

1. Get a list of all running PIDs (Process Identifiers) in the system.
2. **Inspecting Individual Processes:** Once you have a PID, how can you get detailed information about it? The `erlang:process_info/2` function will be crucial here.
3. **Finding the Culprit:**
   - What specific piece of information returned by `erlang:process_info/2` might indicate high CPU usage?
   - Iterate through all processes, get this metric for each, and then identify the busiest ones.
     Collect this data, sort it, and display it in a readable format.

## Your Goal

You should be able to see which PIDs are the "hotspots" in the system.
