# CPU Problem Investigation (Elixir)

## The Scenario

The provided code (`cpu.ex`) will spawn one or more processes. These processes are designed to perform calculations that will utilize CPU cycles. Your goal is not just to observe that the CPU is being used, but to understand _which_ processes contribute more and _how_ to measure their activity.

## Running the Code

1. In IEx compile the code with `c "cpu.ex"` and reload if necessary using `r Cpu`.
2. In your IEx session, start the process(es) by calling the `Cpu.start()` function:
3. Call this function several times to simulate a busier system.

## Investigation Steps

The key to this exercise is to use the built-in tools and functions provided by Erlang/OTP (and thus by Elixir) to inspect running processes.

### General Guidance:

- **Via the Observer:** Elixir provides access to Observer, a graphical tool for inspecting running systems. Run it with `observer:start().` Look for tabs or views related to process information and CPU load.
- **Via Process Information:** You will need to programmatically access information about running processes.

### Elixir (IEx Shell) Investigation:

1. Use `Process` module to get list of all running processes.
2. **Inspecting Individual Processes:** Once you have a PID, the `Process.info/2` function will provide details.
3. **Finding the Culprit:**
   - What key in the map returned by `Process.info/2` corresponds to the CPU usage metric you're looking for?
   - Get this metric for all processes and then sort or filter them to find the most active ones.

## Your Goal

You should be able to see which PIDs are the "hotspots" in the system.
