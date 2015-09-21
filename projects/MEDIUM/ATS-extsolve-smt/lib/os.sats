(**
  A library for controlling and interacting with OS processes
*)
abstype process = ptr

fun
process_start(
  cmd: string, args: List0(String)
): process

fun 
process_read(process): string

overload .read with process_read

fun
process_write(process, string): void

overload .write with process_write

fun
process_stop(process): void

overload .stop with process_stop