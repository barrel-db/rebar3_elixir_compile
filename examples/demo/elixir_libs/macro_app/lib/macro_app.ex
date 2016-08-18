defmodule MyMacro do
  defmacro example({value, _, _}) do
    IO.puts "You'll see me at compile-time: #{inspect value}"

    quote do
      IO.puts "You'll see me at run-time: #{inspect unquote(value)}"
    end
  end
end

defmodule MacroApp do
  import MyMacro

  def run do
    IO.puts "Someone called the run function."
    example(hello)
  end
end
