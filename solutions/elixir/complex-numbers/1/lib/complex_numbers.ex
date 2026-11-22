defmodule ComplexNumbers do
  @typedoc """
  In this module, complex numbers are represented as a tuple-pair containing the real and
  imaginary parts.
  For example, the real number `1` is `{1, 0}`, the imaginary number `i` is `{0, 1}` and
  the complex number `4+3i` is `{4, 3}'.
  """
  @type complex :: {number, number}

  @doc """
  Return the real part of a complex number
  """
  @spec real(a :: complex) :: number
  def real({r, _}), do: r

  @doc """
  Return the imaginary part of a complex number
  """
  @spec imaginary(a :: complex) :: number
  def imaginary({_, i}), do: i

  @doc """
  Multiply two complex numbers, or a real and a complex number
  """
  @spec mul(a :: complex | number, b :: complex | number) :: complex
  def mul({ar, ai}, {br, bi}), do: {(ar * br - ai * bi), (ai * br + ar * bi)}
  def mul(a, b) when is_number(a), do: mul({a,0}, b)
  def mul(a,b) when is_number(b), do: mul(a, {b,0})

  @doc """
  Add two complex numbers, or a real and a complex number
  """
  @spec add(a :: complex | number, b :: complex | number) :: complex
  def add({ar, ai}, {br, bi}), do: {ar + br, ai + bi}
  def add(a,b) when is_number(a), do: add({a,0}, b)
  def add(a,b) when is_number(b), do: add(a, {b,0})

  @doc """
  Subtract two complex numbers, or a real and a complex number
  """
  @spec sub(a :: complex | number, b :: complex | number) :: complex
  def sub({ar, ai}, {br, bi}), do: {ar - br, ai - bi}
  def sub(a,b) when is_number(a), do: sub({a,0}, b)
  def sub(a,b) when is_number(b), do: sub(a, {b,0})

  @doc """
  Divide two complex numbers, or a real and a complex number
  """
  @spec div(a :: complex | number, b :: complex | number) :: complex
  def div(1, {br, bi}), do: {br / (br*br + bi*bi), -bi / (br*br + bi*bi)}
  def div(a,b) when not (is_number(a) or is_number(b)), do: ComplexNumbers.div(1,b) |> mul(a)
  def div(a,b) when is_number(a), do: ComplexNumbers.div({a,0}, b)
  def div(a,b) when is_number(b), do: ComplexNumbers.div(a, {b,0})

  @doc """
  Absolute value of a complex number
  """
  @spec abs(a :: complex) :: number
  def abs({r,i}), do: :math.sqrt(r*r + i*i)


  @doc """
  Conjugate of a complex number
  """
  @spec conjugate(a :: complex) :: complex
  def conjugate({r,i}), do: {r, -i}

  @doc """
  Exponential of a complex number
  """
  @spec exp(a :: complex) :: complex
  def exp({r,i}), do: mul(:math.exp(r), {:math.cos(i), :math.sin(i)})
end
