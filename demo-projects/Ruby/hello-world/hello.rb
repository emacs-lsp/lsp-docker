# Box drawing class.
class Box
  # Initialize to given size, and filled with spaces.
  def initialize(www, h)
    @wid = www
    @hgt = h
    @fill = ' '
  end

  # Change the fill.
  def fill(f)
    @fill = f
    return self
  end

  # Rotate 90 degrees.
  def flip
    @wid, @hgt = @hgt, @wid
    return self
  end

  # Generate (print) the box
  def gen
    line('+', @wid - 2, '-')
    (@hgt - 2).times { line('|', @wid - 2, @fill) }
    line('+', @wid - 2, '-')
  end

  # For printing
  def to_s
    fill = @fill
    if fill == ' '
      fill = '(spaces)'
    end
    return "Box " + @wid.to_s + "x" + @hgt.to_s + ", filled: " + fill
  end

  private
  # Print one line of the box.
  def line(ends, count, fill)
    print ends
    count.times { print fill }
    print ends, "\n"
  end
end

# Create some boxes.
b1 = Box.new(10, 4)
b2 = Box.new(5,12).fill('$')
b3 = Box.new(3,3).fill('@')

print "b1 = ", b1, "\nb2 = ", b2, "\nb3 = ", b3, "\n\n"

# Print some boxes.
print "b1:\n"
b1.gen

print "\nb2:\n"
b2.gen

print "\nb3:\n"
b3.gen

print "\nb2 flipped and filled with #:\n"
b2.fill('#').flip.gen
print "\nb2 = ", b2, "\n"
