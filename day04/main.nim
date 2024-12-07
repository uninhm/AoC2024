import strutils

let input = readFile("input").strip.splitLines

func get(input: seq[string], c, sx, sy, dx, dy: int): string =
  var
    i = 0
    x = sx
    y = sy
  while 0 <= y and y < input.len and 0 <= x and x < input[0].len and i < c:
    result &= input[y][x]
    y += dy
    x += dx
    i += 1

var ans = 0
for y in 0 ..< input.len:
  for x in 0 ..< input[0].len:
    for dx in -1 .. 1:
      for dy in -1 .. 1:
        if dx == 0 and dy == 0:
          continue
        if input.get(4, x, y, dx, dy) == "XMAS":
          ans += 1

echo "Part 1: ", ans

ans = 0
for y in 0 ..< input.len:
  for x in 0 ..< input[0].len:
    if input.get(3, x, y, 1, 1) in ["MAS", "SAM"] and
       input.get(3, x+2, y, -1, 1) in ["MAS", "SAM"]:
      ans += 1

echo "Part 2: ", ans
