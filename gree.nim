import std/[os, terminal, strutils, sequtils, algorithm, unicode, math]

when defined(posix):
  import std/posix
  proc wcwidth(c: cint): cint {.importc, header: "<wchar.h>".}
  discard setlocale(LC_CTYPE, "")

proc cellWidth(r: Rune): int =
  if r.int < 128: return 1
  when defined(posix): result = max(wcwidth(r.cint).int, 0)
  else: result = 1

proc cellWidth(s: string): int =
  for r in s.runes: result += r.cellWidth

type
  Node {.acyclic.} = ref object
    name: string
    isDir: bool
    children: seq[Node]
    path, link: string
    isLink, broken, cut, denied: bool
    parts: seq[Node]
    more: int
    total: int
    open, loaded, packed: bool
    depth, px: int
    y, x0, x1: int

var maxDepth = 8
var budget = 0
var seen = 0
var pending = 1
var rootHidden = 0
var Gap = 3
var W, H: int
var grid: seq[seq[string]]
var valign = 0
var fill = false
var dense = 0
var showHidden = false
var plain = false
var all = false
var unlimited = false
var showInfo = true
var lines = 0
var color = stdout.isatty

proc clean(s: string): string =
  if s.allCharsInSet({' ' .. '~'}): return s
  for r in s.runes:
    if r.int < 0x20 or r.int in 0x7f .. 0x9f: result.add '?'
    else: result.add $r

proc fail(msg: varargs[string, `$`]) =
  stderr.writeLine "gree: " & msg.join("")
  quit(1)

proc dirsFirst(a, b: Node): int =
  if a.isDir != b.isDir: return (if a.isDir: -1 else: 1)
  result = cmpIgnoreCase(a.name, b.name)
  if result == 0: result = cmp(a.name, b.name)

proc load(n: Node) =
  if n.loaded or not n.isDir: return
  n.loaded = true
  let deep = n.depth >= maxDepth
  if not deep: dec pending
  try:
    for kind, p in walkDir(n.path, checkDir = true):
      let name = p.extractFilename
      if name[0] == '.' and not all: continue
      if deep: (n.cut = true; break)
      let c = Node(name: clean(name), path: p, isDir: kind == pcDir,
                   total: ord(kind != pcDir), depth: n.depth + 1)
      if kind in {pcLinkToDir, pcLinkToFile}:
        c.isLink = true
        try: c.link = clean(expandSymlink(p))
        except OSError: c.link = "?"
        when defined(posix):
          var info: Stat
          c.broken = stat(p.cstring, info) != 0
        else:
          c.broken = not fileExists(p) and not dirExists(p)
      if not c.isDir: inc seen
      elif c.depth < maxDepth: inc pending
      n.children.add c
  except OSError:
    n.denied = true
    return
  n.children.sort(dirsFirst)

proc loadAll(n: Node) =
  n.load
  for c in n.children: c.loadAll
  if n.isDir: n.total = n.children.mapIt(it.total).sum

proc reach(n: Node, cap: int): int =
  if cap <= 0: return 0
  n.load
  if n.children.len == 0: return 1
  for c in n.children:
    result += c.reach(cap - result)
    if result >= cap: return cap

proc kids(n: Node): seq[Node] =
  if n.open: n.children else: @[]

proc hidden(n: Node): int =
  if not showHidden or not n.isDir: return 0
  if n.depth == 0: return rootHidden
  max(n.total - n.kids.mapIt(it.total).sum, 0)

const
  DirColor = ansiStyleCode(styleBright) & ansiForegroundColorCode(fgBlue)
  ExecColor = ansiStyleCode(styleBright) & ansiForegroundColorCode(fgGreen)
  PipeColor = ansiForegroundColorCode(fgYellow)
  SocketColor = ansiStyleCode(styleBright) & ansiForegroundColorCode(fgMagenta)
  DeviceColor = ansiStyleCode(styleBright) & ansiForegroundColorCode(fgYellow)
  LinkColor = ansiStyleCode(styleBright) & ansiForegroundColorCode(fgCyan)
  BadColor = ansiStyleCode(styleBright) & ansiForegroundColorCode(fgRed)
  NumColor = ansiForegroundColorCode(fgYellow, bright = true)

proc tag(n: Node, dir: int): string =
  let hidden = n.hidden
  if hidden > 0: (if dir > 0: "+" & $hidden else: $hidden & "+") else: ""

proc dirName(n: Node): string =
  if plain and not color and not n.name.endsWith('/'): n.name & (if n.denied: "!" else: "/") else: n.name

proc label(n: Node): string =
  if n.more > 0: return "+" & $n.more & " more"
  if n.isLink: return n.name & (if plain: "->" else: "→") & n.link
  if not n.isDir: return n.name
  result = n.dirName
  let text = n.tag(1)
  if text.len > 0: result &= " " & text
  if not plain: result &= "  "

proc width(n: Node): int = n.label.cellWidth

proc ellipsis(): seq[string] =
  if plain: @[".", ".", "."] else: @["…"]

proc shownLeaves(n: Node): int =
  if n.more > 0: return 0
  if n.isDir: return n.kids.mapIt(it.shownLeaves).sum
  let dots = ellipsis().len
  let lo = if n.x0 < 0: dots else: 0
  let hi = if n.x1 > W: W - dots else: W
  var x = n.x0
  for p in (if n.parts.len > 0: n.parts else: @[n]):
    if x < hi and x + p.width > lo: inc result
    x += p.width + 2

proc height(n: Node): int =
  max(n.kids.mapIt(it.height).sum, 1)

proc pack(n: Node, x: int) =
  n.packed = true
  let childX = x + n.width + Gap
  let limit = max(budget - childX, 12)
  var packed: seq[Node]
  var row = Node()
  for c in n.children:
    if c.isDir:
      c.px = childX; packed.add c
      continue
    if row.parts.len > 0 and row.name.cellWidth + 2 + c.width > limit:
      packed.add row; row = Node()
    row.name.add (if row.parts.len == 0: "" else: "  ") & c.label
    row.parts.add c
    inc row.total
  if row.parts.len > 0: packed.add row
  n.children = packed

proc colWidths(n: Node, depth: int, cols: var seq[int]) =
  if cols.len <= depth: cols.add 0
  cols[depth] = max(cols[depth], n.width)
  for c in n.kids: c.colWidths(depth + 1, cols)

proc colWidths(n: Node): seq[int] = n.colWidths(0, result)

proc extent(n: Node, cols: seq[int], depth = 0): int =
  if n.kids.len == 0: return n.width
  (if dense > 0: n.width else: cols[depth]) + Gap + n.kids.mapIt(it.extent(cols, depth + 1)).max

proc closedDirs(n: Node, depth: int): seq[Node] =
  if depth > 0:
    for c in n.kids: result.add c.closedDirs(depth - 1)
    return
  n.load
  if dense > 1 and n.isDir and not n.packed: n.pack(n.px)
  if n.isDir and n.children.len > 0 and not n.open: result.add n

proc trim(n: Node, keep: int) =
  if n.children.len <= keep: return
  let cut = n.children[keep .. ^1]
  n.children.setLen(keep)
  if not showHidden: n.children.add Node(more: cut.mapIt(max(it.parts.len, 1)).sum)

proc bump(h: var seq[int], i, by: int) =
  if h.len <= i: h.setLen(i + 1)
  h[i] += by

proc top(h: seq[int]): int =
  for i in countdown(h.len - 1, 0):
    if h[i] > 0: return i

proc openToFit(n: Node, rows, width: int) =
  var colCnt: seq[seq[int]]
  var farCnt: seq[int]
  var height = n.height
  proc add(m: Node, depth, by: int) =
    if colCnt.len <= depth: colCnt.setLen(depth + 1)
    colCnt[depth].bump(m.width, by)
    if m.kids.len == 0: farCnt.bump(m.x1, by)
  proc toggle(d: Node, depth: int, open: bool) =
    if not open:
      for c in d.children: c.add(depth + 1, -1)
    d.add(depth, -1)
    d.open = open
    d.x1 = d.x0 + Gap + d.width
    d.add(depth, 1)
    if open:
      for c in d.children:
        c.x0 = d.x1; c.x1 = d.x1 + Gap + c.width
        c.add(depth + 1, 1)
  proc cur(): int =
    if dense > 0: farCnt.top else: colCnt[1 .. ^1].mapIt(it.top).filterIt(it > 0).mapIt(it + Gap).sum
  proc fits(d: Node, depth: int): bool =
    if not unlimited and height + d.children.len - 1 > rows: return false
    let now = cur()
    d.toggle(depth, true)
    result = cur() <= max(width, now)
    if result: height += d.children.len - 1 else: d.toggle(depth, false)
  n.x1 = 0; n.add(0, 1)
  for c in n.children:
    c.x0 = 0; c.x1 = Gap + c.width
    c.add(1, 1)
  for depth in 1 .. maxDepth:
    let cands = n.closedDirs(depth)
    if cands.len == 0: break
    for d in cands: discard d.fits(depth)
  for depth in 1 .. maxDepth:
    if depth >= colCnt.len: break
    for d in n.closedDirs(depth):
      let keep = if unlimited: d.children.len
                 else: min(d.children.len, rows - height + ord(showHidden))
      let saved = d.children
      var widest = newSeq[int](keep)
      for i in 0 ..< keep: widest[i] = max(saved[i].width, if i > 0: widest[i - 1] else: 0)
      for limit in countdown(keep, 1):
        if limit < keep and widest[limit - 1] == widest[limit]: continue
        d.trim(limit)
        if d.fits(depth): break
        d.children = saved

proc indent(row: seq[string]): int =
  while result < row.len and row[result] == " ": inc result

proc put(x, y: int, s: string) =
  if y >= 0 and y < H and x >= 0 and x < W: grid[y][x] = s

proc cells(s, code: string): seq[string] =
  var last = -1
  for r in s.runes:
    let w = r.cellWidth
    if w == 0:
      if last >= 0: result[last] &= $r
    else:
      last = result.len
      result.add $r
      for i in 1 ..< w: result.add ""
  if color and code.len > 0 and last >= 0:
    result[0] = code & result[0]
    result[last] &= ansiResetCode

proc typeColor(path: string, follow: bool): string =
  if not color: return ""
  when defined(posix):
    var info: Stat
    if (if follow: stat(path.cstring, info) else: lstat(path.cstring, info)) == 0:
      let mode = info.st_mode
      if S_ISDIR(mode): return DirColor
      if S_ISFIFO(mode): return PipeColor
      if S_ISSOCK(mode): return SocketColor
      if S_ISBLK(mode) or S_ISCHR(mode): return DeviceColor
      if S_ISREG(mode) and (mode and Mode(S_IXUSR or S_IXGRP or S_IXOTH)) != 0: return ExecColor
  ""

proc leafCells(n: Node, dir: int): seq[string] =
  if not n.isLink:
    return cells(n.label, if n.more > 0: ansiStyleCode(styleDim) else: n.path.typeColor(false))
  let name = cells(n.name, LinkColor)
  let target = cells(n.link, if n.broken: BadColor else: n.path.typeColor(true))
  let arrow = cells(if plain: (if dir > 0: "->" else: "<-") else: (if dir > 0: "→" else: "←"),
                    ansiStyleCode(styleDim))
  if dir > 0: name & arrow & target else: target & arrow & name

proc putLabel(n: Node, dir: int) =
  var cs: seq[string]
  if not n.isDir:
    for i, p in (if n.parts.len > 0: n.parts else: @[n]):
      if i > 0: cs.add [" ", " "]
      cs.add p.leafCells(dir)
  else:
    let icon = if plain: @[] else: @[if n.denied: "🔒" elif n.children.len > 0 or n.cut: "📁" else: "📂", ""]
    let name = cells(n.dirName, if plain and n.denied: BadColor else: DirColor)
    let tag = cells(n.tag(dir), NumColor)
    cs = if tag.len == 0: (if dir > 0: icon & name else: name & icon)
         elif dir > 0: icon & name & @[" "] & tag
         else: tag & @[" "] & name & icon
  let dots = ellipsis()
  var x = n.x0
  if x + cs.len > W:
    let keep = max(W - x - dots.len, 0)
    if keep > 0 and cs[keep].len == 0: cs[keep - 1] = " "
    cs = cs[0 ..< keep] & dots
    if color: cs[^1] &= ansiResetCode
  elif x < 0 and x + cs.len > 0:
    let start = min(dots.len - x, cs.len)
    if start < cs.len and cs[start].len == 0: cs[start] = " "
    var code = ""
    for c in cs[0 ..< start]:
      var k = 0
      while c.continuesWith("\e[", k):
        let m = c.find('m', k)
        if m < 0: break
        k = m + 1
      if k > 0: code = c[0 ..< k]
      if c.endsWith(ansiResetCode): code = ""
    if start < cs.len: cs[start] = code & cs[start]
    cs = dots[0 ..< min(dots.len, x + cs.len)] & cs[start .. ^1]
    if color: cs[^1] &= ansiResetCode
    x = 0
  for i, c in cs: put(x + i, n.y, c)

proc layout(n: Node, depth: int, cols: seq[int], top, dir, xEdge: int): int =
  n.x0 = if dir > 0: xEdge else: xEdge - n.width
  n.x1 = if dir > 0: xEdge + n.width else: xEdge
  if n.kids.len == 0: n.y = top; result = 1
  else:
    let edge = if dense > 0: (if dir > 0: n.x1 + Gap else: n.x0 - Gap)
               else: xEdge + dir * (cols[depth] + Gap)
    for c in n.kids:
      result += c.layout(depth + 1, cols, top + result, dir, edge)
    n.y = if valign > 0: n.kids[0].y elif valign < 0: n.kids[^1].y
          else: (n.kids[0].y + n.kids[^1].y) div 2

proc draw(n: Node, dir: int) =
  if n.kids.len == 0: return
  let rows = n.kids.mapIt(it.y)
  let y0 = min(rows[0], n.y); let y1 = max(rows[^1], n.y)
  let cx = if dir > 0: n.kids[0].x0 - 2 else: n.kids[0].x1 + 1
  let (first, last, branch, stem) =
    if dir > 0: ("┌", "└", "├", "┤") else: ("┐", "┘", "┤", "├")
  for y in y0 .. y1: put(cx, y, "│")
  for y in rows: put(cx, y, branch)
  put(cx, y0, first); put(cx, y1, last)
  let joins = n.y in rows
  put(cx, n.y, if y0 == y1: "─"
               elif n.y == y0: (if joins: "┬" elif dir > 0: "┐" else: "┌")
               elif n.y == y1: (if joins: "┴" elif dir > 0: "┘" else: "└")
               elif joins: "┼"
               else: stem)
  let (sx0, sx1) = if dir > 0: (n.x1, cx - 1) else: (cx + 1, n.x0 - 1)
  for x in sx0 .. sx1: put(x, n.y, "─")
  for c in n.kids:
    put(cx + dir, c.y, "─")
    c.putLabel(dir)
    c.draw(dir)

proc usage() =
  echo """Usage: gree [-l | -r] [-t | -b] [-f] [-d | -dd] [-n] [-p] [-a] [-u] [-i] [-c] [-H rows] [-L level] [dir]

Position
  -l  root on the left, branches grow right
  -r  root on the right, branches grow left
  -t  top-aligned: each folder sits on its first child's row (default: middle)
  -b  bottom-aligned: each folder sits on its last child's row

Display
  -f  fill the whole terminal height, centering the tree, instead of only the rows it needs
  -d  dense: children start right after their parent instead of in aligned columns,
      which shows more of the tree at the cost of the tidy look
  -dd also packs sibling files side by side on shared rows
  -n  show +N after each folder for the number of files under it that are not displayed
  -p  plain: no folder emoji, folders are told apart by color, or end with / without color
      (a folder that cannot be read is red, or ends with ! without color)
  -a  include hidden entries (names starting with a dot)
  -u  unlimited height: show everything the width allows, for piping into less or a file
      (no width limit when there is no terminal)
  -c  color even when the output is not a terminal, e.g. for less -R (default: auto)
  -i  no info line at the bottom, which frees one more row for the tree
  -H  height: number of output lines including the info line, at least 2 (1 with -i)
  -L  levels: how many folder levels deep to scan (default 8)

Flags combine, e.g. -tl gives a tree-like layout from the top-left corner.
  -h, --help  show this help"""
  quit(0)

proc parseArgs(): (int, string) =
  result = (0, ".")
  let args = commandLineParams()
  var i = 0
  while i < args.len:
    let a = args[i]
    inc i
    if a == "--help": usage()
    elif a.len > 1 and a[0] == '-':
      for j in 1 ..< a.len:
        let ch = a[j]
        if ch in {'H', 'L'}:
          let v = if j + 1 < a.len: a[j + 1 .. ^1] else: (if i < args.len: (inc i; args[i - 1]) else: "")
          let num = try: v.parseInt except ValueError: 0
          if num < 1: fail "-", ch, " needs a number"
          if ch == 'H': lines = num else: maxDepth = num
          break
        case ch
        of 'l': result[0] = 1
        of 'r': result[0] = -1
        of 't': valign = 1
        of 'b': valign = -1
        of 'f': fill = true
        of 'n': showHidden = true
        of 'p': plain = true
        of 'a': all = true
        of 'u': unlimited = true
        of 'i': showInfo = false
        of 'c': color = true
        of 'd': inc dense; Gap = 2
        of 'h': usage()
        else:
          fail "unknown option -", ch
    else: result[1] = a

proc leftCount(tree: Node, cap: int): int =
  let hs = tree.children.mapIt(it.reach(cap))
  let total = hs.sum
  var acc = 0
  for h in hs:
    if 2 * acc + h >= total: break
    acc += h; inc result

proc main() =
  let (side, root) = parseArgs()
  var path = root.absolutePath.normalizedPath.strip(leading = false, chars = {'/'})
  if path.len == 0: path = "/"
  if not dirExists(path): fail "not a directory: ", clean(root)
  let tree = Node(name: clean(path.extractFilename), path: path, isDir: true, open: true)
  if tree.name.len == 0: tree.name = path
  if lines > 0: unlimited = false
  let wide = unlimited and not (stdin.isatty or stdout.isatty or stderr.isatty) and
             not existsEnv("COLUMNS")
  W = terminalWidth()
  let info = ord(showInfo)
  let th = terminalHeight()
  let termH = (if th > 0: th else: 24) - 2 - info
  let rows = if unlimited: int.high div 2 elif lines > 0: lines - info else: termH
  if rows < 1:
    fail if lines > 0: "-H must be at least " & $(1 + info) else: "terminal too small"
  if showHidden: tree.loadAll else: tree.load
  if tree.denied: fail "permission denied: ", clean(root)

  proc sideOf(kids: seq[Node]): Node =
    Node(name: tree.name, isDir: true, open: true, children: kids)
  let mid = if side == 0: tree.leftCount(max(rows div 4, 1)) else: 0
  var sides: seq[(Node, int)]
  var rootLen = -1
  while tree.width > rootLen:
    rootLen = tree.width
    budget = if side == 0: W div 2 - rootLen div 2 - 2 else: W - rootLen - 2
    sides = if side == 0: @[(sideOf(tree.children[mid .. ^1]), 1), (sideOf(tree.children[0 ..< mid]), -1)]
            else: @[(sideOf(tree.children), side)]
    for (s, _) in sides:
      if dense > 1: s.pack(-rootLen)
      if s.children.len > rows: s.trim(rows - 1 + ord(showHidden))
    rootHidden = tree.total - sides.mapIt(it[0].children.mapIt(it.total).sum).sum
  for (s, _) in sides: s.openToFit(rows, if wide: int.high div 4 else: budget)
  if wide:
    let need = sides.mapIt(it[0].extent(it[0].colWidths)).max
    W = if side == 0: 2 * need + 2 else: need + 1
  let rootX0 = if side == 0: W div 2 - rootLen div 2
               elif side > 0: 0 else: W - rootLen
  let tall = sides.mapIt(it[0].height).max
  H = if fill and not unlimited: max(termH, tall) else: rows - (rows - tall) div 2 * 2
  grid = newSeqWith(H, newSeqWith(W, " "))
  for (s, dir) in sides:
    let top = if valign > 0: 0 elif valign < 0: H - s.height else: (H - s.height) div 2
    discard s.layout(0, s.colWidths, top, dir, if dir > 0: rootX0 else: rootX0 + rootLen)
  let full = sides.filterIt(it[0].kids.len > 0).mapIt(it[0])
  var ry = sides.mapIt(it[0].y).sum div sides.len
  if valign == 0 and sides.len == 2 and full.len > 0:
    let lo = full.mapIt(it.kids[0].y).max
    let hi = full.mapIt(it.kids[^1].y).min
    ry = (H - tall) div 2 + (tall - 1) div 2
    if lo <= hi: ry = ry.clamp(lo, hi)
  for (s, dir) in sides:
    s.y = ry
    s.draw(dir)
  tree.x0 = sides[^1][0].x0; tree.y = ry
  tree.putLabel(sides[^1][1])

  let used = if fill: @[0, H - 1] else: toSeq(0 ..< H).filterIt(grid[it].anyIt(it != " "))
  if used.len > 0:
    let cut = if wide: toSeq(used[0] .. used[^1]).mapIt(grid[it].indent).min else: 0
    for y in used[0] .. used[^1]: echo grid[y][cut .. ^1].join("").strip(leading = false)
  if showInfo:
    let shown = sides.mapIt(it[0].shownLeaves).sum
    let (dim, reset) = if color: (ansiStyleCode(styleDim), ansiResetCode) else: ("", "")
    echo dim, shown, " of ", seen, (if pending > 0: "+" else: ""), " files shown", reset

main()
