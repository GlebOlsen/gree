import std/[os, terminal, strutils, sequtils, algorithm, unicode, math]
when defined(posix): import std/posix
when defined(windows):
  import std/[winlean, exitprocs]
  proc setConsoleOutputCP(cp: uint32): WINBOOL {.importc: "SetConsoleOutputCP", stdcall, dynlib: "kernel32".}
  proc getConsoleOutputCP(): uint32 {.importc: "GetConsoleOutputCP", stdcall, dynlib: "kernel32".}
  if stdout.isatty:
    let oldCP = getConsoleOutputCP()
    if oldCP != 65001 and setConsoleOutputCP(65001) != 0:
      addExitProc(proc() = discard setConsoleOutputCP(oldCP))
    var conMode: DWORD
    if getConsoleMode(getStdHandle(STD_OUTPUT_HANDLE), addr conMode) != 0:
      discard setConsoleMode(getStdHandle(STD_OUTPUT_HANDLE), conMode or 4) # ENABLE_VIRTUAL_TERMINAL_PROCESSING
  let exeExts = getEnv("PATHEXT", ".COM;.EXE;.BAT;.CMD").toUpperAscii.split(';').filterIt(it.len > 0)

const
  # Combining marks, format characters, variation selectors, Hangul jamo vowels/finals.
  ZeroWidth = [
    (0x0300, 0x036F), (0x0483, 0x0489), (0x0591, 0x05BD), (0x05BF, 0x05BF), (0x05C1, 0x05C2),
    (0x05C4, 0x05C5), (0x05C7, 0x05C7), (0x0600, 0x0605), (0x0610, 0x061A), (0x061C, 0x061C),
    (0x064B, 0x065F), (0x0670, 0x0670), (0x06D6, 0x06DD), (0x06DF, 0x06E4), (0x06E7, 0x06E8),
    (0x06EA, 0x06ED), (0x070F, 0x070F), (0x0711, 0x0711), (0x0730, 0x074A), (0x07A6, 0x07B0),
    (0x07EB, 0x07F3), (0x07FD, 0x07FD), (0x0816, 0x0819), (0x081B, 0x0823), (0x0825, 0x0827),
    (0x0829, 0x082D), (0x0859, 0x085B), (0x0890, 0x0891), (0x0898, 0x089F), (0x08CA, 0x0902), (0x093A, 0x093A),
    (0x093C, 0x093C), (0x0941, 0x0948), (0x094D, 0x094D), (0x0951, 0x0957), (0x0962, 0x0963),
    (0x0981, 0x0981), (0x09BC, 0x09BC), (0x09C1, 0x09C4), (0x09CD, 0x09CD), (0x09E2, 0x09E3),
    (0x09FE, 0x09FE), (0x0A01, 0x0A02), (0x0A3C, 0x0A3C), (0x0A41, 0x0A42), (0x0A47, 0x0A48),
    (0x0A4B, 0x0A4D), (0x0A51, 0x0A51), (0x0A70, 0x0A71), (0x0A75, 0x0A75), (0x0A81, 0x0A82),
    (0x0ABC, 0x0ABC), (0x0AC1, 0x0AC5), (0x0AC7, 0x0AC8), (0x0ACD, 0x0ACD), (0x0AE2, 0x0AE3),
    (0x0AFA, 0x0AFF), (0x0B01, 0x0B01), (0x0B3C, 0x0B3C), (0x0B3F, 0x0B3F), (0x0B41, 0x0B44),
    (0x0B4D, 0x0B4D), (0x0B55, 0x0B56), (0x0B62, 0x0B63), (0x0B82, 0x0B82), (0x0BC0, 0x0BC0),
    (0x0BCD, 0x0BCD), (0x0C00, 0x0C00), (0x0C04, 0x0C04), (0x0C3C, 0x0C3C), (0x0C3E, 0x0C40),
    (0x0C46, 0x0C48), (0x0C4A, 0x0C4D), (0x0C55, 0x0C56), (0x0C62, 0x0C63), (0x0C81, 0x0C81),
    (0x0CBC, 0x0CBC), (0x0CBF, 0x0CBF), (0x0CC6, 0x0CC6), (0x0CCC, 0x0CCD), (0x0CE2, 0x0CE3),
    (0x0D00, 0x0D01), (0x0D3B, 0x0D3C), (0x0D41, 0x0D44), (0x0D4D, 0x0D4D), (0x0D62, 0x0D63),
    (0x0D81, 0x0D81), (0x0DCA, 0x0DCA), (0x0DD2, 0x0DD4), (0x0DD6, 0x0DD6), (0x0E31, 0x0E31),
    (0x0E34, 0x0E3A), (0x0E47, 0x0E4E), (0x0EB1, 0x0EB1), (0x0EB4, 0x0EBC), (0x0EC8, 0x0ECE),
    (0x0F18, 0x0F19), (0x0F35, 0x0F35), (0x0F37, 0x0F37), (0x0F39, 0x0F39), (0x0F71, 0x0F7E),
    (0x0F80, 0x0F84), (0x0F86, 0x0F87), (0x0F8D, 0x0FBC), (0x0FC6, 0x0FC6), (0x102D, 0x1030),
    (0x1032, 0x1037), (0x1039, 0x103A), (0x103D, 0x103E), (0x1058, 0x1059), (0x105E, 0x1060),
    (0x1071, 0x1074), (0x1082, 0x1082), (0x1085, 0x1086), (0x108D, 0x108D), (0x109D, 0x109D),
    (0x1160, 0x11FF), (0x135D, 0x135F), (0x1712, 0x1714), (0x1732, 0x1733), (0x1752, 0x1753),
    (0x1772, 0x1773), (0x17B4, 0x17B5), (0x17B7, 0x17BD), (0x17C6, 0x17C6), (0x17C9, 0x17D3),
    (0x17DD, 0x17DD), (0x180B, 0x180F), (0x1885, 0x1886), (0x18A9, 0x18A9), (0x1920, 0x1922),
    (0x1927, 0x1928), (0x1932, 0x1932), (0x1939, 0x193B), (0x1A17, 0x1A18), (0x1A1B, 0x1A1B),
    (0x1A56, 0x1A56), (0x1A58, 0x1A5E), (0x1A60, 0x1A60), (0x1A62, 0x1A62), (0x1A65, 0x1A6C),
    (0x1A73, 0x1A7C), (0x1A7F, 0x1A7F), (0x1AB0, 0x1ACE), (0x1B00, 0x1B03), (0x1B34, 0x1B34),
    (0x1B36, 0x1B3A), (0x1B3C, 0x1B3C), (0x1B42, 0x1B42), (0x1B6B, 0x1B73), (0x1B80, 0x1B81),
    (0x1BA2, 0x1BA5), (0x1BA8, 0x1BA9), (0x1BAB, 0x1BAD), (0x1BE6, 0x1BE6), (0x1BE8, 0x1BE9),
    (0x1BED, 0x1BED), (0x1BEF, 0x1BF1), (0x1C2C, 0x1C33), (0x1C36, 0x1C37), (0x1CD0, 0x1CD2),
    (0x1CD4, 0x1CE0), (0x1CE2, 0x1CE8), (0x1CED, 0x1CED), (0x1CF4, 0x1CF4), (0x1CF8, 0x1CF9),
    (0x1DC0, 0x1DFF), (0x200B, 0x200F), (0x202A, 0x202E), (0x2060, 0x2064), (0x2066, 0x206F),
    (0x20D0, 0x20F0), (0x2CEF, 0x2CF1), (0x2D7F, 0x2D7F), (0x2DE0, 0x2DFF), (0x302A, 0x302D),
    (0x3099, 0x309A), (0xA66F, 0xA672), (0xA674, 0xA67D), (0xA69E, 0xA69F), (0xA6F0, 0xA6F1),
    (0xA802, 0xA802), (0xA806, 0xA806), (0xA80B, 0xA80B), (0xA825, 0xA826), (0xA82C, 0xA82C),
    (0xA8C4, 0xA8C5), (0xA8E0, 0xA8F1), (0xA8FF, 0xA8FF), (0xA926, 0xA92D), (0xA947, 0xA951),
    (0xA980, 0xA982), (0xA9B3, 0xA9B3), (0xA9B6, 0xA9B9), (0xA9BC, 0xA9BD), (0xA9E5, 0xA9E5),
    (0xAA29, 0xAA2E), (0xAA31, 0xAA32), (0xAA35, 0xAA36), (0xAA43, 0xAA43), (0xAA4C, 0xAA4C),
    (0xAA7C, 0xAA7C), (0xAAB0, 0xAAB0), (0xAAB2, 0xAAB4), (0xAAB7, 0xAAB8), (0xAABE, 0xAABF),
    (0xAAC1, 0xAAC1), (0xAAEC, 0xAAED), (0xAAF6, 0xAAF6), (0xABE5, 0xABE5), (0xABE8, 0xABE8),
    (0xABED, 0xABED), (0xD7B0, 0xD7FF), (0xFB1E, 0xFB1E), (0xFE00, 0xFE0F), (0xFE20, 0xFE2F),
    (0xFEFF, 0xFEFF), (0xFFF9, 0xFFFB), (0x101FD, 0x101FD), (0x102E0, 0x102E0), (0x10376, 0x1037A),
    (0x10A01, 0x10A0F), (0x10A38, 0x10A3F), (0x10AE5, 0x10AE6), (0x10D24, 0x10D27), (0x10EAB, 0x10EAC), (0x10EFD, 0x10EFF),
    (0x10F46, 0x10F50), (0x10F82, 0x10F85), (0x11001, 0x11001), (0x11038, 0x11046), (0x11070, 0x11070),
    (0x11073, 0x11074), (0x1107F, 0x11081), (0x110B3, 0x110B6), (0x110B9, 0x110BA), (0x110BD, 0x110BD), (0x110C2, 0x110C2), (0x110CD, 0x110CD),
    (0x11100, 0x11102), (0x11127, 0x1112B), (0x1112D, 0x11134), (0x11173, 0x11173), (0x11180, 0x11181),
    (0x111B6, 0x111BE), (0x111C9, 0x111CC), (0x111CF, 0x111CF), (0x1122F, 0x11231), (0x11234, 0x11234),
    (0x11236, 0x11237), (0x1123E, 0x1123E), (0x11241, 0x11241), (0x112DF, 0x112DF), (0x112E3, 0x112EA), (0x11300, 0x11301),
    (0x1133B, 0x1133C), (0x11340, 0x11340), (0x11366, 0x1136C), (0x11370, 0x11374), (0x11438, 0x1143F),
    (0x11442, 0x11444), (0x11446, 0x11446), (0x1145E, 0x1145E), (0x114B3, 0x114B8), (0x114BA, 0x114BA),
    (0x114BF, 0x114C0), (0x114C2, 0x114C3), (0x115B2, 0x115B5), (0x115BC, 0x115BD), (0x115BF, 0x115C0),
    (0x115DC, 0x115DD), (0x11633, 0x1163A), (0x1163D, 0x1163D), (0x1163F, 0x11640), (0x116AB, 0x116AB),
    (0x116AD, 0x116AD), (0x116B0, 0x116B5), (0x116B7, 0x116B7), (0x1171D, 0x1171F), (0x11722, 0x11725),
    (0x11727, 0x1172B), (0x1182F, 0x11837), (0x11839, 0x1183A), (0x1193B, 0x1193C), (0x1193E, 0x1193E),
    (0x11943, 0x11943), (0x119D4, 0x119D7), (0x119DA, 0x119DB), (0x119E0, 0x119E0), (0x11A01, 0x11A0A),
    (0x11A33, 0x11A38), (0x11A3B, 0x11A3E), (0x11A47, 0x11A47), (0x11A51, 0x11A56), (0x11A59, 0x11A5B),
    (0x11A8A, 0x11A96), (0x11A98, 0x11A99), (0x11C30, 0x11C36), (0x11C38, 0x11C3D), (0x11C3F, 0x11C3F),
    (0x11C92, 0x11CA7), (0x11CAA, 0x11CB0), (0x11CB2, 0x11CB3), (0x11CB5, 0x11CB6), (0x11D31, 0x11D36),
    (0x11D3A, 0x11D3A), (0x11D3C, 0x11D3D), (0x11D3F, 0x11D45), (0x11D47, 0x11D47), (0x11D90, 0x11D91),
    (0x11D95, 0x11D95), (0x11D97, 0x11D97), (0x11EF3, 0x11EF4), (0x11F00, 0x11F01), (0x11F36, 0x11F3A), (0x11F40, 0x11F40), (0x11F42, 0x11F42), (0x13430, 0x13440), (0x13447, 0x13455), (0x16AF0, 0x16AF4),
    (0x16B30, 0x16B36), (0x16F4F, 0x16F4F), (0x16F8F, 0x16F92), (0x16FE4, 0x16FE4), (0x1BC9D, 0x1BC9E),
    (0x1BCA0, 0x1BCA3), (0x1CF00, 0x1CF46), (0x1D167, 0x1D169), (0x1D173, 0x1D182), (0x1D185, 0x1D18B),
    (0x1D1AA, 0x1D1AD), (0x1D242, 0x1D244), (0x1DA00, 0x1DA36), (0x1DA3B, 0x1DA6C), (0x1DA75, 0x1DA75),
    (0x1DA84, 0x1DA84), (0x1DA9B, 0x1DA9F), (0x1DAA1, 0x1DAAF), (0x1E000, 0x1E02A), (0x1E08F, 0x1E08F),
    (0x1E130, 0x1E136), (0x1E2AE, 0x1E2AE), (0x1E2EC, 0x1E2EF), (0x1E4EC, 0x1E4EF), (0x1E8D0, 0x1E8D6),
    (0x1E944, 0x1E94A), (0xE0001, 0xE0001), (0xE0020, 0xE007F), (0xE0100, 0xE01EF)]
  # East Asian Wide and Fullwidth, plus emoji with default emoji presentation.
  Wide = [
    (0x1100, 0x115F), (0x231A, 0x231B), (0x2329, 0x232A), (0x23E9, 0x23EC), (0x23F0, 0x23F0),
    (0x23F3, 0x23F3), (0x25FD, 0x25FE), (0x2614, 0x2615), (0x2648, 0x2653), (0x267F, 0x267F),
    (0x2693, 0x2693), (0x26A1, 0x26A1), (0x26AA, 0x26AB), (0x26BD, 0x26BE), (0x26C4, 0x26C5),
    (0x26CE, 0x26CE), (0x26D4, 0x26D4), (0x26EA, 0x26EA), (0x26F2, 0x26F3), (0x26F5, 0x26F5),
    (0x26FA, 0x26FA), (0x26FD, 0x26FD), (0x2705, 0x2705), (0x270A, 0x270B), (0x2728, 0x2728),
    (0x274C, 0x274C), (0x274E, 0x274E), (0x2753, 0x2755), (0x2757, 0x2757), (0x2795, 0x2797),
    (0x27B0, 0x27B0), (0x27BF, 0x27BF), (0x2B1B, 0x2B1C), (0x2B50, 0x2B50), (0x2B55, 0x2B55),
    (0x2E80, 0x303E), (0x3041, 0x3247), (0x3250, 0x33FF), (0x3400, 0x4DBF), (0x4E00, 0xA4CF), (0xA960, 0xA97F),
    (0xAC00, 0xD7A3), (0xF900, 0xFAFF), (0xFE10, 0xFE19), (0xFE30, 0xFE6F), (0xFF00, 0xFF60),
    (0xFFE0, 0xFFE6), (0x16FE0, 0x16FE4), (0x16FF0, 0x16FF1), (0x17000, 0x18CD5), (0x18D00, 0x18D08), (0x1AFF0, 0x1B2FF), (0x1F004, 0x1F004),
    (0x1F0CF, 0x1F0CF), (0x1F18E, 0x1F18E), (0x1F191, 0x1F19A), (0x1F200, 0x1F202), (0x1F210, 0x1F23B),
    (0x1F240, 0x1F248), (0x1F250, 0x1F251), (0x1F260, 0x1F265), (0x1F300, 0x1F320), (0x1F32D, 0x1F335),
    (0x1F337, 0x1F37C), (0x1F37E, 0x1F393), (0x1F3A0, 0x1F3CA), (0x1F3CF, 0x1F3D3), (0x1F3E0, 0x1F3F0),
    (0x1F3F4, 0x1F3F4), (0x1F3F8, 0x1F43E), (0x1F440, 0x1F440), (0x1F442, 0x1F4FC), (0x1F4FF, 0x1F53D),
    (0x1F54B, 0x1F54E), (0x1F550, 0x1F567), (0x1F57A, 0x1F57A), (0x1F595, 0x1F596), (0x1F5A4, 0x1F5A4),
    (0x1F5FB, 0x1F64F), (0x1F680, 0x1F6C5), (0x1F6CC, 0x1F6CC), (0x1F6D0, 0x1F6D2), (0x1F6D5, 0x1F6D7),
    (0x1F6DC, 0x1F6DF), (0x1F6EB, 0x1F6EC), (0x1F6F4, 0x1F6FC), (0x1F7E0, 0x1F7EB), (0x1F7F0, 0x1F7F0),
    (0x1F90C, 0x1F93A), (0x1F93C, 0x1F945), (0x1F947, 0x1F9FF), (0x1FA70, 0x1FA7C), (0x1FA80, 0x1FA88),
    (0x1FA90, 0x1FABD), (0x1FABF, 0x1FAC5), (0x1FACE, 0x1FADB), (0x1FAE0, 0x1FAE8), (0x1FAF0, 0x1FAF8),
    (0x20000, 0x2FFFD), (0x30000, 0x3FFFD)]

proc inRanges(c: int, table: openArray[(int, int)]): bool =
  var lo = 0
  var hi = table.len - 1
  while lo <= hi:
    let mid = (lo + hi) div 2
    if c < table[mid][0]: hi = mid - 1
    elif c > table[mid][1]: lo = mid + 1
    else: return true

proc cellWidth(r: Rune): int =
  if r.int < 0x300: 1
  elif r.int.inRanges(ZeroWidth): 0
  elif r.int.inRanges(Wide): 2
  else: 1

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
    open: bool
    y, x0, x1: int

var maxDepth = 8
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
  for r in s.runes:
    if r.int < 0x20 or r.int in 0x7f .. 0x9f: result.add '?'
    else: result.add $r

proc fail(msg: varargs[string, `$`]) =
  stderr.writeLine "gree: " & msg.join("")
  quit(1)

proc dirsFirst(a, b: Node): int =
  if a.isDir != b.isDir: (if a.isDir: -1 else: 1) else: cmp(a.name, b.name)

proc scan(path, name: string, isDir: bool, depth = 0): Node =
  result = Node(name: clean(name), path: path, isDir: isDir, total: (if isDir: 0 else: 1))
  if not isDir: return
  try:
    if depth >= maxDepth:
      for _ in walkDir(path, checkDir = true): (result.cut = true; break)
      return
    for kind, p in walkDir(path, checkDir = true):
      let n = p.extractFilename
      if n[0] == '.' and not all: continue
      var c = scan(p, n, kind == pcDir, depth + 1)
      if c == nil: continue
      if kind in {pcLinkToDir, pcLinkToFile}:
        c.isLink = true
        try: c.link = clean(expandSymlink(p))
        except OSError: continue
        when defined(posix):
          var info: Stat
          c.broken = stat(p.cstring, info) != 0
        else:
          c.broken = not fileExists(p) and not dirExists(p)
      result.children.add c
      result.total += c.total
  except OSError:
    if not all: return nil
    result.denied = true
    return
  result.children.sort(dirsFirst)

proc kids(n: Node): seq[Node] =
  if n.open: n.children else: @[]

proc shownLeaves(n: Node): int =
  if n.more > 0: 0
  elif not n.isDir: (if n.x0 < W and n.x1 > 0: max(n.parts.len, 1) else: 0)
  else: n.kids.mapIt(it.shownLeaves).sum

proc hidden(n: Node): int =
  if not showHidden or not n.isDir: return 0
  result = n.total
  for c in n.kids:
    if c.more == 0: result -= c.total
  result = max(result, 0)

const
  DirColor = ansiStyleCode(styleBright) & ansiForegroundColorCode(fgBlue)
  ExecColor = ansiStyleCode(styleBright) & ansiForegroundColorCode(fgGreen)
  PipeColor = ansiForegroundColorCode(fgYellow)
  SocketColor = ansiStyleCode(styleBright) & ansiForegroundColorCode(fgMagenta)
  DeviceColor = ansiStyleCode(styleBright) & ansiForegroundColorCode(fgYellow)
  LinkColor = ansiStyleCode(styleBright) & ansiForegroundColorCode(fgCyan)
  BadColor = ansiStyleCode(styleBright) & ansiForegroundColorCode(fgRed)
  NumColor = ansiForegroundColorCode(fgYellow, bright = true)

proc tag(n: Node, dir: int): (string, string) =
  let hidden = n.hidden
  if hidden > 0: ((if dir > 0: "+" & $hidden else: $hidden & "+"), NumColor)
  elif n.denied: ("[denied]", BadColor)
  else: ("", "")

proc label(n: Node): string =
  if n.more > 0: return "+" & $n.more & " more"
  if n.isLink: return n.name & (if plain: "->" else: "→") & n.link
  result = n.name
  if n.isDir:
    let (text, _) = n.tag(1)
    if text.len > 0: result &= " " & text
    if not plain: result &= "  "

proc width(n: Node): int = n.label.cellWidth

proc setOpen(n: Node, v: bool) =
  n.open = v
  for c in n.children: c.setOpen(v)

proc height(n: Node): int =
  if n.kids.len == 0: 1 else: n.kids.mapIt(it.height).foldl(a + b)

proc pack(n: Node, x, budget: int) =
  let childX = x + n.width + Gap
  let limit = max(budget - childX, 12)
  var packed: seq[Node]
  var row = Node()
  for c in n.children:
    if c.isDir:
      c.pack(childX, budget); packed.add c
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

proc closedDirsAt(n: Node, depth, want: int, acc: var seq[Node]) =
  if depth == want:
    if n.isDir and n.children.len > 0 and not n.open: acc.add n
  else:
    for c in n.kids: c.closedDirsAt(depth + 1, want, acc)

proc trim(n: Node, keep: int) =
  if n.children.len > keep:
    let cut = n.children[keep .. ^1]
    n.children = n.children[0 ..< keep] & Node(more: cut.mapIt(max(it.parts.len, 1)).sum)

proc bump(h: var seq[int], i, by: int) =
  if h.len <= i: h.setLen(i + 1)
  h[i] += by

proc top(h: seq[int]): int =
  for i in countdown(h.len - 1, 0):
    if h[i] > 0: return i

proc openToFit(n: Node, rows, width: int) =
  if n.children.len > rows: n.trim(rows - 1)
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
  let cur = proc(): int =
    if dense > 0: farCnt.top else: colCnt[1 .. ^1].mapIt(it.top).filterIt(it > 0).mapIt(it + Gap).foldl(a + b, 0)
  let fits = proc(d: Node, depth: int): bool =
    let now = cur()
    d.toggle(depth, true)
    result = (unlimited or height + d.children.len - 1 <= rows) and cur() <= max(width, now)
    if result: height += d.children.len - 1 else: d.toggle(depth, false)
  n.x1 = 0; n.add(0, 1)
  for c in n.children:
    c.x0 = 0; c.x1 = Gap + c.width
    c.add(1, 1)
  for depth in 1 .. maxDepth:
    var cands: seq[Node]
    n.closedDirsAt(0, depth, cands)
    if cands.len == 0: break
    for d in cands: discard d.fits(depth)
  for depth in 1 .. maxDepth:
    var cands: seq[Node]
    n.closedDirsAt(0, depth, cands)
    for d in cands:
      let keep = if unlimited: d.children.len
                 else: min(d.children.len, rows - height + (if showHidden: 1 else: 0))
      let saved = d.children
      var widest = newSeq[int](keep)
      for i in 0 ..< keep: widest[i] = max(saved[i].width, if i > 0: widest[i - 1] else: 0)
      for limit in countdown(keep, 1):
        if limit < keep and widest[limit - 1] == widest[limit]: continue
        if showHidden: d.children = saved[0 ..< limit] else: d.trim(limit)
        if d.fits(depth): break
        d.children = saved

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
    let ok = if follow: stat(path.cstring, info) == 0 else: lstat(path.cstring, info) == 0
    if path.len > 0 and ok:
      let mode = info.st_mode
      if S_ISDIR(mode): return DirColor
      if S_ISFIFO(mode): return PipeColor
      if S_ISSOCK(mode): return SocketColor
      if S_ISBLK(mode) or S_ISCHR(mode): return DeviceColor
      if S_ISREG(mode) and (mode and Mode(S_IXUSR or S_IXGRP or S_IXOTH)) != 0: return ExecColor
  else:
    if path.len > 0:
      if dirExists(path): return DirColor
      if fileExists(path) and path.splitFile.ext.toUpperAscii in exeExts: return ExecColor
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
  if n.parts.len > 0:
    for i, p in n.parts:
      if i > 0: cs.add [" ", " "]
      cs.add p.leafCells(dir)
  elif not n.isDir:
    cs = n.leafCells(dir)
  else:
    let icon = if plain: @[] else: @[if n.children.len > 0 or n.cut or n.denied: "📁" else: "📂", ""]
    let name = cells(n.name, DirColor)
    let (text, code) = n.tag(dir)
    let tag = cells(text, code)
    cs = if tag.len == 0: (if dir > 0: icon & name else: name & icon)
         elif dir > 0: icon & name & @[" "] & tag
         else: tag & @[" "] & name & icon
  let dots = cells(if plain: "..." else: "…", "")
  var x = n.x0
  if x + cs.len > W:
    let keep = max(W - x - dots.len, 0)
    if keep > 0 and cs[keep].len == 0: cs[keep - 1] = " "
    cs = cs[0 ..< keep] & dots
    if color: cs[^1] &= ansiResetCode
  elif x < 0:
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
    cs = dots & cs[start .. ^1]
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
  for y in y0 .. y1:
    put(cx, y, if y0 == y1: "─"
               elif y == y0: first
               elif y == y1: last
               elif y in rows: branch
               else: "│")
  if y0 != y1:
    put(cx, n.y, if n.y == y0: (if n.y in rows: "┬" elif dir > 0: "┐" else: "┌")
                 elif n.y == y1: (if n.y in rows: "┴" elif dir > 0: "┘" else: "└")
                 elif n.y in rows: "┼"
                 else: stem)
  let (sx0, sx1) = if dir > 0: (n.x1, cx - 1) else: (cx + 1, n.x0 - 1)
  for x in sx0 .. sx1: put(x, n.y, "─")
  for c in n.kids:
    put((if dir > 0: c.x0 - 1 else: c.x1), c.y, "─")
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
  -p  plain: no folder emoji, folders are told apart by color only
  -a  include hidden entries (names starting with a dot)
  -u  unlimited height: show everything the width allows, for piping into less or a file
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
          var num = 0
          try: num = v.parseInt
          except ValueError: discard
          if num < 1:
            fail "-", ch, " needs a number"
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

proc splitBalanced(tree: Node, cap: int): (Node, Node) =
  var left = Node(name: tree.name, isDir: true, open: true)
  var right = Node(name: tree.name, isDir: true, open: true)
  var order = tree.children.mapIt((min(it.height, cap), it))
  order.sort(proc(a, b: auto): int = cmp(b[0], a[0]))
  var hL = 0; var hR = 0
  for (h, c) in order:
    if hR <= hL: right.children.add c; hR += h
    else: left.children.add c; hL += h
  for s in [left, right]: s.children.sort(dirsFirst)
  (left, right)

proc main() =
  let (side, root) = parseArgs()
  var path = root.absolutePath
  when defined(windows): # resolve drive-relative forms like C:foo against that drive's current directory
    try: path = root.expandFilename
    except OSError: fail "not a directory: ", clean(root)
  path = path.normalizedPath.strip(leading = false, chars = {DirSep, AltSep})
  if path.len == 0: path = $DirSep
  elif defined(windows) and path[^1] == ':': path.add DirSep
  if not dirExists(path): fail "not a directory: ", clean(root)
  let tree = scan(path, if root == ".": getCurrentDir().extractFilename else: path.extractFilename, true)
  if tree == nil or tree.denied: fail "permission denied: ", clean(root)
  if tree.name.len == 0: tree.name = path
  if lines > 0: unlimited = false
  W = terminalWidth()
  let termH = terminalHeight() - (if showInfo: 3 else: 2)
  let rows = if unlimited: int.high div 2 elif lines > 0: lines - (if showInfo: 1 else: 0) else: termH
  if rows < 1:
    fail if lines > 0: "-H must be at least " & $(if showInfo: 2 else: 1) else: "terminal too small"
  tree.setOpen(true)
  let rootLen = tree.width

  var sides: seq[(Node, int)]
  if side == 0:
    let (left, right) = tree.splitBalanced(rows div 4)
    sides = @[(right, 1), (left, -1)]
  else:
    sides = @[(Node(name: tree.name, isDir: true, open: true, children: tree.children), side)]

  let budget = if side == 0: W div 2 - rootLen div 2 - 2 else: W - rootLen - 2
  let rootX0 = if side == 0: W div 2 - rootLen div 2
               elif side > 0: 0 else: W - rootLen
  for (s, dir) in sides:
    for c in s.children: c.setOpen(false)
    if dense > 1: s.pack(-rootLen, budget)
    s.openToFit(rows, budget)
  let tall = sides.mapIt(it[0].height).max
  H = if fill and not unlimited: max(termH, tall) else: rows - (rows - tall) div 2 * 2
  grid = newSeqWith(H, newSeqWith(W, " "))
  for (s, dir) in sides:
    let top = if valign > 0: 0 elif valign < 0: H - s.height else: (H - s.height) div 2
    discard s.layout(0, s.colWidths, top, dir, if dir > 0: rootX0 else: rootX0 + rootLen)
  let ry = sides.mapIt(it[0].y).foldl(a + b) div sides.len
  for (s, dir) in sides:
    s.y = ry
    s.draw(dir)
  tree.x0 = sides[^1][0].x0; tree.y = ry
  tree.putLabel(sides[^1][1])

  let used = if fill: @[0, H - 1] else: toSeq(0 ..< H).filterIt(grid[it].anyIt(it != " "))
  if used.len > 0:
    for y in used[0] .. used[^1]: echo grid[y].join("").strip(leading = false)
  if showInfo:
    let shown = sides.mapIt(it[0].shownLeaves).foldl(a + b)
    let (dim, reset) = if color: (ansiStyleCode(styleDim), ansiResetCode) else: ("", "")
    echo dim, shown, " of ", tree.total, " leaves shown", reset

main()
