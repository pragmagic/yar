import strutils

type
  HugeArray{.unchecked.}[T] = array[0, T]
  FlexArray[T] = ptr HugeArray[T]

type
  AnyKind* = enum
    akObject
    akInt
    akBlock
    akContext
    akWord
    akSetWord
    akGetWord
    akQuote

  Type* = enum
    tNativeFunc
    tUserFunc
    tVNode
    tSignal
    tString
    tPath
    tRefinement
    tTemplate
    tView

  # Type = ptr TypeObj
  # TypeObj = object
  #   id: TypeId
  #   exec: proc(vm: VM, o: Obj): Any {.nimcall.}

  Symbol* = distinct cstring
    
  # ANY value can be a pointer to a Cell (Object) or small integer, pointer is parametrized e.g. set/get word
  Any* = distinct int 
  # Word is a pointer to a PairObj, which means all words are bound to a context, as well as there is an
  # associated value
  Word* = Any
  
  List[T] = ptr ListObj[T]
  ListObj[T] = object
    nxt: List[T]
    val*: T

  Obj* = ptr ObjObj
  ObjObj = object
    typ*: Type
    val*: int # semantic depends on object type

  Pair = ptr PairObj
  PairObj = object
    sym*: Symbol
    val*: Any

  UserFunc* = ptr UserFuncObj
  UserFuncObj = object
    spec*: UserFuncSpec
    ctx*: Context

  UserFuncSpec* = ptr UserFuncSpecObj
  UserFuncSpecObj = object
    body*: Block
    spec: Block
  
  Block* = List[Any]
  Context* = List[Pair]
  #Path* = List[Any]
  
  Signal = ptr SignalObj
  SignalObj = object
    val: Any
    deps: int

  VNodeContent = object
    attr*: Context
    kids*: Block

  VNode* = ptr VNodeObj
  VNodeObj = object
    tag*: Any # Quote
    content*: ptr VNodeContent

  Template = ptr TemplateObj
  TemplateObj = object
    params: Block
    body: Block

  View* = ptr ViewObj
  ViewObj = object
    params*: Context
    body*: Block
  
  CellAllocator = object
    mem: pointer
    last: int

  IP = List[Any]
  NativeFunc = proc (vm: VM): Any {.nimcall.}  
    
  VM* = ptr VMObj
  VMObj = object
    ip: IP
    sp: ptr Any
    rsp: ptr IP
    cellAllocator: CellAllocator
    #types: ptr array[TypeId, TypeObj]    
    symbols: FlexArray[char]
    system: Context
    user: Context
    bools: array[bool, Any]
    stack: pointer    
    rstack: pointer    
    unbound: Any

# const
#   None = 0.Any

###
#
# C O N V E R T E R S
#
###

proc toAny*(obj: Obj): Any {.inline.} = Any(cast[int](obj))
proc toAny*(i: int): Any {.inline.} = Any((i shl 3) or akInt.int)
proc toAny*(blk: Block): Any {.inline.} = Any(cast[int](blk) or akBlock.int)
proc toAny*(ctx: Context): Any {.inline.} = Any(cast[int](ctx) or akContext.int)

proc vmcast*[T: Block | Context](a: Any): T {.inline.} = cast[T](a.int and (not 7))
proc vmcast*[T: int](a: Any): T {.inline.} = a.int shr 3
proc vmcast*[T: Obj](a: Any): T {.inline.} = cast[T](a)
  
proc unbox*[T](o: Obj, typ: Type): T {.inline.} = 
  assert o.typ == typ
  result = cast[T](o.val)

proc bools*(vm: VM, b: bool): Any {.inline.} = vm.bools[b]
proc unbound*(vm: VM): Any {.inline.} = vm.unbound
proc user*(vm: VM): Context {.inline.} = vm.user

###
#
# U T I L S
#
###

proc val*(pair: Pair): Any {.inline.} = pair.val

proc exec*(vm: VM): Any # forward decl
proc execAll*(vm: VM, blk: Block): Any # forward decl

proc `==`*(a, b: Any): bool {.borrow.}
proc `==`*(a, b: Symbol): bool {.inline.} = a.pointer == b.pointer
proc `$`(x: Symbol): string = $cstring(x)

proc makeWord(e: Pair, kind: AnyKind): Word = 
  Word(cast[int](e) +% kind.int)

proc entry*(w: Word): Pair {.inline.} = cast[Pair](w.int and (not (sizeof(PairObj) - 1))) # pair is PairObj size aligned
proc sym*(w: Word): Symbol {.inline.} = w.entry.sym
proc kind*(w: Any): AnyKind {.inline.} = AnyKind(w.int and 7)

proc isObj*(w: Any): bool = w.kind == akObject

iterator items*[T](list: List[T]): T =
  var i = list
  while i != nil:
    yield i.val
    i = i.nxt

proc len[T](list: List[T]): int =
  var i = list
  while i != nil:
    inc result
    i = i.nxt

iterator mitems*[T](list: List[T]): var T =
  var i = list
  while i != nil:
    yield i.val
    i = i.nxt

proc `$`*(blk: Block): string
proc `$`*(ctx: Context): string
proc `$`(obj: Obj): string

proc `$`*(w: Any): string =
  let entry = w.entry
  result = case w.kind
    of akWord: $entry.sym
    of akQuote: '\'' & $entry.sym
    of akGetWord: ':' & $entry.sym
    of akSetWord: $entry.sym & ':'
    of akInt: $vmcast[int](w)
    of akBlock: $vmcast[Block](w)
    of akContext: $vmcast[Context](w)
    of akObject: $vmcast[Obj](w)

proc `$`(obj: Obj): string =
  if obj == nil:
    return "None"
  case obj.typ
  of tString: 
    result = '"' & $cast[cstring](obj.val) & '"'
  of tVNode:
    let vnode = cast[ptr VNodeObj](obj.val)
    result = "<"
    result.add $vnode.tag
    result.add ' '
    result.add $vnode.content.attr
    result.add ' '
    result.add $vnode.content.kids
    result.add '>'  
  of tPath:
    var path = cast[List[Any]](obj.val)
    var slash = false
    result = ""
    for i in path:
      if slash: result.add '/' else: slash = true
      result.add $i
  else: 
    result = "<object/" & $obj.typ & " @ " & toHex(cast[int](obj)) & '>'

###
#
# A L L O C A T O R S
#
###

const CellMemSize = 1024 * 1024

proc alignWord(adr: int, align: int): int = (adr +% (align - 1)) and (not (align - 1))

proc init(allocator: var CellAllocator) =
  allocator.mem = alloc(CellMemSize)
  allocator.last = alignWord(cast[int](allocator.mem), sizeof(int) * 2)

proc used(allocator: CellAllocator): int =
  allocator.last -% cast[int](allocator.mem)
  
proc alloc(allocator: var CellAllocator, T: typedesc): ptr T =
  assert sizeof(T) == (sizeof(int) * 2)
  result = cast[ptr T](allocator.last)
  inc allocator.last, sizeof(T)

proc alloc(vm: VM, T: typedesc): ptr T = alloc(vm.cellAllocator, T)

proc allocObj(vm: VM, typ: Type, val: int): Obj = 
  result = alloc(vm.cellAllocator, ObjObj)
  result.typ = typ # addr vm.types[typ]
  result.val = val

proc allocObj(vm: VM, typ: Type, val: pointer): Obj = allocObj(vm, typ, cast[int](val))

proc copy(vm: VM, p: Pair): Pair = 
  result = vm.alloc(PairObj)
  result.sym = p.sym
  result.val = p.val

###
#
# S T R I N G
#
###

proc createString(vm: VM, str: cstring, len: int): Any =
  let data = alloc(len + 1)
  copyMem(data, str, len + 1)
  result = vm.allocObj(tString, data).toAny

###
#
# S I G N A L
#
###

proc createSignal*(vm: VM, val: Any): Any =
  let signal = vm.alloc(SignalObj)
  signal.val = val
  echo "created signal with value: ", val
  result = vm.allocObj(tSignal, signal).toAny

###
#
# U S E R   F U N C
#
###

proc createUserFunc*(vm: VM, ctx: Context, specBlock: Block, body: Block): Any =
  let spec = vm.alloc(UserFuncSpecObj)
  spec.spec = specBlock
  spec.body = body
  let uf = vm.alloc(UserFuncObj)
  uf.ctx = ctx
  uf.spec = spec
  result = vm.allocObj(tUserFunc, uf).toAny

###
#
# B L O C K
#
###

proc `$`(blk: Block): string =
  var space = false
  result = "[";
  for i in blk:
    if space: result.add ' ' else: space = true
    result.add $i
  result.add ']'  

###
#
# C O N T E X T
#
###

type
  ContextBuilder* = object
    tail: List[Pair]
    head: List[Pair]

proc toAny*(builder: ContextBuilder): Any {.inline.} = builder.head.toAny

proc add*(vm: VM, builder: var ContextBuilder, p: Pair) =
  let item = vm.alloc(ListObj[Pair])
  item.val = vm.copy(p)
  if builder.tail != nil:
    builder.tail.nxt = item
    builder.tail = item
  else:
    builder.head = item
    builder.tail = item

proc findWord*(ctx: Context, sym: Symbol): Pair =
  for w in ctx:
    if w.sym == sym:
      return w

proc addWord*(vm: VM, ctx: var Context, sym: Symbol, val: Any): Pair =
  result = vm.alloc(PairObj)
  result.sym = sym
  result.val = val
  let item = vm.alloc(ListObj[Pair])
  item.val = result
  item.nxt = ctx
  ctx = item

proc reverse*(ctx: var Context) =
  var 
    current: Context = nil
    entry = ctx

  while entry != nil:
    let nxt = entry.nxt
    entry.nxt = current
    current = entry
    entry = nxt

  ctx = current

proc bindTo*(blk: Block, ctx: Context) = 
  for i in mitems(blk):
    case i.kind
    of akWord, akSetWord, akGetWord, akQuote:
      let w = i.Word
      for e in ctx:
        if e.sym == w.sym:
          i = makeWord(e, w.kind)
    of akBlock:
      bindTo(vmcast[Block](i), ctx)
    else:
      discard

proc createContext*(vm: VM, blk: Block): Any =
  var ctx: Context
  for i in blk:
    if i.kind == akSetWord:
      discard vm.addWord(ctx, i.sym, 0.toAny)
        
  blk.bindTo(ctx)
  discard vm.execAll(blk)
  result = ctx.toAny

proc copy*(vm: VM, ctx: Context): Context =
  var builder: ContextBuilder
  for i in ctx:
    vm.add(builder, i)
  result = builder.head

proc createReactiveCtx*(vm: VM, blk: Block): Any =
  var ctx: Context
  for i in blk:
    if i.kind == akSetWord:
      discard vm.addWord(ctx, i.sym, vm.createSignal(0.toAny))
        
  blk.bindTo(ctx)
  discard vm.execAll(blk)
  result = ctx.toAny
  
proc `$`*(ctx: Context): string =
  var space = false
  result = "[";
  for entry in ctx:
    if space: result.add ' ' else: space = true
    result.add $entry.sym
    result.add ": "
    result.add $entry.val
  result.add ']'

###
#
# V N O D E 
#
###

proc createVNode*(vm: VM, tag: Word, attr: Context, kids: Block): Any =
  assert tag.kind == akQuote
  let vnode = vm.alloc(VNodeObj)
  vnode.tag = tag
  let content = vm.alloc(VNodeContent)
  content.attr = attr
  content.kids = kids
  vnode.content = content  
  result = vm.allocObj(tVNode, vnode).toAny

###
#
# T E M P L A T E
#
###

proc createTemplate*(vm: VM, params: Block, body: Block): Any =
  let tmpl = vm.alloc(TemplateObj)
  tmpl.params = params
  tmpl.body = body
  result = vm.allocObj(tTemplate, tmpl).toAny

###
#
# T Y P E S
#
###

proc push*(vm: VM, a: Any) {.inline.}
proc pop*(vm: VM): Any {.inline.}

#proc tExecNative(vm: VM, o: Obj): Any = cast[NativeFunc](o.val)(vm) 


#proc tExecValue(vm: VM, o: Obj): Any = o.toAny
      
# const
#   types: array[TypeId, TypeObj] = [
#     TypeObj(exec: tExecNative, id: tNativeFunc),
#     TypeObj(exec: tExecUser, id: tUserFunc),
#     TypeObj(exec: tExecValue, id: tVNode),
#     TypeObj(exec: tExecSignal, id: tSignal),
#     ]

###
#
# V I R T U A L   M A C H I N E
#
###

const 
  StackSize = 4096
  
proc push(vm: VM, a: Any) =
  vm.sp = cast[ptr Any](cast[int](vm.sp) -% sizeof(Any))
  vm.sp[] = a

proc pop(vm: VM): Any = 
  result = vm.sp[]
  vm.sp = cast[ptr Any](cast[int](vm.sp) +% sizeof(Any))

proc pushIP(vm: VM) {.inline.} =
  vm.rsp = cast[ptr IP](cast[int](vm.rsp) -% sizeof(IP))
  vm.rsp[] = vm.ip

proc popIP(vm: VM) {.inline.} = 
  vm.ip = vm.rsp[]
  vm.rsp = cast[ptr IP](cast[int](vm.rsp) +% sizeof(IP))

proc findOrAddSymbol*(vm: VM, ident: cstring): Symbol =
  assert(ident.len > 0)
  var 
    i = 0
    j = 0

  while vm.symbols[i] != 0.char:
    let sym = i
    j = 0
    while (vm.symbols[i] == ident[j]) and (vm.symbols[i] != 0.char) and (ident[j] != 0.char):
      inc i
      inc j

    if vm.symbols[i] == 0.char and ident[j] == 0.char:
      return Symbol(addr(vm.symbols[sym])) # EQUALS

    while vm.symbols[i] != 0.char:
      inc i    # SKIP TO NEXT SYMBOL
    inc i

  echo "adding symbol: ", ident, " (", len ident, ")"
    
  # add here
  let sym = i
  j = 0
  while ident[j] != 0.char:
    vm.symbols[i] = ident[j]
    inc i
    inc j

  vm.symbols[i] = 0.char
  inc i
  vm.symbols[i] = 0.char
  result = Symbol(addr(vm.symbols[sym]))

proc findOrAddWord(vm: VM, ctx: var Context, sym: Symbol, kind: AnyKind): Word =
  var e = findWord(ctx, sym)
  if e == nil:
    e = findWord(vm.system, sym)
  if e == nil:
    e = vm.addWord(ctx, sym, vm.unbound)

  result = makeWord(e, kind)

proc showMemory*(vm: VM) =
  var i = 0
  while true:
    if vm.symbols[i] == 0.char and vm.symbols[i+1] == 0.char: 
      break
    inc i
  echo "Symbols: ", (i+1), " bytes"
  echo "Cells: ", vm.cellAllocator.used, " bytes"

proc addNative*(vm: VM, name: cstring, code: NativeFunc) =
  let nf = vm.allocObj(tNativeFunc, code)
  discard vm.addWord(vm.system, vm.findOrAddSymbol(name), nf.toAny)
  
proc nf_unbound(vm: VM): Any =
  quit("word not bound")
  
proc createVM*(): VM =
  result = create VMObj
  result.cellAllocator.init
  result.symbols = cast[FlexArray[char]](alloc(65536))  
  # Stacks
  result.stack = alloc(StackSize)
  result.sp = cast[ptr Any](cast[int](result.stack) +% StackSize)
  result.rstack = alloc(StackSize)
  result.rsp = cast[ptr IP](cast[int](result.rstack) +% StackSize)
  # Types
  # result.types = create array[TypeId, TypeObj]
  # result.types[] = types
  result.unbound = result.allocObj(tNativeFunc, nf_unbound).toAny 
  echo "creating VM constants..."
  result.bools[false] = 0.toAny
  result.bools[true] = 1.toAny

###
#
# P A R S E R
#
###

type 
  Ident = array[128, char]

  ListBuilder[T] = object
    tail: List[T]
    head*: List[T]

  BlockBuilder* = ListBuilder[Any]

proc toAny*(builder: ListBuilder): Any {.inline.} = builder.head.toAny

proc add*[T](vm: VM, builder: var ListBuilder[T], a: T) =
  let item = vm.alloc(ListObj[T])
  item.val = a
  if builder.tail != nil:
    builder.tail.nxt = item
  else:
    builder.head = item
  builder.tail = item

proc next(str: var cstring) =
  str = cast[cstring](cast[int](str) +% 1)

      
proc parse(vm: VM, str: var cstring): Block = 
  var builder: BlockBuilder
  var code = str

  while true:
    case code[0]
    of char(0):
      break
    of Whitespace: 
      next code
    of ']': 
      next code
      break
    of '[':
      next code
      vm.add builder, vm.parse(code).toAny      
    of Digits:
      var val = 0
      while true:
        val = val * 10 + (code[0].int - '0'.int)
        next code
        if not (code[0] in Digits):
          break
      
      vm.add builder, val.toAny

    of '"':
      var str {.noinit.}: Ident
      var i = 0
      while true:
        next code
        if code[0] == '"':
          break
        str[i] = code[0]
        inc i
        if i == sizeof(str):
          assert false, "buffer overflow"        

      next code
      str[i] = 0.char

      vm.add builder, vm.createString(addr str[0], i)

    of '/':
      # refinement
      var ident {.noinit.}: Ident 
      var i = 0 
      while true:
        next code
        if not (code[0] in Whitespace):
          ident[i] = code[0]
          inc i
        else:
          ident[i] = 0.char
          break
      let sym = vm.findOrAddSymbol(addr ident[0])
      let refinement = vm.allocObj(tRefinement, sym.pointer)
      vm.add builder, refinement.toAny

    else:
      var ident {.noinit.}: Ident 
      var i = 0 
      var kind = akWord
      var path = false
      var pathBuilder: BlockBuilder
      
      if code[0] == '\'': 
        kind = akQuote
        next code
      elif code[0] == ':':
        kind = akGetWord
        next code
      
      while not (code[0] in Whitespace + {'[', ']', '(', ')', '{', '}', ':', ';', 0.char}):
        if code[0] == '/':
          assert i > 0
          ident[i] = 0.char
          path = true
          let sym = vm.findOrAddSymbol(addr ident[0])
          let word = vm.findOrAddWord(vm.user, sym, akWord)
          vm.add pathBuilder, word
          echo "path added: ", word
          i = 0
        else:              
          ident[i] = code[0]
          inc i
        if i == sizeof(ident):
          assert false, "buffer overflow"
        next code

      ident[i] = char(0)
      if code[0] == ':':
        kind = akSetWord
        next code

      let sym = vm.findOrAddSymbol(addr ident[0])
      let word = vm.findOrAddWord(vm.user, sym, kind)
      # echo "word: ", sym, 
      #      " \t", toHex(cast[int](sym)), 
      #      " \t", toHex(word.int), 
      #      " \t", word
      
      if path:
        assert kind in {akWord, akSetWord}
        vm.add pathBuilder, word
        let pathObj = vm.allocObj(tPath, pathBuilder.head)
        vm.add builder, pathObj.toAny
      else:       
        vm.add builder, word

  str = code
  result = builder.head

proc parse*(vm: VM, str: cstring): Block = 
  var code = str
  result = vm.parse code

###
#
# E X E C
#
###

iterator execute*(vm: VM, blk: Block): Any =
  vm.pushIP
  vm.ip = blk
  while vm.ip != nil:
    yield vm.exec
  vm.popIP
  
proc tExecUser(vm: VM, o: Obj): Any {.noinline.} = 
  let uf = cast[UserFunc](o.val)
  let ctx = uf.ctx

  for entry in ctx:
    vm.push entry.val
    entry.val = vm.exec
  result = vm.execAll(uf.spec.body)
  for entry in ctx:
    entry.val = vm.pop

proc tExecUserRefinement(vm: VM, o: Obj, rf: Symbol): Any {.noinline.} = 
  echo "execute with refinement: ", rf, " ", toHex(cast[int](rf))

  let uf = cast[UserFunc](o.val)
  let ctx = uf.ctx
  let spec = uf.spec.spec

  echo "ctx len: ", ctx.len, " spec len: ", spec.len 

  var ctxEntry: List[Pair] = ctx
  var specEntry: List[Any] = spec
  var readParam = true

  while specEntry != nil:
    vm.push ctxEntry.val.val
    if specEntry.val.isObj:
      let r = cast[Obj](specEntry.val)
      assert r.typ == tRefinement
      let sym = cast[Symbol](r.val)
      echo "param refinement: ", sym, " ", toHex(cast[int](sym))
      if sym == rf:
        echo "matched refinement"
        ctxEntry.val.val = vm.bools[true]
        readParam = true
      else:
        echo "unmatched refinement"
        readParam = false
    else:
      # just parameter
      if readParam:
        echo "."
        ctxEntry.val.val = vm.exec
      else:
        ctxEntry.val.val = vm.bools[false]
        
    ctxEntry = ctxEntry.nxt
    specEntry = specEntry.nxt

  # for entry in ctx:
  #   vm.push entry.val
  #   entry.val = vm.exec
  
  result = vm.execAll(uf.spec.body)

  # restore entire context
  for entry in ctx:
    entry.val = vm.pop
  
proc tExecSignal(o: Obj): Any {.noinline.} = 
  echo "accessing signal: ", cast[ptr SignalObj](o.val).val
  result = o.toAny

proc tExecPath(vm: VM, o: Obj): Any {.noinline.} = 
  assert o.typ == tPath
  var path = cast[List[Any]](o.val)
  let word = path.val
  assert word.kind == akWord
  result = word.entry.val
  while true:
    case result.kind
    of akContext:
      path = path.nxt
      if path == nil:
        break
      result = findWord(vmcast[Context](result), path.val.sym).val
    of akObject:
      let o = cast[Obj](result)
      assert o.typ == tUserFunc
      echo "execute function with refinement"
      path = path.nxt
      assert path != nil
      echo "refinement: ", path.val.sym
      assert path.nxt == nil
      result = tExecUserRefinement(vm, o, path.val.sym)
    else:
      assert path.nxt == nil
      echo "path returns ", result
      break

#var renderer*: proc (vm: VM, view: Block) {.nimcall.}

proc tExecTemplate(vm: VM, o: Obj): Any {.noinline.} = 
  assert o.typ == tTemplate
  echo "instantiating template"
  var params: Context
  let tmpl = cast[Template](o.val)
  for i in tmpl.params:
    discard vm.addWord(params, i.sym, vm.exec)

  tmpl.body.bindTo params

  let view = vm.alloc(ViewObj)
  view.params = params
  view.body = tmpl.body # copy

  result = vm.allocObj(tView, view).toAny


proc tSetSignal(vm: VM, o: Obj, val: Any): Any {.noinline.} =
  echo "setting signal to ", val
  cast[ptr SignalObj](o.val).val = val
  result = val

proc execObj(vm: VM, o: Obj): Any =
  case o.typ
  of tNativeFunc: cast[NativeFunc](o.val)(vm) 
  of tUserFunc: vm.tExecUser(o)
  of tSignal: tExecSignal(o)
  of tPath: vm.tExecPath(o)
  of tTemplate: vm.tExecTemplate(o)
  else: o.toAny

proc execW(vm: VM, word: Word): Any =
  result = word.entry.val
  if result.kind == akObject:
    result = vm.execObj(cast[Obj](result))

proc execSW(vm: VM, word: Word): Any = 
  let val = vm.exec
  let e = word.entry
  if e.val.kind == akObject:
    let o = cast[Obj](e.val)
    if o.typ == tSignal:
      return tSetSignal(vm, o, val)        
  e.val = val
  result = val

# take a value at IP without execution
# advance IP
proc readIP*(vm: VM): Any {.inline.} =
  result = vm.ip.val
  vm.ip = vm.ip.nxt

proc exec(vm: VM): Any = 
  result = vm.readIP
  case result.kind
  of akSetWord:
    result = vm.execSW(result)
  of akGetWord:
    result = result.entry.val
  of akWord:
    result = vm.execW(result)
  of akObject:
    result = vm.execObj(cast[Obj](result))
  else:
#    echo "not covered outer: ", result.kind    
    discard

proc execAll(vm: VM, blk: Block): Any = 
  for i in execute(vm, blk):
    result = i

