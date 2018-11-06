import core

proc nf_print(vm: VM): Any {.nimcall.} =
  result = vm.exec
  echo "printing kind: ", result.kind
  echo "print: ", result 

proc nf_add(vm: VM): Any =
  let x = vmcast[int](vm.exec)
  let y = vmcast[int](vm.exec)
  result = toAny(x + y)

proc nf_sub(vm: VM): Any =
  let x = vmcast[int](vm.exec)
  let y = vmcast[int](vm.exec)
  result = toAny(x - y)
  
proc nf_lt(vm: VM): Any =
  let x = vmcast[int](vm.exec)
  let y = vmcast[int](vm.exec)
  result = vm.bools(x < y)

proc nf_gt(vm: VM): Any =
  let x = vmcast[int](vm.exec)
  let y = vmcast[int](vm.exec)
  result = vm.bools(x > y)
  
proc nf_while(vm: VM): Any =
  let cond = vmcast[Block](vm.exec)
  let body = vmcast[Block](vm.exec)
  while true:
    let c = vm.execAll(cond)
    if c == vm.bools(false):
      break
    result = vm.execAll(body)
  
proc nf_either(vm: VM): Any =
  let cond = vmcast[int](vm.exec)
  let thenBlock = vmcast[Block](vm.exec)
  let elseBlock = vmcast[Block](vm.exec)
  result = vm.execAll(if cond == 0: elseBlock else: thenBlock)

proc nf_func(vm: VM): Any =
  let head = vmcast[Block](vm.exec)
  var ctx: Context
  for i in head:
    case i.kind
    of akWord:
      discard vm.addWord(ctx, i.Word.sym, vm.unbound)
    of akObject:
      let o = cast[Obj](i)
      let sym = unbox[Symbol](o, tRefinement)
      discard vm.addWord(ctx, sym, vm.bools(false))
    else:
      assert false, "bad stuff in function spec"

  let body = vmcast[Block](vm.exec)
  ctx.reverse
  body.bindTo(ctx)
  result = vm.createUserFunc(ctx, head, body)
  
# proc nf_func_exp(vm: VM): Any =
#   let head = vmcast[Block](vm.exec)
#   var all: Context # map with all refinements
#   var ctx: Context # shared spec
#   var refinement: Context
#   var refSym: Symbol
#   var cur: ptr Context # current (context or refinement)

#   let body = vmcast[Block](vm.exec)
#   cur = addr ctx
#   for i in head:
#     case i.kind 
#     of akWord:
#       discard vm.addWord(cur[], i.Word.sym, vm.unbound)
#     of akObject:
#       if refinement != nil:
#         # save previous refinement
#         refinement.reverse
#         body.bindTo(ctx)
#         echo "create refinement: ", refinement
#         discard vm.addWord(all, refSym, vm.createUserFunc(refinement, body))
#       # create refinement
#       refinement = vm.copy(ctx)
#       let o = vmcast[Obj](i)
#       refSym = unbox[Symbol](o, tRefinement)
#       cur = addr refinement
#     else:
#       assert false, "uncovered"
          
#   ctx.reverse
#   body.bindTo(ctx)
#   if all == nil: 
#     # no refinements
#     result = vm.createUserFunc(ctx, body)
#   else:
#     discard vm.addWord(all, "default".Symbol, vm.createUserFunc(ctx, body))
#     echo "creating multifunc: ", all.toAny
#     result = all.toAny
  
proc nf_makeObject(vm: VM): Any = vm.createContext(vmcast[Block](vm.exec))

proc nf_compose(vm: VM): Any =
  var composed: BlockBuilder
  let blk = vmcast[Block](vm.exec)
  for i in execute(vm, blk):
    vm.add composed, i
  result = composed.toAny

proc nf_makeVNode(vm: VM): Any =
  let tag = vm.exec
  let attr = vmcast[Context](vm.exec)
  let kids = vmcast[Block](vm.exec)
  result = vm.createVNode(tag, attr, kids)

proc nf_signal(vm: VM): Any = vm.createSignal(vm.exec)

proc nf_react(vm: VM): Any =
  let body = vmcast[Block](vm.exec)
  result = vm.createReactiveCtx(body)
  
proc nf_true(vm: VM): Any = vm.bools(true)

proc nf_map(vm: VM): Any =
  let w = vm.readIP
  let obj = cast[Obj](w.entry.val)
  let uf = cast[UserFunc](obj.val)
  let blk = vmcast[Block](vm.exec)
  var mapped: BlockBuilder
  let ctx = uf.ctx
  
  for i in blk:  
    vm.push ctx.val.val
    ctx.val.val = i
  
    vm.add mapped, vm.execAll(uf.spec.body)

    ctx.val.val = vm.pop
  
  result = mapped.head.toAny

proc nf_foreach(vm: VM): Any =
  let blk = vmcast[Block](vm.exec)
  let w = vm.readIP
  let code = vmcast[Block](vm.exec)

  var ctx: Context
  let pair = vm.addWord(ctx, w.sym, 0.toAny)
  code.bindTo ctx
  
  for i in blk:  
    pair.val = i
    discard vm.execAll(code)
  
  result = 0.toAny
    
const
  nativeFuncs = [
    (name: "print".cstring, code: nf_print),
    (name: "add".cstring, code: nf_add),
    (name: "sub".cstring, code: nf_sub),
    (name: "lt".cstring, code: nf_lt),
    (name: "gt".cstring, code: nf_gt),
    (name: "while".cstring, code: nf_while),
    (name: "either".cstring, code: nf_either),
    (name: "func".cstring, code: nf_func),
    (name: "make-object".cstring, code: nf_makeObject),
    (name: "compose".cstring, code: nf_compose),
    (name: "make-vnode".cstring, code: nf_makeVNode),
    (name: "signal".cstring, code: nf_signal),
    (name: "react".cstring, code: nf_react),
    (name: "true".cstring, code: nf_true),
    (name: "map".cstring, code: nf_map),
    (name: "foreach".cstring, code: nf_foreach),
    ]

const userModule = """
does: func [body] [func [] body]

header: func [attr kids] [make-vnode 'header make-object attr compose kids]
h1: func [attr kids] [make-vnode 'h1 make-object attr compose kids]
div: func [attr kids] [make-vnode 'div make-object attr compose kids]
input: func [attr] [make-vnode 'input make-object attr []]
""".cstring    

proc boot*(vm: VM) =
  echo "creating native functions..."
  for f in nativeFuncs:
    vm.addNative(f.name, f.code)
  echo "loading user module..."
  discard vm.execAll(vm.parse userModule)  
