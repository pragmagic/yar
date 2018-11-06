import core
import boot

###
#
# T E S T
#
###

var vm = createVM()
vm.boot
#vm.bootDOM

#let test = "a: 42"
let test = "print 42"
#let test = "hey: 24 print hey print add 5 hey print lt 2 3 print lt 3 2"
#let test = "sum: 0 i: 0 while [lt i 100000] [sum: add sum i i: add i 1] print sum"
#let test = "x: func [a b] [print a print b] x 1 2"
#let test = "hey: add 300 500 print hey x: func [a] [print add a hey] z: x 200 x 10000"
#let test = "print either gt 5 2 [111] [222]"
#let test = "fibo: func [x] [either gt x 1 [add fibo sub x 1 fibo sub x 2] [1]] fibo 40"
#let test = "print 42 o: make-object [a: add 8 8 b: 42 c: make-object [x: 1 y: 2]] print o"
#let test = "z: does [print 42] z"
#let test = "compose [add 3 4 5 6 sub 7 1 5]"
#let test = "z: make-vnode 'h1 make-object [id: 42] [] print z"
#let test = "z: div [] [h1 [id: 55] [h1 [x: 77] [] h1 [] []]]"
#let test = "x: \"hey there!\" z: 5 print x"
# let test = """
# z: react [a: 18] title: signal 777 title: 888 print title
# """
# let test = """make-vnode 'h1 [] compose [make-vnode 'h1 make-object [id: 5] [1] make-vnode 'h1 make-object [id: 5] compose
# [2] make-vnode 'h1 make-object [id: 5] compose [3]]"""
# let test = """
#     header [] [
#       h1 [] ["todos"]
#       input [placeholder: "What needs to be done?" 
#            autofocus: true
#            on-enter: does [print "on enter!"]]
#     ]
# """
# let test = """

# f: func [x] [add x x]

# map f map f [1 2 3]

# mmap: func [f blk] [
#   res: []
#   foreach blk x [print f x]
#   res
# ]

# mmap :f [5 6 7]

# """
# let test = """

# iv: func [x] [
#   h1 [] [x]
# ]

# vw: func [title] [
#     header [] [
#       h1 [] ["title"]
#       iv title
#       input [placeholder: "What needs to be done?" 
#              autofocus: true
#              on-enter: does [print "on enter!"]]
#     ]
#   ]
#   render vw "Todos"
# """
# let test = """
# z: make-object [a: 42 b: make-object [m: 16 n: 11] x: 888] print z/x print z/b/m
# """
# let test = """
# f: func [a /b bv /c cv] [print a print b print c print bv print cv]
# f/c 1 2 3 4 5
# """
# let test = """
#   f-maker: func [x] [does [print x]]
#   f-ok: f-maker "OK"
#   f-ok
# """
# let test = """
# f: does [sum: 0 i: 0 while [lt i 100] [sum: add sum i i: add i 1] print sum]
# f
# """
# let test = "x: signal true"

# let b1 = vm.findOrAddSymbol("b")
# let b2 = vm.findOrAddSymbol("b")
# assert b1 == b2

import times

let t = cpuTime()

let code = vm.parse test
echo code
let x = vm.execAll code
echo x
showMemory(vm)

# import dom

# vm.show unbox[VNode](vmcast[Obj](x), tVNode)

echo "Time: ", (cpuTime() - t), " seconds"

