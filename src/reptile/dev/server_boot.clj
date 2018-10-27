(ns reptile.dev.server-boot
  (:use [reptile.dev.reload-server]))


;------------------- ************* -------------------
;
; Bootstrap when the namespace is loaded
;
;------------------- ************* -------------------

(boot-and-watch-fs! "src" 56665 "warm-blooded-lizards-rock")

