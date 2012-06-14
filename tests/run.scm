(use jsmin test)

(test "" (jsmin-string ""))

(test "" (jsmin-string "                 "))

(test "" (jsmin-string "\t"))

(test "" (jsmin-string "\n"))

(test "" (jsmin-string "\r"))

(test "" (jsmin-string "\t\n\r"))

(test "" (jsmin-string "/* a comment */"))

(test "\n1" (jsmin-string "1"))

(test "\nfactorial=function(n){if(n<2){return 1;}\nelse{return n*factorial(n-1);}}"
      (jsmin-file "a-file.js"))
