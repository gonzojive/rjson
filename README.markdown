# RJSON

### A JSON-like language that can encode circular references and more!


RJSON is a Javascript-evaluable data interchange format, like JSON, that supports features that make it capable of encoding sophisticated object graphs with ease.

## Background

JSON is a fantastic, simple data interchange format that has become the de facto encoding for AJAX, and the preferred format for http APIs in many circles.  Unfortunately it does not provide a means of encoding object graphs with circular reference without further processing of the transmitted RJSON.  We sought to include a few common features that often must be reinvented when performing complex tasks with JSON:

  - Referring to an object that occurs multiple times with a simple reference
  - Handling the tricky case of circular reference
  - Including type information for each object.

This library provides a Javascript (Parenscript) decoder and a Common Lisp encoder.

## Examples

### Encoding a simple list
To encode some object into rjson, use RJSON:ENCODE-RJSON or RJSON:ENCODE-RJSON-TO-STRING:

    > (rjson:encode-rjson (list 1 "two" "three") t)
    
    {"header":{"allocs":[],"inits":[]},"content":[1,"two","three"]}

The RJSON message here is purely a JSON object with some header information.  The result of decoding this message is the value of "content."

### Encoding a list with multiple references to the same object
Where RJSON shines is the ability to encode multiple references to the same object.  This happens transparently for objects where EQL is a valid comparison.

    > (let ((x (list 1 2 3 4)))                                                                                                                                                                                                             
        (rjson:encode-rjson (list x x) t))
    
    {"header":{"allocs":[rjalloc(1,"json:array")],
               "inits":[rjinit(1,[1,2,3,4])]},
     "content":[rjref(1),rjref(1)]}

This is illustrative of the abilities of RJSON to handle multiple copies of an object, and also for how RJSON handles types.

-  The *rjalloc* function call establishes an object of type "json:array" that can be referenced with the id 1
-  The *rjinit* function call initializes the value of the object at id 1 by passing the argument [1,2,3,4] to the registered initialization function for "json:array"
-  The *rjref* returns the value for the object with id 1

Notably, the order of evaluation of the header, allocs, inits, and content is significant, and the RJSON encoder makes sure to order them properly.

### Decoding an RJSON with Parenscript

RJSON is commonly transmitted from server to client, and for that purpose we provide a Parenscript and Javascript library for decoding an RJSON string to a Javascript object.

The RJSON decoder is mostly a definition for the functions rjinit, rjalloc, rjconstruct, and rjref, and a call to Javascript's eval function.  By relying on eval, RJSON's parser is exceptionally fast.

In any case, decoding an RJSON string in Parenscript is as simple as calling *rjson-decode*:

    (rjson:decode-rjson rjson-string)
    =>
    [[1,2,3,4], [1,2,3,4]]

