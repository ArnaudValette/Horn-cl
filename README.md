# Horn
An Org parser prototype
## Usage

Load the system with asdf:

```
 (asdf:load-asd "path/to/horn.asd")                                                                                            
 (in-package :horn-system)                                                                                                                               
 (asdf:load-system :horn)    
```

Parse:

```
(org-file-to-horns "/path/to/file.org")
(recurse-and-process-glitters-in-horns *roots*)
```

The result is a forest representation of the org file where each first level heading represents the root of a tree.
A tree consist in horn-nodes that convey useful informations about the parsed lines of org.

Each one of the horn-nodes can be parsed furthermore, so you can obtain a representation of their textual content
via glitter-nodes (a less structural type of node that represents bold, italic, verbatim  elements and etc...).
