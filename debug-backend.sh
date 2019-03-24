#!/bin/bash

dhall <<"EOF"
< Result : {_1 : List {filename : Text, content : Text}}
| Error : {_1 : {}}
>.Result
    { _1 = [{ filename = "/dev/stdout", content = "something"}]
    }
EOF
