---
title: "#define true false"
date: 2011-03-16
category: code, humor
---

What is the output of the following C++ program?

```cpp
#include <iostream>

#define true false
#define false true

int main(){ std::cout << (
        false ? "false" :
        true  ? "true"  :
                "mu"
) << "\n";}
```