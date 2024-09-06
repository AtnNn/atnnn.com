---
title: C++ left arrow operator
date: 2016-07-29
category: code, humor
---

Sometimes you have a pointer to a class, and you want to invoke a method. You can use the `->` operator for that.

So what do you do when you have a pointer to a method, and want to invoke it on a class? Use the `<-` operator!

```cpp
#include <iostream>

template<class T>
struct larrow {
    larrow(T* a_) : a(a_) { }
    T* a;
};

template <class T, class R>
R operator<(R (T::* f)(), larrow<T> it) {
    return (it.a->*f)();
}

template<class T>
larrow<T> operator-(T& a) {
    return larrow<T>(&a);
}

struct C {
    void f() { std::cout << "foo\n"; }
};

int main() {
    C x;
    (&C::f)<-x;
}
```

<br><hr><br>

Update: someone [on Reddit](https://www.reddit.com/r/cpp/comments/4v7xyn/comment/d5w7vnx/) complained about this being

> a terrible idea because it becomes easy to confuse it with the `<--` operator.

If you can get over the confusion (it should be easy: a lot of other
operators also differ by only one character), both operators can be
combined to write concise and efficient code:

```cpp
struct C {
  virtual void f(){ std::cout << "f\n"; }
  virtual void g(){ std::cout << "g\n"; }
  virtual void h(){ std::cout << "h\n"; }
} x;

int main(){
    void(C::*(*a))() = 2 + *(void(C::*(**))())&x;
    while(*(void**)&x <-- a){
        (*a)<-x;
    }
}
```
